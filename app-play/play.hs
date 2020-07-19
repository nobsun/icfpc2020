module Main where

import Prelude hiding (interact)
import Control.Arrow ((&&&), (***))
import Control.Monad
import qualified Data.IntMap.Lazy as IntMap
import Data.IORef
import qualified Data.Set as Set
import Data.Word
import qualified Codec.Picture as Picture
import Options.Applicative
import System.IO
import Text.Read

import GalaxyTxt (getGalaxyExprs, galaxyKey)
import Interact
import Message
import NFEval
import Send


saveImages :: String -> [Image] -> IO ()
saveImages prefix images = do
  let ps = (0, 0):[p | pixels <- images, p <- pixels]
      min'max = minimum &&& maximum
      ((xmin, xmax), (ymin, ymax)) = min'max *** min'max $ unzip ps
      w = xmax - xmin + 1
      h = ymax - ymin + 1
  print ((xmin,ymin), (xmax,ymax))
  forM_ (zip [(0::Int)..] images) $ \(i, pixels) -> do
    let pixels' = Set.fromList pixels
        f x y = if (x + xmin, y + ymin) `Set.member` pixels' then 255 else 0
        img :: Picture.Image Word8
        img = Picture.generateImage f w h
    let fname = prefix ++ "-ch" ++ show i ++ ".png"
    hPutStrLn stderr $ "writing " ++ fname
    Picture.writePng fname img


readPixel :: IO (Int, Int)
readPixel = do
  hPutStr stderr "Enter pixel> "
  hFlush stderr
  s <- getLine
  case readEither s of
    Right px -> return px
    Left err -> do
      hPutStrLn stderr err
      readPixel


data Options
  = Options
  { optServerURL :: String
  , optApiKey :: String
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser = Options <$> serverURL <*> apiKey
  where
    serverURL :: Parser String
    serverURL = strArgument $ metavar "ServerURL" <> value ""

    apiKey :: Parser String
    apiKey = strArgument $ metavar "APIKey" <> value ""

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) $ fullDesc


main :: IO ()
main = do
  opt <- execParser parserInfo
  print opt

  ps <- getGalaxyExprs
  let env = IntMap.fromList ps
      galaxy = env IntMap.! galaxyKey

  stepRef <- newIORef (0::Int)
  let send :: NFValue -> IO (Int, Int)
      send val = do
        n <- readIORef stepRef
        writeIORef stepRef $! n + 1
        saveImages ("step" ++ show n) (asImages val)
        if optServerURL opt == "" then
          readPixel
        else do
          -- FIXME: use optServerURL and optApiKey
          e <- sendNF val
          return $! asPixel $ reduceNF' IntMap.empty e -- XXX

  pt <- readPixel
  (st, images) <- Interact.interact send env galaxy SNil pt
  print st
  saveImages "final" images


asPixel :: NFValue -> (Int, Int)
asPixel (NFPAp Cons [x, y]) = (asNum x, asNum y)
asPixel x = error $ "asPixel: " ++ show x
