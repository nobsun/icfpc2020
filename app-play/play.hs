module Main where

import Prelude hiding (interact)
import qualified Data.IntMap.Lazy as IntMap
import Data.IORef
import Options.Applicative
import System.IO
import Text.Read

import GalaxyTxt (getGalaxyExprs, galaxyKey)
import ImageFile (saveImages)
import Interact
import Message
import NFEval
import Send


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

  let send :: SValue -> IO SValue
      send val = do
        -- FIXME: use optServerURL and optApiKey
        e <- sendExpr $ svToExpr val
        let px = svFromNFValue $ NFEval.reduceNF' IntMap.empty e --XXX
        -- hPutStrLn stderr $ "send( " ++ show val ++ ") => " ++ show px
        return px

  stepRef <- newIORef (1::Int)
  let loop s = do
        n <- readIORef stepRef
        writeIORef stepRef $! n+1
        -- hPutStrLn stderr $ "state = " ++ show s
        pt <- readPixel
        (s', images) <- Interact.interact send env galaxy s pt
        saveImages ("step" ++ show n) images
        loop s'
  loop SNil


asPixel :: NFValue -> (Int, Int)
asPixel (NFPAp Cons [x, NFPAp Cons [y], NFPAp Nil []]) = (asNum x, asNum y)
asPixel x = error $ "asPixel: " ++ show x
