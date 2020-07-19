module Main where

import Prelude hiding (interact)
import Control.Arrow ((&&&), (***))
import Control.Monad
import qualified Data.IntMap.Lazy as IntMap
import Data.IORef
import qualified Data.Set as Set
import Data.Word
import qualified Codec.Picture as Picture
import System.IO
import Text.Read

import GalaxyTxt (getGalaxyExprs, galaxyKey)
import Interact


saveImages :: String -> [Image] -> IO ()
saveImages prefix images = do
  let ps = (0, 0):[p | pixels <- images, p <- pixels]
      min'max = minimum &&& maximum
      ((xmin, xmax), (ymin, ymax)) = min'max *** min'max $ unzip ps
      (w, h) = (xmax - xmin + 1, ymax - ymin + 1)
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


main :: IO ()
main = do
  ps <- getGalaxyExprs
  let env = IntMap.fromList ps
      galaxy = env IntMap.! galaxyKey

  stepRef <- newIORef (0::Int)
  let send val = do
        n <- readIORef stepRef
        writeIORef stepRef $! n + 1
        saveImages ("step" ++ show n) (asImages val)
        readPixel

  hPutStr stderr "Enter pixel> "
  hFlush stderr
  pt <- readPixel

  (st, images) <- Interact.interact send env galaxy SNil pt
  print st
  saveImages "final" images
