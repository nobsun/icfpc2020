module ImageFile where

import Control.Arrow ((&&&), (***))
import Control.Monad (forM_)
import Data.List (find)
import qualified Data.Set as Set
import Data.Word
import qualified Codec.Picture as Picture
import System.IO (hPutStrLn, stderr)

type Image = [(Int, Int)]

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

  -- merge all images
  let f x y = maybe bgColor ((colors !!).(`mod` colorLen).fst)
              $ find (\(_, pixels) -> (x + xmin, y + ymin) `Set.member` Set.fromList pixels)
              $ zip [(0::Int)..] images
      img = Picture.generateImage f w h
  let fname = prefix ++ "-all" ++ ".png"
  hPutStrLn stderr $ "writing " ++ fname
  Picture.writePng fname img
  where
    -- TODO
    bgColor = Picture.PixelRGB8   0   0   0
    colors = [ Picture.PixelRGB8 216 164 108
             , Picture.PixelRGB8  96  72 120
             , Picture.PixelRGB8 216 152 120
             , Picture.PixelRGB8  68  76 156
             , Picture.PixelRGB8 220 140 140
             , Picture.PixelRGB8 120 104 196
             , Picture.PixelRGB8 184  72  40
             , Picture.PixelRGB8 168  88 168
             , Picture.PixelRGB8 240 188 168
             , Picture.PixelRGB8 148  76  56
             , Picture.PixelRGB8 188 143 143
             , Picture.PixelRGB8 204 168 192
             , Picture.PixelRGB8 120  84 124
             , Picture.PixelRGB8 196 124 180
             , Picture.PixelRGB8 228 136 152
             , Picture.PixelRGB8 196 184 160
             , Picture.PixelRGB8 116 124 116
             , Picture.PixelRGB8 196  80 160
             , Picture.PixelRGB8 136 120 148
             ]
    colorLen = length colors
