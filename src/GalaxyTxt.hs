module GalaxyTxt (
  getGalaxyTxt,
  getGalaxyTokens,
  ) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8

import Message (Token)
import TextParser (parseLine)

-- | Get content of galaxy.txt with lazy-IO
getGalaxyTxt :: IO LB.ByteString
getGalaxyTxt = LB.readFile "data/galaxy.txt"

-- | Get tokens of galaxy.txt with lazy-IO
getGalaxyTokens :: IO [(Int, [Token])]
getGalaxyTokens = do
  in_ <- getGalaxyTxt
  mapM (either fail return . parseLine) $ L8.lines in_
