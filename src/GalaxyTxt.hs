module GalaxyTxt (
  getGalaxyTxt,
  getGalaxyTokens,
  getGalaxyExprs,
  galaxyKey,
  ) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO.Unsafe (unsafeInterleaveIO)

import Message (Token, Expr, toExpr)
import TextParser (parseLine, galaxyKey)

-- | Get content of galaxy.txt with lazy-IO
getGalaxyTxt :: IO LB.ByteString
getGalaxyTxt = LB.readFile "data/galaxy.txt"

-- | Get tokens of galaxy.txt with lazy-IO
getGalaxyTokens :: IO [(Int, [Token])]
getGalaxyTokens = do
  in_ <- getGalaxyTxt
  let loop  []    = return []
      loop (x:xs) = unsafeInterleaveIO $
        (:)
        <$> either fail return (parseLine x)
        <*> loop xs

  loop $ L8.lines in_

getGalaxyExprs :: IO [(Int, Expr)]
getGalaxyExprs = do
  ps <- getGalaxyTokens
  let toExpr_ (n, ts) =
        maybe (fail $ "parse error: " ++ show n ++ ": " ++ show (take 30 ts) ++ " ...") (return . (,) n) $ toExpr ts
  mapM toExpr_ ps
