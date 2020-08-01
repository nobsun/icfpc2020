module GalaxyPpr (run) where

import Data.Ord (comparing)
import Data.List (sortBy)

import HsLikePrinter (pprDefinition, Env, emptyEnv, singleExprEnv, countFree)
import Message (Expr)
import GalaxyTxt (getGalaxyExprs)


genHsLike :: FilePath -> Env -> [(Int, Expr)] -> IO ()
genHsLike fn env ps0 =
    writeFile fn $ unlines $ map (uncurry $ pprDefinition env) ps
  where
    ps = sortBy (comparing $ countFree env . snd) ps0

run :: IO ()
run = do
  ps <- getGalaxyExprs
  let env = singleExprEnv ps
  genHsLike "galaxyI.hs" env ps
  genHsLike "galaxy.hs" emptyEnv ps
