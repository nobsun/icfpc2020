module GalaxyRun (
  runGalaxy,
  ) where

import Data.IntMap (fromList)
import qualified Data.IntMap as IM

import Eval (Value, reduce)
import GalaxyTxt (getGalaxyExprs)

runGalaxy :: IO Value
runGalaxy = do
  ps <- getGalaxyExprs
  let envm = fromList ps
  gexpr <- maybe (fail "galaxy expr not found!") return $ IM.lookup (-1) envm
  reduce pure envm gexpr
