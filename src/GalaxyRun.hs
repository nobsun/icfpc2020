module GalaxyRun (
  runGalaxy,
  ) where

import Data.IntMap (fromList)
import qualified Data.IntMap as IM

import Send (send)
import Eval (Value, reduce)
import GalaxyTxt (getGalaxyExprs, galaxyKey)

runGalaxy :: IO Value
runGalaxy = do
  ps <- getGalaxyExprs
  let envm = fromList ps
  gexpr <- maybe (fail "galaxy expr not found!") return $ IM.lookup galaxyKey envm
  reduce send envm gexpr
