module GalaxyRun (
  runInteract,
  interact1,
  ) where

import Data.IntMap (fromList)
import qualified Data.IntMap as IM

import Message (Expr (Ap, Prim), Prim (Interact, Cons, Nil, Num))
import Send (send)
import Eval (Value, NFValue, reduce, normalize)
import GalaxyTxt (getGalaxyExprs, galaxyKey)

runInteract :: Expr -> Expr -> Expr -> IO Value
runInteract cont vx vy = do
  ps <- getGalaxyExprs
  let envm = fromList ps
  galaxy <- maybe (fail "galaxy expr not found!") return $ IM.lookup galaxyKey envm
  let applyInteract = Ap (Ap (Ap (Prim Interact) galaxy) cont) (Ap (Ap (Prim Cons) vx) vy)
  reduce send envm applyInteract
  -- r <- reduce send envm applyInteract
  -- normalize send envm r

interact1 :: IO Value
interact1 =
  runInteract
  (Prim Nil)
  (Prim (Num 0))
  (Prim (Num 0))
