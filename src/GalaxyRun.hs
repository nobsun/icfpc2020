module GalaxyRun (
  getGalaxyProtocol,
  runProtocol,
  protocol1,
  ) where

import Data.IntMap (IntMap, fromList)
import qualified Data.IntMap as IM

import Message (Expr (Ap, Prim), Prim (Cons, Nil, Num))
-- import Send (sendNF)
import Eval (NFValue (..), reduceNF')
import GalaxyTxt (getGalaxyExprs, galaxyKey)


getGalaxyProtocol :: IO (IntMap Expr, Expr)
getGalaxyProtocol = do
  ps <- getGalaxyExprs
  let env = fromList ps
  galaxy <- maybe (fail "galaxy expr not found!") return $ IM.lookup galaxyKey env
  return (env, galaxy)

runProtocol :: Expr -> (Expr, Expr) -> IO NFValue
runProtocol state (vx, vy) = do
  (env, galaxy) <- getGalaxyProtocol
  let applyProtocol = Ap (Ap galaxy state) (Ap (Ap (Prim Cons) vx) vy)
  return $ reduceNF' env applyProtocol

protocol1 :: IO ()
protocol1 = do
  flag : newSt : dat : xs  <-
    asList <$>
    runProtocol
    (Prim Nil)
    (Prim (Num 0), Prim (Num 0))
  putStrLn $ "flag: " ++ show (asNum flag)
  putStrLn $ "newSt: " ++ show newSt
  putStrLn $ "dat: " ++ show dat
  putStrLn $ "other: " ++ show xs

asNum :: NFValue -> Int
asNum (NFPAp (Num n) []) = n
asNum v = error $ "asNum: " ++ show v

asList :: NFValue -> [NFValue]
asList (NFPAp Nil []) = []
asList (NFPAp Cons [x, y]) = x : asList y
asList v = error $ "asList: " ++ show v
