module GalaxyRun (
  getGalaxyProtocol,
  interacts,
  rangedInteracts,

  runProtocol,
  protocol1,
  ) where

import Prelude hiding (interact)
import Data.IntMap (IntMap, fromList)
import qualified Data.IntMap as IM
import System.IO.Unsafe (unsafeInterleaveIO)

import Message (Expr (Ap, Prim), Prim (Cons, Nil, Num))
-- import Send (sendNF)
import NFEval (NFValue (..), asNum, asList, reduceNF')
import GalaxyTxt (getGalaxyExprs, galaxyKey)
import Interact (State (SNil), Image, asImages, step, )


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

interacts :: (NFValue -> IO (Int, Int)) -> IntMap Expr -> Expr -> State -> (Int, Int) -> IO [(State, [Image])]
interacts send env protocol istate ivector = do
  let loop state vector = unsafeInterleaveIO $ do
        case step env protocol state vector of
          (flag, newState, dat) ->
            if flag == 0 then
              return [(newState, asImages dat)]
            else do
              newVector <- send dat
              (:) (newState, asImages dat) <$> loop newState newVector
  loop istate ivector

rangedInteracts :: (NFValue -> IO (Int, Int)) -> IntMap Expr -> Expr
                -> ((Int, Int), (Int, Int))
                -> IO [((Int, Int), [(State, [Image])])]
rangedInteracts send env protocol ((minx, miny), (maxx, maxy)) =
  sequence [ do let v = (x, y)
                (,) v <$> interacts send env protocol SNil v
           | x <- [minx .. maxx]
           , y <- [miny .. maxy]
           ]

-- galaxyInteracts = do
--   () <- getGalaxyProtocol
