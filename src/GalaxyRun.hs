module GalaxyRun (
  getGalaxyProtocol,

  interacts, rangedInteracts,

  manualInteracts, galaxyManualInteracts, galaxyManualInteracts_,

  runProtocol,
  protocol1,
  ) where

import Prelude hiding (interact)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.IntMap (IntMap, fromList)
import qualified Data.IntMap as IM
import System.IO.Unsafe (unsafeInterleaveIO)

import Message (Expr (Ap, Prim), Prim (Cons, Nil, Num))
import Send (sendSValue)
import SValue (SValue (SCons, SNum))
import NFEval (NFValue (..), asNum, asList, reduceNF')
import GalaxyTxt (getGalaxyExprs, galaxyKey)
import ImageFile (Image)
import Interact (SValue (SNil), State, svAsImages, step)


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

interacts :: (SValue -> IO SValue)
          -> IntMap Expr -> Expr
          -> State -> (Int, Int) -> IO [(State, [Image])]
interacts send env protocol istate (ix, iy) = do
  let loop state sval = unsafeInterleaveIO $ do
        case step env protocol state sval of
          (flag, newState, dat)
            | flag == 0  ->
                return [(newState, svAsImages dat)]
            | otherwise  -> do
                newSVal <- send dat
                (:) (newState, svAsImages dat) <$> loop newState newSVal
  loop istate $ SCons (SNum ix) (SNum iy)

manualInteracts :: IntMap Expr -> Expr
                -> State -> [(Int, Int)] -> [((State, [Image]), Bool)]
manualInteracts env protocol istate ivectors =
    loop istate ivectors
  where
    loop _      []    = []
    loop state ((vx, vy):vs) =
        ((newState, svAsImages dat), flag /= 0) : loop newState vs
      where
        (flag, newState, dat) = step env protocol state $ SCons (SNum vx) (SNum vy)

rangedInteracts :: (SValue -> IO SValue)
                -> IntMap Expr -> Expr
                -> ((Int, Int), (Int, Int))
                -> IO [((Int, Int), [(State, [Image])])]
rangedInteracts send env protocol ((minx, miny), (maxx, maxy)) =
  sequence [ do let v = (x, y)
                (,) v <$> interacts send env protocol SNil v
           | x <- [minx .. maxx]
           , y <- [miny .. maxy]
           ]

galaxyInteracts :: ((Int, Int), (Int, Int))
                -> IO [((Int, Int), [(State, [Image])])]
galaxyInteracts range = do
  (env, proto) <- getGalaxyProtocol
  rangedInteracts sendSValue env proto range

_run :: IO ()
_run = do
  gs <- galaxyInteracts ((-1000,-1000), (1000, 1000))
  print $ maximumBy (comparing snd)  [ (length rs, v) | (v, rs) <- gs ]

galaxyManualInteracts :: [(Int, Int)] -> IO [((State, [Image]), Bool)]
galaxyManualInteracts vs = do
  (env, proto) <- getGalaxyProtocol
  return $ manualInteracts env proto SNil vs

galaxyManualInteracts_ :: IO [((State, [Image]), Bool)]
galaxyManualInteracts_ = galaxyManualInteracts $ repeat (0, 0)
