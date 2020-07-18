module Interact
  ( State (..)
  , stateToExpr

  , Image
  , asImage
  , asImages

  , interact
  , step

  , _test_interact
  , _test_interact_2
  , _test_step
  ) where

import Prelude hiding (interact)
import Control.Monad
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap.Lazy (IntMap)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Word
import qualified Codec.Picture as Picture

import Eval
import Message
import GalaxyTxt (getGalaxyExprs, galaxyKey)
import qualified Send


data State
  = SNum Int
  | SNil
  | SCons State State
  deriving (Eq, Show)

stateToExpr :: State -> Expr
stateToExpr SNil = Prim Nil
stateToExpr (SCons a b) = Ap (Ap (Prim Cons) (stateToExpr a)) (stateToExpr b)
stateToExpr (SNum n) = Prim (Num n)


type Image = [(Int, Int)]

asImage :: NFValue -> Image
asImage = map asPixel . asList

asPixel :: NFValue -> (Int, Int)
asPixel (NFPAp Cons [x, y]) = (asNum x, asNum y)
asPixel x = error $ "asPixel: " ++ show x

asImages :: NFValue -> [Image]
asImages = map asImage . asList


-- TODO: send の引数も　NFValue ではなく [Image] にしてしまうことは可能?
interact :: Monad m => (NFValue -> m (Int, Int)) -> IntMap Expr -> Expr -> State -> (Int, Int) -> m (State, [Image])
interact send env protocol state vector =
  case step env protocol state vector of
    (flag, newState, dat) ->
      if flag == 0 then
        return (newState, asImages dat)
      else do
        vector' <- send dat
        interact send env protocol newState vector'

step :: IntMap Expr -> Expr -> State -> (Int, Int) -> (Int, State, NFValue)
step env protocol state (x,y) =
  case asList (reduceNF' env (Ap (Ap protocol (stateToExpr state)) (Ap (Ap (Prim Cons) (Prim (Num x))) (Prim (Num y))))) of
    [flag_, newState_, dat] ->
      let flag = asNum flag_
          newState = asState newState_
       in (flag, newState, dat)
    xs -> error $ "step: " ++ show xs


asList :: NFValue -> [NFValue]
asList (NFPAp Nil []) = []
asList (NFPAp Cons [x, y]) = x : asList y
asList v = error $ "asList: " ++ show v

asNum :: NFValue -> Int
asNum (NFPAp (Num n) []) = n
asNum v = error $ "asNum: " ++ show v

asState :: NFValue -> State
asState (NFPAp Nil []) = SNil
asState (NFPAp Cons [x, y]) = SCons (asState x) (asState y)
asState (NFPAp (Num n) []) = SNum n
asState v = error $ "asState: " ++ show v


_test_step = do
  ps <- getGalaxyExprs  
  let env = IntMap.fromList ps
      galaxy = env IntMap.! galaxyKey
  let ret = step env galaxy SNil (0,0)
  print ret

_test_interact = do
  ps <- getGalaxyExprs  
  let env = IntMap.fromList ps
      galaxy = env IntMap.! galaxyKey
      send _ = return (0, 0)
  (_, images) <- interact send env galaxy SNil (0,0)
  forM_ (zip [(0::Int)..] images) $ \(i, pixels) -> do
    print pixels
    let xmin = minimum $ 0 : [x | (x,_y) <- pixels]
        xmax = maximum $ 0 : [x | (x,_y) <- pixels]
        ymin = minimum $ 0 : [y | (_x,y) <- pixels]
        ymax = maximum $ 0 : [y | (_x,y) <- pixels]
        w = xmax - xmin + 1
        h = ymax - ymin + 1
        pixels' = Set.fromList pixels
        f x y = if (x + xmin, y + ymin) `Set.member` pixels' then 255 else 0
        img :: Picture.Image Word8
        img = Picture.generateImage f w h 
    Picture.writePng ("output" ++ show i ++ ".png") img

_test_interact_2 = do
  ps <- getGalaxyExprs  
  let env = IntMap.fromList ps
      galaxy = env IntMap.! galaxyKey
      toExpr (NFPAp prim args) = foldl (\e arg -> Ap e (toExpr arg)) (Prim prim) args
      toNFValue (Ap a b) =
        case toNFValue a of
          NFPAp prim args -> NFPAp prim (args ++ [toNFValue b])
      toNFValue (Prim prim) = NFPAp prim []
      send val = liftM (asPixel . toNFValue) $ Send.send $ toExpr val
  (_, images) <- interact send env galaxy SNil (0,0)
  forM_ (zip [(0::Int)..] images) $ \(i, pixels) -> do
    print pixels
    let xmin = minimum $ 0 : [x | (x,_y) <- pixels]
        xmax = maximum $ 0 : [x | (x,_y) <- pixels]
        ymin = minimum $ 0 : [y | (_x,y) <- pixels]
        ymax = maximum $ 0 : [y | (_x,y) <- pixels]
        w = xmax - xmin + 1
        h = ymax - ymin + 1
        pixels' = Set.fromList pixels
        f x y = if (x + xmin, y + ymin) `Set.member` pixels' then 255 else 0
        img :: Picture.Image Word8
        img = Picture.generateImage f w h 
    Picture.writePng ("output" ++ show i ++ ".png") img
  
