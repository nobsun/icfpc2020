module Interact
  ( State (..)
  , stateToExpr

  , Image
  , asImage
  , asImages

  , interact
  , step

  , _test_step
  ) where

import Prelude hiding (interact)
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap.Lazy (IntMap)

import NFEval (NFValue (..), reduceNF')
import qualified NFEval as NF
import Message (Expr (Ap, Prim), Prim (Cons, Nil, Num))
import qualified ImageFile as IMG
import GalaxyTxt (getGalaxyExprs, galaxyKey)


type Image = IMG.Image
{-# DEPRECATED Image "use Image in ImageFile.hs instead of this." #-}

data State
  = SNum Int
  | SNil
  | SCons State State
  deriving (Eq, Show)

stateToExpr :: State -> Expr
stateToExpr SNil = Prim Nil
stateToExpr (SCons a b) = Ap (Ap (Prim Cons) (stateToExpr a)) (stateToExpr b)
stateToExpr (SNum n) = Prim (Num n)


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
asList = NF.asList

asNum :: NFValue -> Int
asNum = NF.asNum

asState :: NFValue -> State
asState (NFPAp Nil []) = SNil
asState (NFPAp Cons [x, y]) = SCons (asState x) (asState y)
asState (NFPAp (Num n) []) = SNum n
asState v = error $ "asState: " ++ show v

_test_step :: IO ()
_test_step = do
  ps <- getGalaxyExprs
  let env = IntMap.fromList ps
      galaxy = env IntMap.! galaxyKey
  let ret = step env galaxy SNil (0,0)
  print ret
