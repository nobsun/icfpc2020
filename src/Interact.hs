module Interact
  ( SValue (..)
  , svAsNum
  , svAsList
  , svAsPixel
  , svAsImage
  , svAsImages
  , svToExpr
  , svFromNFValue

  , State
  , Image

  , interact
  , step
  , stepOld
  ) where

import Prelude hiding (interact)
import Data.IntMap.Lazy (IntMap)

import NFEval (NFValue (..), reduceNF')
import Message (Expr (Ap, Prim), Prim (Cons, Nil, Num))
import qualified ImageFile as IMG


type Image = IMG.Image
{-# DEPRECATED Image "use Image in ImageFile.hs instead of this." #-}


data SValue
  = SNum !Int
  | SNil
  | SCons !State !State
  deriving (Eq, Show, Read)

svAsNum :: SValue -> Int
svAsNum (SNum n) = n
svAsNum x = error $ "svAsNum: " ++ show x

svAsList :: SValue -> [SValue]
svAsList SNil = []
svAsList (SCons x y) = x : svAsList y
svAsList v = error $ "svAsList: " ++ show v

svAsPixel :: SValue -> (Int, Int)
svAsPixel (SCons x y) = (svAsNum x, svAsNum y)
svAsPixel x = error $ "svAsPixel: " ++ show x

svAsImage :: SValue -> Image
svAsImage = map svAsPixel . svAsList

svAsImages :: SValue -> [Image]
svAsImages = map svAsImage . svAsList

svToExpr :: State -> Expr
svToExpr SNil = Prim Nil
svToExpr (SCons a b) = Ap (Ap (Prim Cons) (svToExpr a)) (svToExpr b)
svToExpr (SNum n) = Prim (Num n)

svFromNFValue :: NFValue -> State
svFromNFValue (NFPAp Nil []) = SNil
svFromNFValue (NFPAp Cons [x, y]) = SCons (svFromNFValue x) (svFromNFValue y)
svFromNFValue (NFPAp (Num n) []) = SNum n
svFromNFValue v = error $ "svFromNFValue: " ++ show v


type State = SValue

interact :: Monad m => (SValue -> m SValue) -> IntMap Expr -> Expr -> State -> (Int, Int) -> m (State, [Image])
interact send env protocol state (x,y) = interact' send env protocol state (SCons (SNum x) (SNum y))

interact' :: Monad m => (SValue -> m SValue) -> IntMap Expr -> Expr -> State -> SValue -> m (State, [Image])
interact' send env protocol state event =
  case step env protocol state event of
    (flag, newState, dat) ->
      if flag == 0 then
        return (newState, svAsImages dat)
      else do
        ret <- send dat
        interact' send env protocol newState ret

step :: IntMap Expr -> Expr -> State -> SValue -> (Int, State, SValue)
step env protocol state event =
  case svAsList $ svFromNFValue $ reduceNF' env (Ap (Ap protocol (svToExpr state)) (svToExpr event)) of
    [flag_, newState, dat] ->
      let flag = svAsNum flag_
       in seq flag $ (flag, newState, dat)
    xs -> error $ "step: " ++ show xs

{-# DEPRECATED stepOld "use new step function" #-}
stepOld :: IntMap Expr -> Expr -> State -> (Int, Int) -> (Int, State, SValue)
stepOld env protocol state (x,y) = step env protocol state (SCons (SNum x) (SNum y))
-- 最初の座標は cons pair で与えるが、 send の返り値はリストで帰ってきて、それをそのままprotocolに引き渡す必要があるので、
-- このインターフェースは問題があった。
