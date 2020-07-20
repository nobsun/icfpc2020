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

interact :: Monad m => (SValue -> m (Int, Int)) -> IntMap Expr -> Expr -> State -> (Int, Int) -> m (State, [Image])
interact send env protocol state vector =
  case step env protocol state vector of
    (flag, newState, dat) ->
      if flag == 0 then
        return (newState, svAsImages dat)
      else do
        vector' <- send dat
        interact send env protocol newState vector'

step :: IntMap Expr -> Expr -> State -> (Int, Int) -> (Int, State, SValue)
step env protocol state (x,y) =
  case svAsList $ svFromNFValue $ reduceNF' env (Ap (Ap protocol (svToExpr state)) (Ap (Ap (Prim Cons) (Prim (Num x))) (Prim (Num y)))) of
    [flag_, newState, dat] ->
      let flag = svAsNum flag_
       in seq flag $ (flag, newState, dat)
    xs -> error $ "step: " ++ show xs
