module SValue where

import Message
import ImageFile (Image)
import NFEval

data SValue
  = SNum !Int
  | SNil
  | SCons !SValue !SValue
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

svToExpr :: SValue -> Expr
svToExpr SNil = Prim Nil
svToExpr (SCons a b) = Ap (Ap (Prim Cons) (svToExpr a)) (svToExpr b)
svToExpr (SNum n) = Prim (Num n)

svFromNFValue :: NFValue -> SValue
svFromNFValue (NFPAp Nil []) = SNil
svFromNFValue (NFPAp Cons [x, y]) = SCons (svFromNFValue x) (svFromNFValue y)
svFromNFValue (NFPAp (Num n) []) = SNum n
svFromNFValue v = error $ "svFromNFValue: " ++ show v
