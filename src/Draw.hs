module Draw where

import Data.Array

import Message


type Point = (Int,Int)
type Pic   = Array Point Char

-- | XXX
--
-- >>> draw (makePic [(1,1)])
-- ["  "," x"]
--
-- >>> draw (makePic [(1,1),(1,2)])
-- ["  "," x"," x"]
--
draw :: Pic -> [String]
draw pic =
    [[ pic!(x,y) | x<-[hx..tx]] | y<-[hy..ty]]
  where
     ((hx,hy),(tx,ty)) = bounds pic

makePic :: [Point] -> Pic
makePic ps =
    array ((0,0),(w,h)) $ [((x,y),' ')| x<-[0..w],y<-[0..h]] ++ [(p,'x') |p<-ps]
  where
    w = maximum (map fst ps)
    h = maximum (map snd ps)

-- | XXX
-- >>> makePicFromExpr (Ap (Ap (Prim Cons) (Ap (Ap (Prim Cons) (Prim (Num 1))) (Prim (Num 2)))) (Prim Nil))
-- array ((0,0),(1,2)) [((0,0),' '),((0,1),' '),((0,2),' '),((1,0),' '),((1,1),' '),((1,2),'x')]
--
makePicFromExpr :: Expr -> Pic
makePicFromExpr = makePic . exprToPoints

exprToPoints :: Expr -> [Point]
exprToPoints (Prim Nil) = []
exprToPoints (Ap (Ap (Prim Cons) (Prim (Num n1))) (Prim (Num n2))) = [(n1,n2)]
exprToPoints (Ap (Ap (Prim Cons) e1) e2) = exprToPoints e1 ++ exprToPoints e2
exprToPoints e = error ("invalid list" ++ show e)

