module Modulate 
  ( modulate
  ) where

import Numeric (showIntAtBase)

import Message


-- XXX
-- >>> modulate (Prim (Num 0))
-- "010"
--
-- >>> modulate (Prim Nil)
-- "00"
--
-- >>> modulate (Ap (Ap (Prim Cons) (Prim Nil)) (Prim Nil))
-- "110000"
--x
-- >>> modulate (Prim (Num 1))
-- "01100001"
--
-- >>> modulate (Ap (Ap (Prim Cons) (Prim (Num 1))) (Ap (Ap (Prim Cons) (Prim (Num 2))) (Prim Nil)))
-- "1101100001110110001000"
modulate :: Expr -> String
modulate (Prim (Num n)) = modulateNum n
modulate (Prim Nil)     = "00"
modulate (Ap(Prim  Cons) e)    = "11" ++ modulate e
modulate (Ap e1 e2)     = modulate e1 ++ modulate e2


modulateNum :: Int -> String
modulateNum n = 
    sn ++ ['1'|_<-[1..len]] ++ ('0': (if n>0 then num else ""))
  where
    sn = if signum n >= 0 then "01" else "10" 
    len = length $ takeWhile (>0) $ iterate (`div`16) n
    num = reverse $ take (4*len) $ (reverse $ showIntAtBase 2 char (abs n) "") ++ (repeat '0')

char 0 = '0'
char 1 = '1'

