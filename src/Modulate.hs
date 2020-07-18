{-# LANGUAGE OverloadedStrings #-}

module Modulate 
  ( modulate
  , demodulate
  ) where

import Prelude hiding (take, takeWhile)

import Control.Applicative
import qualified Data.List as L
import Data.Attoparsec.ByteString.Lazy
  (parse, eitherResult)
import Data.Attoparsec.ByteString.Char8 as C8 hiding (parse, eitherResult)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L8 hiding (take, takeWhile)


import Numeric (showIntAtBase, readInt)

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
--
-- >>> modulate (Prim (Num 1))
-- "01100001"
--
-- >>> modulate (Prim (Num (-1)))
-- "10100001"
--
-- >>> modulate (Ap (Ap (Prim Cons) (Prim (Num 1))) (Ap (Ap (Prim Cons) (Prim (Num 2))) (Prim Nil)))
-- "1101100001110110001000"
modulate :: Expr -> String
modulate (Prim (Num n))     = modulateNum n
modulate (Prim Nil)         = "00"
modulate (Ap(Prim  Cons) e) = "11" ++ modulate e
modulate (Ap e1 e2)         = modulate e1 ++ modulate e2
modulate e                  = error "unsupported Expr found!: " ++ show e


modulateNum :: Int -> String
modulateNum n =
    sn ++ ['1'|_<-[1..len]] ++ ('0': (if n>0 then num else ""))
  where
    sn = if signum n >= 0 then "01" else "10"
    len = length $ L.takeWhile (>0) $ iterate (`div`16) n
    num = reverse $ L.take (4*len) $ (reverse $ showIntAtBase 2 toChar (abs n) "") ++ (repeat '0')

toChar :: Int -> Char
toChar 0 = '0'
toChar _ = '1'

toInt :: Char -> Int
toInt '0' = 0
toInt _   = 1


-- XXX
-- >>> demodulate (L8.pack "00")
-- Right (Prim Nil)
--
-- >>> demodulate (L8.pack "010")
-- Right (Prim (Num 0))
--
-- >>> demodulate (L8.pack "10100001")
-- Right (Prim (Num (-1)))
--
-- >>> demodulate (L8.pack "110000")
-- Right (Ap (Ap (Prim Cons) (Prim Nil)) (Prim Nil))
--
-- >>> demodulate (L8.pack "1101100001110110001000")
-- Right (Ap (Ap (Prim Cons) (Prim (Num 1))) (Ap (Ap (Prim Cons) (Prim (Num 2))) (Prim Nil)))
--
demodulate :: L8.ByteString -> Either String Expr
demodulate = eitherResult . parse demodP


demodP :: Parser Expr
demodP = choice
  [ string "00" *> pure (Prim Nil)
  , string "11" *> (Ap <$> Ap (Prim Cons) <$> demodP <*> demodP)
  , demodNumP
  ]

demodNumP :: Parser Expr
demodNumP = do
  sig <- (string "01" *> pure 1) <|> (string "10" *> pure (-1))
  len <- (4*) . B.length <$> takeWhile (=='1')
  _ <-char '0'
  if len == 0
    then pure (Prim (Num 0))
    else Prim . Num <$> (*sig) . fst . head . readInt 2 (`elem`("01"::String)) toInt . B.unpack <$> take len


