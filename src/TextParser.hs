{-# LANGUAGE OverloadedStrings #-}

module TextParser
  ( parseLines
  , parseLine
  , parseToken
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Attoparsec.ByteString.Lazy
  (Parser, parse, eitherResult)
import Data.Attoparsec.ByteString.Char8 hiding (parse, eitherResult)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as L8

import Message


-- | XXX
--
-- >>> parseToken (L8.pack "ap ap cons 2 ap ap cons 7 nil")
-- Right [TAp,TAp,TPrim Cons,TPrim (Num 2),TAp,TAp,TPrim Cons,TPrim (Num 7),TPrim Nil]
--
-- >>> parseToken (L8.pack "ap ap cons x0 x1")
-- Right [TAp,TAp,TPrim Cons,TPrim (Var 0),TPrim (Var 1)]
--
-- >>> parseToken (L8.pack "ap ap ap c add 1 2")
-- Right [TAp,TAp,TAp,TPrim C,TPrim Add,TPrim (Num 1),TPrim (Num 2)]
--
-- >>> parseLine (L8.pack ":1388 = ap ap :1162 :1386 0")
-- Right (1388,[TAp,TAp,TPrim (LineVar 1162),TPrim (LineVar 1386),TPrim (Num 0)])
--
-- >>> parseLine (L8.pack "galaxy = :1338")
-- Right (-1,[TPrim (LineVar 1338)])
--
parseToken :: L8.ByteString -> Either String [Token]
parseToken = eitherResult . parse (tokenP `sepBy` char ' ')

parseLine :: L8.ByteString -> Either String (Int,[Token])
parseLine = eitherResult . parse lineP

parseLines :: L8.ByteString -> Either String [(Int,[Token])]
parseLines = eitherResult . parse (lineP `sepBy` endOfLine)


lineP :: Parser (Int,[Token])
lineP = do
  n <- lineNoP
  string " = "
  ts <- tokenP `sepBy` char ' '
  return (n, ts)


lineNoP :: Parser Int
lineNoP = char ':' *> decimal
  <|> (string "galaxy" *> pure (-1))


tokenP :: Parser Token
tokenP = choice
  [ string "ap"  *> pure TAp
  , TPrim . Num <$> (signed decimal)
  , char 'x' >> TPrim . Var <$> decimal
  , char ':' >> TPrim . LineVar <$> decimal
  , string "inc" *> pure (TPrim Succ)
  , string "dec" *> pure (TPrim Pred)
  , string "add" *> pure (TPrim Add)
  , string "mul" *> pure (TPrim Mul)
  , string "div" *> pure (TPrim Div)
  , string "mod" *> pure (TPrim Mod)
  , string "dem" *> pure (TPrim Dem)
  , string "send"  *> pure (TPrim Send)
  , string "neg"   *> pure (TPrim Neg)
  , string "pwr"   *> pure (TPrim Pow2)
  , string "cons"  *> pure (TPrim Cons)
  , string "nil"   *> pure (TPrim Nil)
  , string "car"   *> pure (TPrim Car)
  , string "cdr"   *> pure (TPrim Cdr)
  , string "if0"   *> pure (TPrim If0)
  , string "draw"  *> pure (TPrim Draw)
  , string "checkerboard"   *> pure (TPrim Chkb)
  , string "multipledraw"   *> pure (TPrim MultiDraw)
  , string "s"     *> pure (TPrim S)
  , string "c"     *> pure (TPrim C)
  , string "b"     *> pure (TPrim B)
  , string "t"     *> pure (TPrim T)
  , string "f"     *> pure (TPrim F)
  , string "i"     *> pure (TPrim Message.I)
   ]

