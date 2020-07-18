{-# LANGUAGE OverloadedStrings #-}

module TextParser
  ( parseLines
  , parseLine
  , parseToken
  ) where

import Control.Applicative
import Control.Monad (void)
-- import Data.List
import Data.Attoparsec.ByteString.Lazy
  (Parser, parse, eitherResult)
import Data.Attoparsec.ByteString.Char8
  (endOfLine, endOfInput, sepBy, char, string, decimal, signed)
-- import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as L8

import Message (Prim (..))
import qualified Message as M
import ListDesugar (Token (..), desugar)

-- | XXX
--
-- >>> parseToken (L8.pack "ap ap cons 2 ap ap cons 7 nil")
-- Right [TAp,TAp,TPrim Cons,TPrim (Num 2),TAp,TAp,TPrim Cons,TPrim (Num 7),TPrim Nil]
--
-- >>> parseToken (L8.pack "ap ap cons :1029 :1030")
-- Right [TAp,TAp,TPrim Cons,TPrim (Var 1029),TPrim (Var 1030)]
--
-- >>> parseToken (L8.pack "ap ap ap c add 1 2")
-- Right [TAp,TAp,TAp,TPrim C,TPrim Add,TPrim (Num 1),TPrim (Num 2)]
--
-- >>> parseLine (L8.pack ":1388 = ap ap :1162 :1386 0")
-- Right (1388,[TAp,TAp,TPrim (Var 1162),TPrim (Var 1386),TPrim (Num 0)])
--
-- >>> parseLine (L8.pack "galaxy = :1338")
-- Right (-1,[TPrim (Var 1338)])
--
parseToken :: L8.ByteString -> Either String [M.Token]
parseToken = (listDesugar =<<) . eitherResult . parse (tokenP `sepBy` char ' ')

parseLine :: L8.ByteString -> Either String (Int,[M.Token])
parseLine in_ = do
  (n, ts) <- eitherResult $ parse (lineP <* endOfInput) in_
  (,) n <$> listDesugar ts

parseLines :: L8.ByteString -> Either String [(Int,[M.Token])]
parseLines in_ = do
  ps <- eitherResult $ parse (lineP `sepBy` endOfLine) in_
  let desugar_ (n, ts) = (,) n <$> listDesugar ts
  mapM desugar_ ps

listDesugar :: [Token] -> Either String [M.Token]
listDesugar = maybe (Left "list desugar error") return . desugar

lineP :: Parser (Int,[Token])
lineP = do
  n <- lineNoP
  void $ string " = "
  ts <- tokenP `sepBy` char ' '
  return (n, ts)


lineNoP :: Parser Int
lineNoP =
  char ':' *> decimal          <|>
  string "galaxy" *> pure (-1)

tokenP :: Parser Token
tokenP =
  TokenM <$>
  (string "ap"  *> pure M.TAp  <|>
   M.TPrim <$>
   ( Num <$> (signed decimal)      <|>
     (char ':' >> Var <$> decimal) <|>
     string "eq"  *> pure Eq       <|>
     string "lt"  *> pure Lt       <|>
     string "inc" *> pure Succ     <|>
     string "dec" *> pure Pred     <|>
     string "add" *> pure Add      <|>
     string "mul" *> pure Mul      <|>
     string "div" *> pure Div      <|>
     string "mod" *> pure Mod      <|>
     string "dem" *> pure Dem      <|>
     string "send"  *> pure Send   <|>
     string "neg"   *> pure Neg    <|>
     string "pwr2"  *> pure Pow2   <|>
     string "cons"  *> pure Cons   <|>
     string "nil"   *> pure Nil    <|>
     string "isnil" *> pure IsNil  <|>
     string "car"   *> pure Car    <|>
     string "cdr"   *> pure Cdr    <|>
     string "if0"   *> pure If0    <|>
     string "draw"  *> pure Draw   <|>
     string "checkerboard"   *> pure Chkb       <|>
     string "multipledraw"   *> pure MultiDraw  <|>
     string "s"     *> pure S      <|>
     string "c"     *> pure C      <|>
     string "b"     *> pure B      <|>
     string "t"     *> pure T      <|>
     string "f"     *> pure F      <|>
     string "i"     *> pure I )) <|>
  string "(" *> pure ParenL      <|>
  string ")" *> pure ParenR      <|>
  string "," *> pure Comma
