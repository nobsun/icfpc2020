{-# LANGUAGE OverloadedStrings #-}

module TextParser
  ( parseLines
  , parseLine
  , parseTokens
  , parseMessage
  , galaxyKey
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

galaxyKey :: Int
galaxyKey = -1

-- | XXX
--
-- >>> parseTokens (L8.pack "ap ap cons 2 ap ap cons 7 nil")
-- Right [TAp,TAp,TPrim Cons,TPrim (Num 2),TAp,TAp,TPrim Cons,TPrim (Num 7),TPrim Nil]
--
-- >>> parseTokens (L8.pack "ap ap cons :1029 :1030")
-- Right [TAp,TAp,TPrim Cons,TPrim (Var 1029),TPrim (Var 1030)]
--
-- >>> parseTokens (L8.pack "ap ap ap c add 1 2")
-- Right [TAp,TAp,TAp,TPrim C,TPrim Add,TPrim (Num 1),TPrim (Num 2)]
--
-- >>> parseLine (L8.pack ":1388 = ap ap :1162 :1386 0")
-- Right (1388,[TAp,TAp,TPrim (Var 1162),TPrim (Var 1386),TPrim (Num 0)])
--
-- >>> parseLine (L8.pack "galaxy = :1338")
-- Right (-1,[TPrim (Var 1338)])
--
parseTokens :: L8.ByteString -> Either String [M.Token]
parseTokens = (listDesugar =<<) . eitherResult . parse (tokenP `sepBy` char ' ')

parseLine :: L8.ByteString -> Either String (Int,[M.Token])
parseLine in_ = do
  (n, ts) <- eitherResult $ parse (defineP <* endOfInput) in_
  (,) n <$> listDesugar ts

parseLines :: L8.ByteString -> Either String [(Int,[M.Token])]
parseLines in_ = do
  ps <- eitherResult $ parse (defineP `sepBy` endOfLine) in_
  let desugar_ (n, ts) = (,) n <$> listDesugar ts
  mapM desugar_ ps

listDesugar :: [Token] -> Either String [M.Token]
listDesugar = maybe (Left "list desugar error") return . desugar

defineP :: Parser (Int,[Token])
defineP = do
  n <- varNoP
  void $ string " = "
  ts <- tokenP `sepBy` char ' '
  return (n, ts)


varNoP :: Parser Int
varNoP =
  char ':' *> decimal          <|>
  string "galaxy" *> pure galaxyKey

tokenP :: Parser Token
tokenP =
  TokenM <$>
  (string "ap"  *> pure M.TAp  <|>
   M.TPrim <$>
   ( Num <$> (signed decimal)      <|>
     (char ':' >> Var <$> decimal) <|>
     (char 'x' >> LVar <$> decimal) <|>
     string "checkerboard"   *> pure Chkb       <|>
     string "multipledraw"   *> pure MultiDraw  <|>
     string "modem" *> pure Modem  <|>
     string "f38"   *> pure F38    <|>
     string "interact"       *> pure Interact   <|>
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
     string "s"     *> pure S      <|>
     string "c"     *> pure C      <|>
     string "b"     *> pure B      <|>
     string "t"     *> pure T      <|>
     string "f"     *> pure F      <|>
     string "i"     *> pure I )) <|>
  string "(" *> pure ParenL      <|>
  string ")" *> pure ParenR      <|>
  string "," *> pure Comma


parseMessage :: L8.ByteString -> Either String M.Expr
parseMessage s = do
  ts <- parseTokens s
  maybe (Left $ "message: invalid token sequence: " ++ show ts) Right $ M.toExpr ts


_modem :: Either String M.Expr
_modem = parseMessage "ap dem ap mod x0"

_f38 :: Either String M.Expr
_f38 = parseMessage "ap ap ap if0 ap car x0 ( ap modem ap car ap cdr x0 , ap multipledraw ap car ap cdr ap cdr x0 ) ap ap ap interact x2 ap modem ap car ap cdr x0 ap send ap car ap cdr ap cdr x0"

_interact :: Either String M.Expr
_interact = parseMessage "ap ap f38 x2 ap ap x2 x4 x3"
