{-# LANGUAGE OverloadedStrings #-}

module TextParser (
  parseToken
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
-- >>> parseToken (L8.pack "ap ap cons 2 ap ap cons 7 nil")
-- Right [TAp,TAp,TPrim Cons,TPrim (Num 2),TAp,TAp,TPrim Cons,TPrim (Num 7),TPrim Nil]
parseToken :: L8.ByteString -> Either String [Token]
parseToken = eitherResult . parse (tokenP `sepBy` char ' ')

tokenP :: Parser Token
tokenP = msum
  [ string "ap" *> pure TAp
  , TPrim . Num <$> decimal
  , string "cons" *> pure (TPrim Cons)
  , string "nil" *> pure (TPrim Nil)
  ]

