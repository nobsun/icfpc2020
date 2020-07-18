module ListDesugar (
  Token (..),
  desugar,
  ) where

import Control.Applicative (empty, (<|>), some, many)
import MonadicParser (runParser, token, satisfy, eof)
import qualified MonadicParser as MP

import Message (Prim (Cons, Nil, Var))
import qualified Message as M


-- extend syntax level token
data Token
  = ParenL
  | ParenR
  | Comma
  | TokenM M.Token
  deriving (Eq, Show)


-- プログラムの項は一つの token のまたはリスト表現
data Term
  = TkTerm M.Token
  | List List
  deriving (Eq, Show)

-- リスト表現の要素は項の並び
type ListElem = [Term]

-- リスト表現は要素の並び
type List = [ListElem]

-- プログラムは項の並び
type Program = [Term]

programTokens :: Program -> [M.Token]
programTokens =
    terms
  where
    list x = concatMap ((cons ++) . listElem) x ++ nil
    listElem = terms
    terms = concatMap term
    term (TkTerm tk) = [tk]
    term (List x)    = list x

    cons = [M.TAp, M.TAp, M.TPrim Cons]
    nil = [M.TPrim Nil]


type Parser = MP.Parser Token Maybe

just :: Token -> Parser Token
just t = satisfy (== t)


mtokenP :: Parser M.Token
mtokenP = do
  t <- token
  case t of
    TokenM tk -> pure tk
    ParenL    -> empty
    ParenR    -> empty
    Comma     -> empty

termP :: Parser Term
termP =
  TkTerm <$> mtokenP  <|>
  List   <$> listP

termsP :: Parser [Term]
termsP = some termP

listElemP :: Parser ListElem
listElemP = termsP

listP :: Parser List
listP =
  just ParenL *> ((:) <$> listElemP <*> many (just Comma *> listElemP) <|> pure []) <* just ParenR

programP :: Parser Program
programP = termsP

parseProgram :: [Token] -> Maybe Program
parseProgram = fmap fst . runParser (programP <* eof)

-- | desugar example
--
-- >>> desugar [ParenL, ParenR]
-- Just [TPrim Nil]
--
-- >>> desugar [ParenL, TokenM (M.TPrim (Var 100)), ParenR]
-- Just [TAp,TAp,TPrim Cons,TPrim (Var 100),TPrim Nil]
--
-- >>> desugar [ParenL, TokenM (M.TPrim (Var 100)), Comma, TokenM (M.TPrim (Var 101)), Comma, TokenM (M.TPrim (Var 102)), ParenR]
-- Just [TAp,TAp,TPrim Cons,TPrim (Var 100),TAp,TAp,TPrim Cons,TPrim (Var 101),TAp,TAp,TPrim Cons,TPrim (Var 102),TPrim Nil]
--
-- >>> desugar [ParenL, TokenM (M.TPrim (Var 100)), Comma, ParenL, TokenM (M.TPrim (Var 101)), Comma, TokenM (M.TPrim (Var 102)), ParenR, Comma, TokenM (M.TPrim (Var 103)), ParenR]
-- Just [TAp,TAp,TPrim Cons,TPrim (Var 100),TAp,TAp,TPrim Cons,TAp,TAp,TPrim Cons,TPrim (Var 101),TAp,TAp,TPrim Cons,TPrim (Var 102),TPrim Nil,TAp,TAp,TPrim Cons,TPrim (Var 103),TPrim Nil]
--
desugar :: [Token] -> Maybe [M.Token]
desugar = fmap programTokens . parseProgram

_ex0 :: Maybe [M.Token]
_ex0 = desugar [ParenL, ParenR]

_ex1 :: Maybe [M.Token]
_ex1 = desugar [ParenL, TokenM (M.TPrim (Var 100)), ParenR]

_ex2 :: Maybe [M.Token]
_ex2 = desugar [ParenL, TokenM (M.TPrim (Var 100)), Comma, TokenM (M.TPrim (Var 101)), ParenR]

_ex3 :: Maybe [M.Token]
_ex3 = desugar [ParenL, TokenM (M.TPrim (Var 100)), Comma, TokenM (M.TPrim (Var 101)), Comma, TokenM (M.TPrim (Var 102)), ParenR]

_ex4 :: Maybe [M.Token]
_ex4 = desugar [ParenL, TokenM (M.TPrim (Var 100)), Comma, ParenL, TokenM (M.TPrim (Var 101)), Comma, TokenM (M.TPrim (Var 102)), ParenR, Comma, TokenM (M.TPrim (Var 103)), ParenR]
-- ap ap cons :100 ap ap cons ap ap cons :101 ap ap cons :102 nil ap ap cons :103 nil
