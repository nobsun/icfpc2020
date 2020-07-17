module Message (
  Node (..),
  Token (..),
  Expr (..),
  toExpr,
  ) where

import Control.Applicative (empty)
import Control.Monad.Trans.State (StateT, runStateT, get, put)

-----

data Node
  = Num Int
  | Var Int
  | Eq
  | Lt
  | Succ
  | Pred
  | Sum
  | Mul
  | Div
  | T
  | F
  | Mod
  | Dem
  deriving (Eq, Show)

data Token
  = TArg Node
  | TAp
  deriving (Eq, Show)

data Expr
  = Arg Node
  | Ap Expr Expr
  deriving (Eq, Show)

-----

type Parser = StateT [Token] Maybe

runParser :: Parser a -> [Token] -> Maybe (a, [Token])
runParser = runStateT

token :: Parser Token
token = do
  tts <- get
  case tts of
    []    ->  empty
    t:ts  ->  put ts *> pure t

eof :: Parser ()
eof = do
  tts <- get
  case tts of
    []   ->  pure ()
    _:_  ->  empty

-----

expr :: Parser Expr
expr = do
  t <- token
  case t of
    TArg n -> pure (Arg n)
    TAp    -> Ap <$> expr <*> expr

toExpr :: [Token] -> Maybe Expr
toExpr = (fst <$>) . runParser (expr <* eof)

-----

_example0 :: Maybe (Expr, [Token])
_example0 =
  runParser expr [TAp, TAp, TArg Sum, TArg $ Num 1, TArg $ Num 2]
