module Message (
  Prim (..),
  arity,
  Token (..),
  Expr (..),
  toExpr,
  fromList,
  toList, toList',
  ) where

import qualified Data.Tree as T

import MonadicParser (runParser, token, eof)
import qualified MonadicParser as MP

-----

data Prim
  = Num Int
  | Var Int
  | MBits String  -- modulated bits - string of '0' '1'
  | LVar Int      -- local variable for function definition
  | Eq | Lt          -- 2 args
  | Succ | Pred      -- 1 arg
  | Add | Mul | Div  -- 2 arg
  | Mod | Dem        -- 1 arg
  | Send             -- 1 arg
  | Neg              -- 1 arg
  | S | C | B        -- 3 arg
  | T {- True, K combinator -} | F {- False, flip K -}  -- 2 arg
  | Pow2             -- 1 arg (NOTE: 0-arityで再帰的に定義されたコンビネータとして扱うべき?)
  | I                -- 1 arg
  | Cons             -- 3 arg
  | Car | Cdr        -- 1 arg
  | Nil | IsNil      -- 1 arg
  | If0              -- 3 arg
  | Draw             -- 1 arg
  | Chkb             -- 0 arg
  | MultiDraw        -- 1 arg

  | Modem            -- 1 arg
  | F38              -- 2 arg
  | Interact         -- 3 arg
  deriving (Eq, Ord, Show)

arity :: Prim -> Int
arity (Num _) = 0
arity (Var _) = 0
arity (MBits _) = 0
arity (LVar _) = 0
arity Eq = 2
arity Lt = 2
arity Succ = 1
arity Pred = 1
arity Add = 2
arity Mul = 2
arity Div = 2
arity Mod = 1
arity Dem = 1
arity Send = 1
arity Neg = 1
arity S = 3
arity C = 3
arity B = 3
arity T = 2
arity F = 2
arity Pow2 = 1
arity I = 1
arity Cons = 3
arity Car = 1
arity Cdr = 1
arity Nil = 1
arity IsNil = 1
arity If0 = 1
arity Draw = 1
arity Chkb = 0
arity MultiDraw = 1
arity Modem = 1
arity F38 = 2
arity Interact = 3

data Token
  = TPrim Prim
  | TAp
  deriving (Eq, Show)

data Expr
  = Prim Prim
  | Ap Expr Expr
  deriving (Eq, Show)

cataExpr :: (Prim -> t) -> (t -> t -> t) -> Expr -> t
cataExpr f g = u
  where u (Prim a) = f a
        u (Ap l r) = g (u l) (u r)

-----

type Parser = MP.Parser Token Maybe

expr :: Parser Expr
expr = do
  t <- token
  case t of
    TPrim n -> pure (Prim n)
    TAp    -> Ap <$> expr <*> expr

toExpr :: [Token] -> Maybe Expr
toExpr = (fst <$>) . runParser (expr <* eof)

-----

-- | fromList
--
-- >>> fromList $ map Prim [Num 1, Num 2, Num 3]
-- Ap (Ap (Prim Cons) (Prim (Num 1))) (Ap (Ap (Prim Cons) (Prim (Num 2))) (Ap (Ap (Prim Cons) (Prim (Num 3))) (Prim Nil)))
---
fromList :: [Expr] -> Expr
fromList = foldr cons nil
  where
    nil = Prim Nil
    cons a d = Ap (Ap (Prim Cons) a) d

toList :: Expr -> Maybe [Expr]
toList (Prim Nil)                   = Just []
toList (Ap (Ap (Prim Cons) e1) e2)  = (e1 :) <$> toList e2
toList  _                           = Nothing

toList' :: Expr -> [Expr]
toList' e = maybe (error $ "toList': unable to convert to list: " ++ show e) id $ toList e


-----

toDataTree :: Expr -> T.Tree String
toDataTree = cataExpr f g
  where f a = T.Node (show a) []
        g l r = T.Node "App" [l, r]

draw :: Expr -> IO ()
draw = putStr . T.drawTree . toDataTree

-----

_example0 :: Maybe (Expr, [Token])
_example0 =
  runParser expr [TAp, TAp, TPrim Add, TPrim $ Num 1, TPrim $ Num 2]

_drawExample41 :: IO ()
_drawExample41 = let Just e = toExpr [ TAp, TAp, TPrim B, TAp, TPrim B, TAp, TAp, TPrim S, TAp
                                     , TAp, TPrim B, TAp, TPrim B, TAp, TPrim Cons, TPrim (Num 0)
                                     , TAp, TAp, TPrim C, TAp, TAp, TPrim B, TPrim B, TPrim Cons
                                     , TAp, TAp, TPrim C, TPrim Cons, TPrim Nil, TAp,  TAp, TPrim C
                                     , TPrim Cons,  TPrim Nil, TAp, TPrim C, TPrim Cons]
                 in draw e
