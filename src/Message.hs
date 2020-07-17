module Message (
  Prim (..),
  Token (..),
  Expr (..),
  toExpr,
  ) where

import Control.Applicative (empty)
import Control.Monad.Trans.State (StateT, runStateT, get, put)

import qualified Data.Tree as T

-----

data Prim
  = Num Int
  | Var Int
  | Eq | Lt          -- 2 args
  | Succ | Pred      -- 1 arg
  | Add | Mul | Div  -- 2 arg
  | Mod | Dem        -- 1 arg
  | Send             -- 1 arg
  | Neg              -- 1 arg
  | S | C | B        -- 3 arg
  | T {- True, K combinator -} | F {- False, flip K -}  -- 2 arg
  | Pow2             -- 1 arg
  | I                -- 1 arg
  | Cons             -- 3 arg
  | Car | Cdr        -- 1 arg
  | Nil | IsNil      -- 1 arg
  | If0              -- 3 arg
  | Draw             -- 1 arg
  | Chkb             -- 2 arg
  | MultiDraw        -- 1 arg
  deriving (Eq, Show)

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
    TPrim n -> pure (Prim n)
    TAp    -> Ap <$> expr <*> expr

toExpr :: [Token] -> Maybe Expr
toExpr = (fst <$>) . runParser (expr <* eof)

-- data Value = Val Prim | Fun (Value -> Value)

-- FORMULA --> WHNF

{-
eval :: Expr -> Expr
eval e@(Prim _)  = e
eval (Ap {}) = undefined
  where
    eval1 e0@(Ap f e) = case eval f of
      w@(Ap {}) -> Ap w e1
      Prim n    -> case n of
        Succ     -> case e1 of
          Prim (Num i) -> Prim $ Num $ succ i
          _           -> Ap (Prim Succ) e1
        Pred     -> case e1 of
          Prim (Num i) -> Prim $ Num $ pred i
          _           -> Ap (Prim Pred) e1
        {- Mod -}
        {- Dem -}
        Neg      -> case e1 of
          Prim (Num i) -> Prim $ Num $ negate i
          _           -> Ap (Prim Neg) e1
        Pow2     -> case e1 of
          Prim (Num i) -> Prim $ Num $ 2^i
          _            -> Ap (Prim Pow2) e1
        I        -> eval e
        Car      -> eval $ Ap e $ Prim T
        Cdr      -> eval $ Ap e $ Prim F
        {- Nil -} {- 引数が IsNil 以外のときは? -}
        IsNil    -> case e1 of
          Prim Nil     -> Prim T
          _            -> Prim F
        {- Draw -}
        {- MultiDraw -}
        _        -> e0
      where
        e1 = eval e
 -}

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
