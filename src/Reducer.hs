module Reducer where

import qualified Data.IntMap as M
import Data.Maybe
import Message

type Env = M.IntMap Expr

reduce :: Env -> Expr -> Maybe Expr
reduce env e = case e of
  Prim (Var x) -> maybe Nothing (reduce env) (M.lookup x env)
  Prim _       -> Just e
  Ap e1 e2     -> case reduce env e1 of
    Nothing       -> Nothing
    Just r1       -> case r1 of
      Prim p        -> case arity p of
        0             -> case p of
          Var x         -> maybe Nothing (reduce env) (M.lookup x env)
          _             -> error "cannot apply"
        1             -> ap1 p (reduce env e2)
        _             -> Just (Ap r1 e2)
      Ap e1' e2' -> case e1' of
        Prim p'    -> case arity p' of
          0          -> case p' of
            Var x      -> maybe (error "unbound variable") (reduce env) (M.lookup x env)
            _          -> error "cannot apply"
          1          -> case ap1 p' (reduce env e2') of
            Just r1'   -> reduce env (Ap r1' e2)
            _          -> Nothing
          2          -> ap2 p' (reduce env e2') (reduce env e2)
          _          -> Just (Ap r1 e2)
        Ap e1'' e2''
                   -> case e1'' of
          Prim p''   -> case arity p'' of
            0          -> case p'' of
              Var x      -> maybe (error "unbound variable") (reduce env) (M.lookup x env)
              _          -> error "cannot apply"
            1          -> case ap1 p'' (reduce env e2'') of
              Just r1'   -> Ap <$> (Ap r1' <$> reduce env e2') <*> reduce env e2
              _          -> Nothing
            2          -> case ap2 p'' (reduce env e2'') (reduce env e2') of
              Just r2'   -> Ap r2' <$> reduce env e2
              _          -> Nothing
            3          -> ap3 p'' (reduce env e2'') (reduce env e2') (reduce env e2)
          _          -> Just (Ap (Ap (Ap e1'' e2'') e2') e2)

p1s, p2s, p3s :: [Prim]
p1s = [Succ, Pred, Mod, Dem, Send, Neg, Pow2, I, Car, Cdr, Nil, IsNil, Draw]
p2s = [Eq, Lt, Add, Mul, Div, T, F, Chkb]
p3s = [S, C, B, Cons, If0]


ap3 :: Prim -> Maybe Expr -> Maybe Expr -> Maybe Expr -> Maybe Expr
ap3 p e1 e2 e3 = undefined

ap2 :: Prim -> Maybe Expr -> Maybe Expr -> Maybe Expr
ap2 p e1 e2 = case p of
  Eq  -> case e1 of
    Just (Prim (Num x)) -> case e2 of
      Just (Prim (Num y)) -> if x == y then Just (Prim T) else Just (Prim F)
      _                   -> Nothing
    _                     -> Nothing
  Lt  -> case e1 of
    Just (Prim (Num x)) -> case e2 of
      Just (Prim (Num y)) -> if x < y then Just (Prim T) else Just (Prim F)
      _                   -> Nothing
    _                   -> Nothing
  Add -> case e1 of
    Just (Prim (Num x)) -> case e2 of
      Just (Prim (Num y)) -> Just (Prim (Num (x + y)))
      _                   -> Nothing
    _                   -> Nothing
  Mul -> case e1 of
    Just (Prim (Num x)) -> case e2 of
      Just (Prim (Num y)) -> Just (Prim (Num (x * y)))
      _                   -> Nothing
    _                   -> Nothing
  Div -> case e1 of
    Just (Prim (Num x)) -> case e2 of
      Just (Prim (Num y)) -> Just (Prim (Num (x `quot` y)))
      _                   -> Nothing
    _                   -> Nothing
  T   -> e1
  F   -> e2
  Chkb -> case e1 of
    Just e1' -> case e2 of
      Just e2' -> Just (Ap (Ap (Prim p) e1') e2')
      _        -> Nothing
    _        -> Nothing
ap2 _ _ _ = Nothing
  

ap1 :: Prim -> Maybe Expr -> Maybe Expr
ap1 p (Just e) = case p of
  Succ -> case e of
    Prim (Num x) -> Just (Prim (Num (succ x)))
    _            -> Nothing
  Pred -> case e of
    Prim (Num x) -> Just (Prim (Num (pred x)))
    _            -> Nothing
  Mod  -> Just (Ap (Prim Mod) e)
  Dem  -> Just (Ap (Prim Dem) e)
  Send -> Just (Ap (Prim Send) e)
  Neg  -> case e of
    Prim (Num x) -> Just (Prim (Num (negate x)))
    _            -> Nothing
  Pow2 -> case e of
    Prim (Num x) -> Just (Prim (Num (2^x)))
    _            -> Nothing
  I    -> Just e
  Car  -> case e of
    Ap (Ap (Prim Cons) h) _
         -> Just h
    _    -> Nothing
  Cdr  -> case e of
    Ap (Ap (Prim Cons) _) t
         -> Just t
    _    -> Nothing
  Nil  -> Just (Prim T)
  IsNil -> case e of
    Prim Nil     -> Just (Prim T)
    Ap (Ap (Prim Cons) _) _
          -> Just (Prim F)
    _     -> Nothing
  Draw -> Just (Ap (Prim Draw) e)
  MultiDraw -> Just (Ap (Prim MultiDraw) e)
ap1 _ _ = Nothing
