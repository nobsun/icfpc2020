{-# LANGUAGE ScopedTypeVariables #-}
module Eval where

import Control.Monad hiding (ap)
import Data.List (foldl')
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

import Message
import NFEval (pwr2Def, chkbDef)
import qualified NFEval as NF
import Modulate (modulate_, demodulate)

-- 変数の参照を解決する
resolve :: IntMap Expr -> Expr -> Expr
resolve env = f
  where
    f (Ap a b) = Ap (f a) (f b)
    f (Prim (Var n)) = env' IntMap.! n
    f prim@(Prim _) = prim
    env' = IntMap.map f env

resolveLocal :: IntMap Expr -> Expr -> Expr
resolveLocal lenv = f
  where
    f (Ap a b) = Ap (f a) (f b)
    f (Prim (LVar n)) = env' IntMap.! n
    f prim@(Prim _) = prim
    env' = IntMap.map f lenv

data Value
  = PAp Prim [Expr]
  | Picture (Set (Int,Int))
  deriving (Eq, Show)


-- | Numbers
-- >>> reduce pure IntMap.empty (Prim (Num 42))
-- PAp (Num 42) []
--
-- >>> reduce pure IntMap.empty (Prim (Num (-3)))
-- PAp (Num (-3)) []
--
-- >>> reduce pure IntMap.empty (Prim (Num 0))
-- PAp (Num 0) []
--
-- | Calculate Number
-- >>> reduce pure IntMap.empty (Ap (Prim Pred) (Ap (Ap (Prim Add) (Prim (Num 1))) (Prim (Num 2))))
-- PAp (Num 2) []
--
-- | Lazy Evaluation
-- >>> :{
-- let env = IntMap.fromList [(2048, Ap (Prim F) (Prim (Var 2048)))]
--     exp = Ap (Ap (Prim F) (Prim (Var 2048))) (Prim (Num 42))
-- in reduce pure env exp
-- :}
-- PAp (Num 42) []
--
-- | Succ
-- >>> reduce pure IntMap.empty (Ap (Prim Succ) (Prim (Num 0)))
-- PAp (Num 1) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Succ) (Prim (Num 1)))
-- PAp (Num 2) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Succ) (Prim (Num 2)))
-- PAp (Num 3) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Succ) (Prim (Num 300)))
-- PAp (Num 301) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Succ) (Prim (Num 1023)))
-- PAp (Num 1024) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Succ) (Prim (Num (-2))))
-- PAp (Num (-1)) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Succ) (Prim (Num (-1))))
-- PAp (Num 0) []
--
-- | Pred
-- >>> reduce pure IntMap.empty (Ap (Prim Pred) (Prim (Num 0)))
-- PAp (Num (-1)) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pred) (Prim (Num 1)))
-- PAp (Num 0) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pred) (Prim (Num 2)))
-- PAp (Num 1) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pred) (Prim (Num 300)))
-- PAp (Num 299) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pred) (Prim (Num 1024)))
-- PAp (Num 1023) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pred) (Prim (Num (-2))))
-- PAp (Num (-3)) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pred) (Prim (Num (-1))))
-- PAp (Num (-2)) []
--
-- | Sum
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Prim (Num 1))) (Prim (Num 2)))
-- PAp (Num 3) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Prim (Num 2))) (Prim (Num 1)))
-- PAp (Num 3) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Prim (Num 0))) (Prim (Num 1)))
-- PAp (Num 1) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Prim (Num 2))) (Prim (Num 3)))
-- PAp (Num 5) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Prim (Num 3))) (Prim (Num 5)))
-- PAp (Num 8) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Prim (Num (-2)))) (Prim (Num (-1))))
-- PAp (Num (-3)) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Prim (Num 0))) (Prim (Num (-1))))
-- PAp (Num (-1)) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Prim (Num (-2)))) (Prim (Num (-3))))
-- PAp (Num (-5)) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Prim (Num (-5)))) (Prim (Num 3)))
-- PAp (Num (-2)) []
--
-- | Variables
-- >>> :{
-- let env = IntMap.fromList [(2048, Prim (Num 42))]
-- in reduce pure env (Ap (Ap (Prim Add) (Prim (Num 0))) (Prim (Var 2048)))
-- :}
-- PAp (Num 42) []
--
-- >>> :{
-- let env = IntMap.fromList [(2048, Prim (Num 42))]
-- in reduce pure env (Ap (Ap (Prim Add) (Prim (Var 2048))) (Prim (Num 0)))
-- :}
-- PAp (Num 42) []
--
-- | Product
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Mul) (Prim (Num 4))) (Prim (Num 2)))
-- PAp (Num 8) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Mul) (Prim (Num 3))) (Prim (Num 4)))
-- PAp (Num 12) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Mul) (Prim (Num 3))) (Prim (Num (-2))))
-- PAp (Num (-6)) []
--
-- >>> :{
-- let env = IntMap.fromList [(2048, Prim (Num 42))]
-- in reduce pure env (Ap (Ap (Prim Mul) (Prim (Var 2048))) (Prim (Num 0)))
-- :}
-- PAp (Num 0) []
--
-- >>> :{
-- let env = IntMap.fromList [(2048, Prim (Num 42))]
-- in reduce pure env (Ap (Ap (Prim Mul) (Prim (Var 2048))) (Prim (Num 1)))
-- :}
-- PAp (Num 42) []
--
-- >>> :{
-- let env = IntMap.fromList [(1030, Prim (Num 3)), (1031, Prim (Num 5))]
-- in reduce pure env (Ap (Ap (Prim Mul) (Prim (Var 1030))) (Prim (Var 1031)))
-- :}
-- PAp (Num 15) []
--
-- | Integer Division
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Div) (Prim (Num 4))) (Prim (Num 2)))
-- PAp (Num 2) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Div) (Prim (Num 4))) (Prim (Num 3)))
-- PAp (Num 1) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Div) (Prim (Num 4))) (Prim (Num 4)))
-- PAp (Num 1) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Div) (Prim (Num 4))) (Prim (Num 5)))
-- PAp (Num 0) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Div) (Prim (Num 5))) (Prim (Num 2)))
-- PAp (Num 2) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Div) (Prim (Num 6))) (Prim (Num (-2))))
-- PAp (Num (-3)) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Div) (Prim (Num 5))) (Prim (Num (-3))))
-- PAp (Num (-1)) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Div) (Prim (Num (-5)))) (Prim (Num 3)))
-- PAp (Num (-1)) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Div) (Prim (Num (-5)))) (Prim (Num (-3))))
-- PAp (Num 1) []
--
-- >>> :{
-- let env = IntMap.fromList [(1030, Prim (Num 42))]
-- in reduce pure env (Ap (Ap (Prim Mul) (Prim (Var 1030))) (Prim (Num 1)))
-- :}
-- PAp (Num 42) []
--
-- | Equality and Boolean
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Eq) (Prim (Num 0))) (Prim (Num 0)))
-- PAp T []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Eq) (Prim (Num 0))) (Prim (Num 1)))
-- PAp F []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Eq) (Prim (Num 0))) (Prim (Num (-1))))
-- PAp F []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Eq) (Prim (Num (-1)))) (Prim (Num (-1))))
-- PAp T []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Eq) (Prim (Num (-1)))) (Prim (Num (-2))))
-- PAp F []
--
-- | Strict Less-Than
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Lt) (Prim (Num 0))) (Prim (Num 0)))
-- PAp F []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Lt) (Prim (Num 0))) (Prim (Num 1)))
-- PAp T []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Lt) (Prim (Num (-1)))) (Prim (Num 0)))
-- PAp T []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Lt) (Prim (Num (-1)))) (Prim (Num (-2))))
-- PAp F []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Lt) (Prim (Num (-2)))) (Prim (Num (-1))))
-- PAp T []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Lt) (Prim (Num (-1)))) (Prim (Num 0)))
-- PAp T []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Lt) (Prim (Num (-1)))) (Prim (Num (-3))))
-- PAp F []
--
-- | Modulate
-- TODO
--
-- | Demodulate
-- TODO
--
-- | Send
-- TODO
--
-- | Negate
-- >>> reduce pure IntMap.empty (Ap (Prim Neg) (Prim (Num 0)))
-- PAp (Num 0) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Neg) (Prim (Num 1)))
-- PAp (Num (-1)) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Neg) (Prim (Num (-1))))
-- PAp (Num 1) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Neg) (Prim (Num 2)))
-- PAp (Num (-2)) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Neg) (Prim (Num (-2))))
-- PAp (Num 2) []
--
-- | Function Application
-- >>> reduce pure IntMap.empty (Ap (Prim Succ) (Ap (Prim Succ) (Prim (Num 0))))
-- PAp (Num 2) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Succ) (Ap (Prim Succ) (Ap (Prim Succ) (Prim (Num 0)))))
-- PAp (Num 3) []
--
-- >>> :{
-- let env = IntMap.fromList [(1030, Prim (Num 42))]
-- in reduce pure env (Ap (Prim Succ) (Ap (Prim Pred) (Prim (Var 1030))))
-- :}
-- PAp (Num 42) []
--
-- >>> :{
-- let env = IntMap.fromList [(1030, Prim (Num 42))]
-- in reduce pure env (Ap (Prim Pred) (Ap (Ap (Prim Add) (Prim (Var 1030))) (Prim (Num 1))))
-- :}
-- PAp (Num 42) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Ap (Ap (Prim Add) (Prim (Num 2))) (Prim (Num 3)))) (Prim (Num 4)))
-- PAp (Num 9) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Prim (Num 2))) (Ap (Ap (Prim Add) (Prim (Num 3))) (Prim (Num 4))))
-- PAp (Num 9) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Add) (Ap (Ap (Prim Mul) (Prim (Num 2))) (Prim (Num 3)))) (Prim (Num 4)))
-- PAp (Num 10) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim Mul) (Prim (Num 2))) (Ap (Ap (Prim Add) (Prim (Num 3))) (Prim (Num 4))))
-- PAp (Num 14) []
--
-- | S combinator
-- >>> reduce pure IntMap.empty (Ap (Ap (Ap (Prim S) (Prim Add)) (Prim Succ)) (Prim (Num 1)))
-- PAp (Num 3) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Ap (Prim S) (Prim Mul)) (Ap (Prim Add) (Prim (Num 1)))) (Prim (Num 6)))
-- PAp (Num 42) []
--
-- >>> :{
-- let env = IntMap.fromList [(1031, (Prim Add)), (1032, (Prim Succ)), (1033, (Prim (Num 4)))]
-- in reduce pure env (Ap (Ap (Ap (Prim S) (Prim (Var 1031))) (Prim (Var 1032))) (Prim (Var 1033)))
-- :}
-- PAp (Num 9) []
--
-- | C combinator
-- >>> reduce pure IntMap.empty (Ap (Ap (Ap (Prim C) (Prim Add)) (Prim (Num 2))) (Prim (Num 1)))
-- PAp (Num 3) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Ap (Prim C) (Prim Div)) (Prim (Num 2))) (Prim (Num 6)))
-- PAp (Num 3) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Ap (Prim C) (Prim T)) (Prim T)) (Prim F))
-- PAp F []
--
-- | B combinator
-- >>> reduce pure IntMap.empty (Ap (Ap (Ap (Prim B) (Prim Succ)) (Prim Pred)) (Prim (Num 3)))
-- PAp (Num 3) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Ap (Prim B) (Prim Succ)) (Ap (Prim Mul) (Prim (Num 3)))) (Prim (Num 3)))
-- PAp (Num 10) []
--
-- | K combinator
-- >>> reduce pure IntMap.empty  (Ap (Ap (Prim T) (Prim (Num 1))) (Prim (Num 5)))
-- PAp (Num 1) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim T) (Prim T)) (Prim I))
-- PAp T []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim T) (Prim T)) (Ap (Prim Succ) (Prim (Num 5))))
--PAp T []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim T) (Ap (Prim Succ) (Prim (Num 5)))) (Prim T))
-- PAp (Num 6) []
--
-- | Kite (a.k.a. False)
-- >>> reduce pure IntMap.empty  (Ap (Ap (Prim F) (Prim (Num 1))) (Prim (Num 5)))
-- PAp (Num 5) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim F) (Prim T)) (Prim I))
-- PAp I []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim F) (Prim T)) (Ap (Prim Succ) (Prim (Num 5))))
--PAp (Num 6) []
--
-- >>> reduce pure IntMap.empty (Ap (Ap (Prim F) (Ap (Prim Succ) (Prim (Num 5)))) (Prim T))
-- PAp T []
--
-- | Power of Two
-- >>> reduce pure IntMap.empty (Ap (Prim Pow2) (Prim (Num 2)))
-- PAp (Num 4) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pow2) (Prim (Num 3)))
-- PAp (Num 8) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pow2) (Prim (Num 4)))
-- PAp (Num 16) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pow2) (Prim (Num 5)))
-- PAp (Num 32) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pow2) (Prim (Num 6)))
-- PAp (Num 64) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pow2) (Prim (Num 7)))
-- PAp (Num 128) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pow2) (Prim (Num 8)))
-- PAp (Num 256) []
--
-- | I combinator
-- >>> reduce pure IntMap.empty (Ap (Prim I) (Prim (Num 1)))
-- PAp (Num 1) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim I) (Prim I))
-- PAp I []
--
-- >>> reduce pure IntMap.empty (Ap (Prim I) (Ap (Prim Add) (Prim (Num 1))))
-- PAp Add [Prim (Num 1)]
--
-- | Cons, Car, Cdr
-- >>> reduce pure IntMap.empty (Ap (Prim Car) (Ap (Ap (Prim Cons) (Prim (Num 1))) (Prim (Num 2))))
-- PAp (Num 1) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Cdr) (Ap (Ap (Prim Cons) (Prim (Num 1))) (Prim (Num 2))))
-- PAp (Num 2) []
--
-- | Nil
-- >>> reduce pure IntMap.empty  (Ap (Prim Nil) (Prim (LVar 0)))
-- PAp T []
--
-- >>> reduce pure IntMap.empty  (Ap (Prim Nil) (Prim F))
-- PAp T []
--
-- >>> :{
-- let env = IntMap.fromList [(2048, Ap (Prim F) (Prim (Var 2048)))]
-- in reduce pure env  (Ap (Prim Nil) (Prim (Var 2048)))
-- :}
-- PAp T []
--
-- | Is Nil
-- >>> reduce pure IntMap.empty (Ap (Prim IsNil) (Prim Nil))
-- PAp T []
--
-- >>> reduce pure IntMap.empty (Ap (Prim IsNil) (Ap (Ap (Prim Cons) (Prim (Num 1))) (Prim (Num 2))))
-- PAp F []
--
reduce :: forall m. (Monad m, MonadFail m) => (Expr -> m Expr) -> IntMap Expr -> Expr -> m Value
reduce send env = f
  where
    f :: Expr -> m Value
    f (Prim (Var n)) =
      case IntMap.lookup n env of
        Just e -> f e
        Nothing -> fail $ "cannot find (Var " ++ show n ++ ") in environment"
    f (Prim prim) =
      if arity prim == 0 then
        redPrim prim []
      else
        return $ PAp prim []
    f (Ap fun arg) = do
      fun' <- f fun
      case fun' of
        PAp prim xs -> do
          if arity prim < length xs then
            fail "should not happen"
          else if arity prim == length xs + 1 then
            redPrim prim (xs ++ [arg])
          else
            return $ PAp prim (xs ++ [arg])
        _ -> fail $ show fun' ++ " is not a function"

    asNum :: Expr -> m Int
    asNum e = do
      e' <- f e
      case e' of
        PAp (Num n) [] -> return n
        _ -> fail $ "asNum: " ++ show e'

    asCons :: Expr -> m (Expr, Expr)
    asCons e = do
      e' <- f e
      case e' of
        PAp Cons [x1, x2] -> return (x1, x2)
        _ -> fail $ "asCons: " ++ show e'

    asConsOrNil :: Expr -> m (Maybe (Expr, Expr))
    asConsOrNil e = do
      e' <- f e
      case e' of
        PAp Cons [x1, x2] -> return $ Just (x1, x2)
        PAp Nil [] -> return $ Nothing
        _ -> fail $ "asConsOrNil: " ++ show e'

    asList :: Expr -> m [Expr]
    asList xs = do
      c <- asConsOrNil xs
      case c of
        Nothing -> return []
        Just (x, ys) -> do
          ys' <- asList ys
          return $ x : ys'

    asMBits :: Expr -> m String
    asMBits e = do
      e' <- f e
      case e' of
        PAp (MBits s) [] -> return s
        _ -> fail $ "asMBits: " ++ show e'

    asExpr :: Expr -> m Expr
    asExpr e = do
      e' <- f e
      case e' of
        PAp p@(Num _) []  -> return $ Prim p
        PAp Cons [x1, x2] -> return $ Ap (Ap (Prim Cons) x1) x2
        PAp Nil []        -> return $ Prim Nil
        _ -> fail $ "asExpr: " ++ show e'

    redPrim prim@(Num _) _ = return $ PAp prim []
    redPrim prim@(Var _) _ = return $ PAp prim []
    redPrim Eq [x1, x2] = do
      x1' <- asNum x1
      x2' <- asNum x2
      return $! if x1' == x2' then PAp T [] else PAp F []
    redPrim Lt [x1, x2] = do
      x1' <- asNum x1
      x2' <- asNum x2
      return $! if x1' < x2' then PAp T [] else PAp F []
    redPrim Succ [x1] = do
      x1' <- asNum x1
      return $ PAp (Num (x1' + 1)) []
    redPrim Pred [x1] = do
      x1' <- asNum x1
      return $ PAp (Num (x1' - 1)) []
    redPrim Add [x1, x2] = do
      x1' <- asNum x1
      x2' <- asNum x2
      return $ PAp (Num (x1' + x2')) []
    redPrim Mul [x1, x2] = do
      x1' <- asNum x1
      x2' <- asNum x2
      return $ PAp (Num (x1' * x2')) []
    redPrim Div [x1, x2] = do
      x1' <- asNum x1
      x2' <- asNum x2
      return $ PAp (Num (x1' `quot` x2')) []
    redPrim Mod [x] = do
      x' <- asExpr x
      let mbits v = PAp (MBits v) []
      either (fail . ("reduce: mod: " ++)) (return . mbits) $ modulate_ x'
    redPrim Dem [x] = do
      s <- asMBits x
      either (fail . ("reduce: dem: " ++)) f $ demodulate s
    redPrim Send [x] = do
      let valueExpr (PAp p es)  =  return $ foldl' Ap (Prim p) es
          valueExpr  _          =  fail "reduce: send: cannot convert picture state to expr"
      f =<< send =<< valueExpr =<< f x
    redPrim Neg [x] = do
      x' <- asNum x
      return $ PAp (Num (- x')) []
    redPrim S [x1, b, c] = f $ Ap (Ap x1 c) (Ap b c)
    redPrim C [x1, b, c] = f $ Ap (Ap x1 c) b
    redPrim B [x1, b, c] = f $ Ap x1 (Ap b c)
    redPrim T [x1, _x2] = f x1
    redPrim F [_x1, x2] = f x2
    redPrim Pow2 [x] = do
      x' <- asNum x
      return $ PAp (Num (2 ^ x')) []
    redPrim Pow2 [] = f pwr2Def
    redPrim I [x] = f x
    redPrim Cons [x1, x2, x3] = f $ Ap (Ap x3 x1) x2
    redPrim Car [x] = f $ Ap x (Prim T)
    redPrim Cdr [x] = f $ Ap x (Prim F)
    redPrim Nil [_x] = f $ Prim T
    redPrim IsNil [x] = do
      x' <- f x
      case x' of
        PAp Nil [] -> f (Prim T)
        PAp Cons [_, _] -> f (Prim F)
        _ -> fail $ "IsNil: " ++ show x'
    redPrim If0 [x1, x2, x3] = do
      n <- asNum x1
      if n == 0 then
        f x2
      else
        f x3
    redPrim If0 [x1] = do
      n <- asNum x1
      if n == 0 then f (Prim T)
      else           f (Prim F)
    redPrim Draw [xs] = do
      let p (x1,x2) = do
            x1' <- asNum x1
            x2' <- asNum x2
            return (x1', x2')
      cs <- mapM (p <=< asCons) =<< asList xs
      return $ Picture (Set.fromList cs)
    redPrim Chkb [] = f chkbDef
    redPrim MultiDraw [x] = do
      x' <- asConsOrNil x
      case x' of
        Nothing -> return $ PAp Nil []
        Just (x0, x1) -> f $ Ap (Ap (Prim Cons) (Ap (Prim Draw) x0)) (Ap (Prim MultiDraw) x1)
    redPrim prim args
      | Just (ixs, def) <- Map.lookup prim funcs  =  f $ resolveLocal (IntMap.fromList $ zip ixs args) def
      | otherwise                                 =  fail $ "redPrim: " ++ show prim ++ " " ++ show args

    funcs =
      Map.fromList
      [(Modem, modemDef),
       (F38, f38Def),
       (Interact, interactDef)]

modemDef :: ([Int], Expr)
modemDef = ([0], Ap (Prim Dem) (Ap (Prim Mod) (Prim (LVar 0))))

f38Def :: ([Int], Expr)
f38Def = ([2,0], Ap (Ap (Ap (Prim If0) (Ap (Prim Car) (Prim (LVar 0)))) (Ap (Ap (Prim Cons) (Ap (Prim Modem) (Ap (Prim Car) (Ap (Prim Cdr) (Prim (LVar 0)))))) (Ap (Ap (Prim Cons) (Ap (Prim MultiDraw) (Ap (Prim Car) (Ap (Prim Cdr) (Ap (Prim Cdr) (Prim (LVar 0))))))) (Prim Nil)))) (Ap (Ap (Ap (Prim Interact) (Prim (LVar 2))) (Ap (Prim Modem) (Ap (Prim Car) (Ap (Prim Cdr) (Prim (LVar 0)))))) (Ap (Prim Send) (Ap (Prim Car) (Ap (Prim Cdr) (Ap (Prim Cdr) (Prim (LVar 0))))))))

interactDef :: ([Int], Expr)
interactDef = ([2,4,3], Ap (Ap (Prim F38) (Prim (LVar 2))) (Ap (Ap (Prim (LVar 2)) (Prim (LVar 4))) (Prim (LVar 3))))

type NFValue = NF.NFValue
{-# DEPRECATED NFValue "use NFValue in NFEval.hs instead of this." #-}

reduceNF :: (Monad m, MonadFail m) => (Expr -> m Expr) -> IntMap Expr -> Expr -> m NFValue
reduceNF send env = normalize send env <=< reduce send env

normalize :: forall m. (Monad m, MonadFail m) => (Expr -> m Expr) -> IntMap Expr -> Value -> m NFValue
normalize send env = f
  where
    f (PAp prim args) = do
      args' <- mapM (f <=< reduce send env) args
      return $ NF.NFPAp prim args'
    f (Picture xs) = return $ NF.NFPicture xs


reduceNF' :: IntMap Expr -> Expr -> NFValue
reduceNF' = NF.reduceNF'
