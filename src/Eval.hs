{-# LANGUAGE ScopedTypeVariables #-}
module Eval where

import Control.Monad
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import Data.Set (Set)

import Message

data Value
  = PAp Prim [Expr]
  | Picture (Set (Int,Int))
  deriving (Eq, Show)


-- | XXX
-- >>> reduce pure IntMap.empty (Prim (Num 42))
-- PAp (Num 42) []
--
-- >>> reduce pure IntMap.empty (Ap (Prim Pred) (Ap (Ap (Prim Add) (Prim (Num 1))) (Prim (Num 2))))
-- PAp (Num 2) []
--
-- >>> :{
-- let env = IntMap.fromList [(2048,(Ap (Prim F) (Prim (Var 2048))))]
--     exp = Ap (Ap (Prim F) (Prim (Var 2048))) (Prim (Num 42))
-- in reduce pure env exp
-- :}
-- PAp (Num 42) []
--
reduce :: forall m. (Monad m, MonadFail m) => (Value -> m Value) -> IntMap Expr -> Expr -> m Value
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
    redPrim Mod [_x] = undefined
    redPrim Dem [_x] = undefined
    redPrim Send [x] = send =<< f x
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
    redPrim prim args = fail $ "redPrim: " ++ show prim ++ " " ++ show args


pwr2Def :: Expr
pwr2Def = Ap (Ap (Prim S) (Ap (Ap (Prim C) (Ap (Prim Eq) (Prim (Num 0)))) (Prim (Num 1)))) (Ap (Ap (Prim B) (Ap (Prim Mul) (Prim (Num 2)))) (Ap (Ap (Prim B) (Prim Pow2)) (Ap (Prim Add) (Prim (Num (-1))))))

chkbDef :: Expr
chkbDef = Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim S)) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim C) (Ap (Prim C) (Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim S)) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Ap (Prim S) (Prim I)) (Prim I)))) (Prim Lt)))) (Prim Eq))))) (Ap (Ap (Prim S) (Prim Mul)) (Prim I))))) (Prim Nil)))) (Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim S)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim Cons))) (Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim S)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim Cons))) (Ap (Prim C) (Prim Div))))) (Ap (Prim C) (Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim B)) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Prim Add))) (Prim Neg)))) (Ap (Ap (Prim B) (Ap (Prim S) (Prim Mul))) (Prim Div)))))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Prim Chkb))) (Ap (Ap (Prim C) (Prim Add)) (Prim (Num 2)))))
