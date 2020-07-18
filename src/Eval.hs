{-# LANGUAGE ScopedTypeVariables #-}
module Eval where

import Control.Monad hiding (ap)
import Data.List (foldl')
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import Data.Set (Set)

import Message
import Modulate (modulate_, demodulate)

-- 変数の参照を解決する
resolve :: IntMap Expr -> Expr -> Expr
resolve env = f
  where
    f (Ap a b) = Ap (f a) (f b)
    f (Prim (Var n)) = env' IntMap.! n
    f prim@(Prim _) = prim
    env' = IntMap.map f env

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


data NFValue
  = NFPAp Prim [NFValue]
  | NFPicture (Set (Int,Int))
  deriving (Eq, Show)

reduceNF :: (Monad m, MonadFail m) => (Expr -> m Expr) -> IntMap Expr -> Expr -> m NFValue
reduceNF send env = normalize send env <=< reduce send env

normalize :: forall m. (Monad m, MonadFail m) => (Expr -> m Expr) -> IntMap Expr -> Value -> m NFValue
normalize send env = f
  where
    f (PAp prim args) = do
      args' <- mapM (f <=< reduce send env) args
      return $ NFPAp prim args'
    f (Picture xs) = return $ NFPicture xs


reduceNF' :: IntMap Expr -> Expr -> NFValue
reduceNF' env = ev
  where
    ev :: Expr -> NFValue
    ev (Prim (Var n)) =
      case IntMap.lookup n env of
        Just e -> ev e
        Nothing -> error $ "cannot find (Var " ++ show n ++ ") in environment"
    ev (Prim prim) =
      if arity prim == 0 then
        redPrim prim []
      else
        NFPAp prim []
    ev (Ap fun arg) = ap (ev fun) (ev arg)

    ap :: NFValue -> NFValue -> NFValue
    ap fun arg =
      case fun of
        NFPAp prim xs ->
          if arity prim <= length xs then
            error "should not happen"
          else if arity prim == length xs + 1 then
            redPrim prim (xs ++ [arg])
          else
            NFPAp prim (xs ++ [arg])
        _ -> error $ show fun ++ " is not a function"

    asNum :: NFValue -> Int
    asNum (NFPAp (Num n) []) = n
    asNum e = error $ "asNum: " ++ show e

    asCons :: NFValue -> (NFValue, NFValue)
    asCons (NFPAp Cons [x1, x2]) = (x1, x2)
    asCons e = error $ "asCons: " ++ show e

    asConsOrNil :: NFValue -> (Maybe (NFValue, NFValue))
    asConsOrNil (NFPAp Cons [x1, x2]) = Just (x1, x2)
    asConsOrNil (NFPAp Nil []) = Nothing
    asConsOrNil e = error $ "asConsOrNil: " ++ show e

    asList :: NFValue -> [NFValue]
    asList xs =
      case asConsOrNil xs of
        Nothing -> []
        Just (x, ys) -> x : asList ys

    redPrim prim@(Num _) _ = NFPAp prim []
    redPrim prim@(Var _) _ = NFPAp prim []
    redPrim Eq [x1, x2]
      | asNum x1 == asNum x2 = NFPAp T []
      | otherwise = NFPAp F []
    redPrim Lt [x1, x2]
      | asNum x1 < asNum x2 = NFPAp T []
      | otherwise = NFPAp F []
    redPrim Succ [x1] = NFPAp (Num (asNum x1 + 1)) []
    redPrim Pred [x1] = NFPAp (Num (asNum x1 - 1)) []
    redPrim Add [x1, x2] = NFPAp (Num (asNum x1 + asNum x2)) []
    redPrim Mul [x1, x2] = NFPAp (Num (asNum x1 * asNum x2)) []
    redPrim Div [x1, x2] = NFPAp (Num (asNum x1 `quot` asNum x2)) []
    redPrim Mod [_x] = undefined
    redPrim Dem [_x] = undefined
    redPrim Send [_x] = undefined
    redPrim Neg [x] = NFPAp (Num (- asNum x)) []
    redPrim S [x1, b, c] = ap (ap x1 c) (ap b c)
    redPrim C [x1, b, c] = ap (ap x1 c) b
    redPrim B [x1, b, c] = ap x1 (ap b c)
    redPrim T [x1, _x2] = x1
    redPrim F [_x1, x2] = x2
    redPrim Pow2 [x] = NFPAp (Num (2 ^ asNum x)) []
    redPrim Pow2 [] = ev pwr2Def
    redPrim I [x] = x
    redPrim Cons [x1, x2, x3] = ap (ap x3 x1) x2
    redPrim Car [x] = ap x (NFPAp T [])
    redPrim Cdr [x] = ap x (NFPAp F [])
    redPrim Nil [_x] = NFPAp T []
    redPrim IsNil [x] =
      case x of
        NFPAp Nil [] -> NFPAp T []
        NFPAp Cons [_, _] -> NFPAp F []
        _ -> error $ "IsNil: " ++ show x
    redPrim If0 [x1, x2, x3] = if asNum x1 == 0 then x2 else x3
    redPrim Draw [xs] =
      let cs = map ((\(x1, x2) -> (asNum x1, asNum x2)) . asCons) $ asList xs
       in NFPicture (Set.fromList cs)
    redPrim Chkb [] = ev chkbDef
    redPrim MultiDraw [x] =
      case asConsOrNil x of
        Nothing -> NFPAp Nil []
        Just (x0, x1) -> NFPAp Cons [redPrim Draw [x0], redPrim MultiDraw [x1]]
    redPrim prim args = error $ "redPrim: " ++ show prim ++ " " ++ show args
