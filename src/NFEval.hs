module NFEval where

-- import Control.Monad hiding (ap)
-- import Data.List (foldl')
import qualified Data.IntMap.Lazy as IntMap
import Data.IntMap (IntMap)
-- import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

import Message (Expr (..), Prim (..), arity)
-- import Modulate (modulate_, demodulate)


pwr2Def :: Expr
pwr2Def = Ap (Ap (Prim S) (Ap (Ap (Prim C) (Ap (Prim Eq) (Prim (Num 0)))) (Prim (Num 1)))) (Ap (Ap (Prim B) (Ap (Prim Mul) (Prim (Num 2)))) (Ap (Ap (Prim B) (Prim Pow2)) (Ap (Prim Add) (Prim (Num (-1))))))

chkbDef :: Expr
chkbDef = Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim S)) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim C) (Ap (Prim C) (Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim S)) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Ap (Prim S) (Prim I)) (Prim I)))) (Prim Lt)))) (Prim Eq))))) (Ap (Ap (Prim S) (Prim Mul)) (Prim I))))) (Prim Nil)))) (Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim S)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim Cons))) (Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim S)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim Cons))) (Ap (Prim C) (Prim Div))))) (Ap (Prim C) (Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim B)) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Prim Add))) (Prim Neg)))) (Ap (Ap (Prim B) (Ap (Prim S) (Prim Mul))) (Prim Div)))))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Prim Chkb))) (Ap (Ap (Prim C) (Prim Add)) (Prim (Num 2)))))


data NFValue
  = NFPAp Prim [NFValue]
  | NFPicture (Set (Int,Int))
  deriving (Eq, Show)

asNum :: NFValue -> Int
asNum (NFPAp (Num n) []) = n
asNum v = error $ "NFValue: asNum: " ++ show v

asList :: NFValue -> [NFValue]
asList (NFPAp Nil []) = []
asList (NFPAp Cons [x, y]) = x : asList y
asList v = error $ "NFValue: asList: " ++ show v

asExpr :: NFValue -> Expr
asExpr (NFPAp prim args) = foldl (\e arg -> Ap e (asExpr arg)) (Prim prim) args
asExpr x = error $ "NFValue: asExpr: unsupported data constructor: " ++ show x


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

    asCons :: NFValue -> (NFValue, NFValue)
    asCons (NFPAp Cons [x1, x2]) = (x1, x2)
    asCons e = error $ "asCons: " ++ show e

    asConsOrNil :: NFValue -> (Maybe (NFValue, NFValue))
    asConsOrNil (NFPAp Cons [x1, x2]) = Just (x1, x2)
    asConsOrNil (NFPAp Nil []) = Nothing
    asConsOrNil e = error $ "asConsOrNil: " ++ show e

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
