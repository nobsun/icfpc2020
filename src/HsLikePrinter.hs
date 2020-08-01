module HsLikePrinter (
  pprDefinition,
  emptyEnv, singleExprEnv,

  Term (..), Env,
  pprTop, pprTerm, unAp,
  ) where

import Data.Char (toLower)
import Data.List (intersperse)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map

import Message (Expr (Ap, Prim), Prim (..))

type Env = Map Int Expr

emptyEnv :: Env
emptyEnv = Map.empty

singleExprEnv :: [(Int, Expr)] -> Env
singleExprEnv ps =
  Map.fromList
  [ (n, e)
  | (n, expr) <- ps
  , Just e <- [singleExpr expr]
  ]

singleExpr :: Expr -> Maybe Expr
singleExpr =
    single
  where
    single e@(Prim _) =
      Just e
    single e@(Ap (Ap (Prim Cons) x) (Prim Nil)) = do
      _ <- single x
      return e
    single _ =
      Nothing

pprDefinition :: Env -> Int -> Expr -> String
pprDefinition env n e =
  "f" ++ show n ++ " =" ++ pprTop env (unAp e)

{-
  single primitive term like
     nil
  or term list
     s k k (cons 1 nil)
 -}

data Term
  = PrimTM Prim
  | AppTM [Term]
  | ListSYN [Term]
  deriving Show

unAp :: Expr -> Term
unAp =
    urec0
  where
    urec0 = urec []

    urec :: [Expr] -> Expr -> Term
    urec xs (Prim p)
      | null xs    =  PrimTM p
      | otherwise  =  AppTM $ PrimTM p : map urec0 xs
    urec xs ex@(Ap f a)  =
      maybe (urec (a:xs) f) (ListSYN . map urec0) $ list id ex

    list c (Prim Nil)                  = Just $ c []
    list c (Ap (Ap (Prim Cons) hd) tl) = list (c . (hd :)) tl
    list _  _                          = Nothing

_ex0 :: Expr
_ex0 = Ap (Ap (Prim Cons) (Prim (Num 0))) (Prim Nil)

sepBy :: String -> [ShowS] -> ShowS
sepBy d = foldr (.) id . intersperse (showString d)

ss :: String -> ShowS
ss = showString

pprTop :: Env -> Term -> String
pprTop env =
    ppr
  where
    ppr (PrimTM p)   =  ss " " . pprPrim env p . ss "\n" $ ""
    ppr (AppTM [PrimTM B, x])    = ss " " . pprB1 env x . ss "\n" $ ""
    ppr (AppTM [PrimTM B, x, y]) = ss " " . pprB2 env x y . ss "\n" $ ""
    ppr (AppTM (PrimTM B : x : y : ts)) = ss " " . pprBMany env x y ts . ss "\n" $ ""
    ppr (AppTM ts)   =  unlines $ "" : map (("  " ++) . ($ "") . pprTerm env) ts
    ppr (ListSYN ts) =  pprListNL env ts $ ""

pprTerm :: Env -> Term -> ShowS
pprTerm env =
    ppr
  where
    ppr (PrimTM p)   = pprPrim env p
    ppr (AppTM [PrimTM B, x])    = pprB1 env x
    ppr (AppTM [PrimTM B, x, y]) = pprB2 env x y
    ppr (AppTM (PrimTM B : x : y : ts)) = pprBMany env x y ts
    ppr (AppTM ts)   = ss "( " . sepBy " " (map ppr ts) . ss " )"
    ppr (ListSYN ts) = pprList env ts

pprB1 :: Env -> Term -> ShowS
pprB1 env x = ss "( " . pprTerm env x . ss " . )"

pprB2 :: Env -> Term -> Term -> ShowS
pprB2 env x y = ss "( " . pprTerm env x . ss " . " . pprTerm env y . ss " )"

pprBMany :: Env -> Term -> Term -> [Term] -> ShowS
pprBMany env x y ts =
  ss "( " . ss "( " . pprTerm env x . ss " . " . pprTerm env y . ss " )" . sepBy " " (map (pprTerm env) ts) . ss " )"

pprList :: Env -> [Term] -> ShowS
pprList env ts = ss "[" . sepBy ", " (map (pprTerm env) ts) . ss "]"

pprListNL :: Map Int Expr -> [Term] -> String -> String
pprListNL env =
    fmt . chunksOf 8
  where
    fmt  []     = ss " " . ss "[]" . ss "\n"
    fmt [ts]    = ss " " . ss "[" . sepBy ", " (map (pprTerm env) ts) . ss "]" . ss "\n"
    fmt (ts:cs) = ss "\n" . ss "  [" . sepBy ", " (map (pprTerm env) ts) . ss "," . ss "\n" .
                  fmt1 cs
    fmt1 []      = error "prListNL: fmt1: nil: shound not happen."
    fmt1 [ts]    = ss "   " . sepBy ", " (map (pprTerm env) ts) . ss "]\n"
    fmt1 (ts:cs) = ss "   " . sepBy ", " (map (pprTerm env) ts) . ss ",\n" .
                   fmt1 cs


pprPrim :: Env -> Prim -> ShowS
pprPrim env =
    ppr
  where
    ppr (Var n) = maybe (ss "f" . shows n) (pprTerm env . unAp) $ Map.lookup n env
    ppr (Num n) = shows n
    ppr  Succ   = ss "inc"
    ppr  Pred   = ss "dec"
    ppr  Pow2   = ss "pwr2"
    ppr  B      = ss "(.)"
    ppr  C      = ss "flip"
    ppr  p      = lname p

    lname :: Prim -> ShowS
    lname = ss . map toLower . show

{-
_ex1 :: Expr
_ex1 = Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim S)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim B)) (Ap (Ap (Prim B) (Prim B)) (Prim B)))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim S))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim S)))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim B))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim B))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim B))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim B))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim (Var 1476))))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim C) (Prim (Var 1115))))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim C) (Prim (Var 1127)))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim B))) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C)))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim B)))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim B))) (Ap (Prim C) (Prim (Var 1494)))))) (Prim I)))))) (Prim I))))))) (Prim I))))))) (Prim (Num 0))))))))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim Cons) (Prim Nil)))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim Cons))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim Cons))) (Prim (Var 1131))))) (Prim Nil))))) (Prim Nil))))))))))) (Ap (Ap (Prim B) (Ap (Prim B) (Prim (Var 1134)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim (Var 1126)) (Ap (Prim (Var 1139)) (Prim (Num 8)))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim (Var 1183)) (Ap (Prim (Var 1199)) (Prim (Num 1))))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Ap (Ap (Prim B) (Prim B)) (Ap (Ap (Prim B) (Prim (Var 1162))) (Ap (Prim Add) (Prim (Num (-3)))))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Prim Add))) (Ap (Ap (Prim C) (Prim Mul)) (Prim (Num 3))))))))))))))) (Ap (Ap (Prim B) (Ap (Prim B) (Prim (Var 1134)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim (Var 1126)) (Ap (Prim (Var 1139)) (Prim (Num 8)))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim (Var 1183)) (Ap (Prim (Var 1199)) (Prim (Num 1))))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim (Var 1162)))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Prim Add))) (Ap (Ap (Prim C) (Prim Mul)) (Prim (Num 3)))))))) (Ap (Prim Add) (Prim (Num (-3))))))))))))) (Ap (Ap (Prim C) (Prim (Var 1208))) (Prim (Num 64)))
 -}
