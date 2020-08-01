module HsLikePrinter (
  pprDefinition,

  Term (..),
  pprTop, pprTerm, unAp,
  ) where

import Data.Char (toLower)
import Data.List (intersperse)

import Message (Expr (Ap, Prim), Prim (..))

pprDefinition :: Int -> Expr -> String
pprDefinition n e =
  "f" ++ show n ++ " =" ++ pprTop (unAp e)

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

pprTop :: Term -> String
pprTop =
    ppr
  where
    ppr (PrimTM p)   =  ss " " . pprPrim p . ss "\n" $ ""
    ppr (AppTM [PrimTM B, x])    = ss " " . pprB1 x $ ""
    ppr (AppTM [PrimTM B, x, y]) = ss " " . pprB2 x y $ ""
    ppr (AppTM (PrimTM B : x : y : ts)) = ss " " . pprBMany x y ts $ ""
    ppr (AppTM ts)   =  unlines $ "" : map (("  " ++) . ($ "") . pprTerm) ts
    ppr (ListSYN ts) =  ss " " . pprList ts . ss "\n" $ ""

pprTerm :: Term -> ShowS
pprTerm =
    ppr
  where
    ppr (PrimTM p)  = pprPrim p
    ppr (AppTM [PrimTM B, x])    = pprB1 x
    ppr (AppTM [PrimTM B, x, y]) = pprB2 x y
    ppr (AppTM (PrimTM B : x : y : ts)) = pprBMany x y ts
    ppr (AppTM ts) = ss "( " . sepBy " " (map ppr ts) . ss " )"
    ppr (ListSYN ts) =  pprList ts

pprB1 :: Term -> ShowS
pprB1 x = ss "( " . pprTerm x . ss " . )"

pprB2 :: Term -> Term -> ShowS
pprB2 x y = ss "( " . pprTerm x . ss " . " . pprTerm y . ss " )"

pprBMany :: Term -> Term -> [Term] -> ShowS
pprBMany x y ts =
  ss "( " . ss "( " . pprTerm x . ss " . " . pprTerm y . ss " )" . sepBy " " (map pprTerm ts) . ss " )"

pprList :: [Term] -> ShowS
pprList ts = ss "[" . sepBy ", " (map pprTerm ts) . ss "]"

pprPrim :: Prim -> ShowS
pprPrim =
    showString . ppr
  where
    ppr (Var n) = "f" ++ show n
    ppr (Num n) = show n
    ppr  Succ   = "inc"
    ppr  Pred   = "dec"
    ppr  Pow2   = "pwr2"
    ppr  B      = "(.)"
    ppr  C      = "flip"
    ppr  p      = lname p

    lname :: Prim -> String
    lname = map toLower . show

{-
_ex1 :: Expr
_ex1 = Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim S)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim S) (Ap (Ap (Prim B) (Prim B)) (Ap (Ap (Prim B) (Prim B)) (Prim B)))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim S))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim S)))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim B))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim B))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim B))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim B))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim (Var 1476))))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim C) (Prim (Var 1115))))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim C) (Prim (Var 1127)))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim B))) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim C)))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim C))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Prim B)))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim B))) (Ap (Prim C) (Prim (Var 1494)))))) (Prim I)))))) (Prim I))))))) (Prim I))))))) (Prim (Num 0))))))))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim Cons) (Prim Nil)))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim Cons))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim Cons))) (Prim (Var 1131))))) (Prim Nil))))) (Prim Nil))))))))))) (Ap (Ap (Prim B) (Ap (Prim B) (Prim (Var 1134)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim (Var 1126)) (Ap (Prim (Var 1139)) (Prim (Num 8)))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim (Var 1183)) (Ap (Prim (Var 1199)) (Prim (Num 1))))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Ap (Ap (Prim B) (Prim B)) (Ap (Ap (Prim B) (Prim (Var 1162))) (Ap (Prim Add) (Prim (Num (-3)))))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Prim Add))) (Ap (Ap (Prim C) (Prim Mul)) (Prim (Num 3))))))))))))))) (Ap (Ap (Prim B) (Ap (Prim B) (Prim (Var 1134)))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim (Var 1126)) (Ap (Prim (Var 1139)) (Prim (Num 8)))))) (Ap (Ap (Prim B) (Ap (Prim B) (Ap (Prim B) (Ap (Prim (Var 1183)) (Ap (Prim (Var 1199)) (Prim (Num 1))))))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Ap (Ap (Prim B) (Prim C)) (Ap (Ap (Prim B) (Ap (Prim B) (Prim (Var 1162)))) (Ap (Ap (Prim C) (Ap (Ap (Prim B) (Prim B)) (Prim Add))) (Ap (Ap (Prim C) (Prim Mul)) (Prim (Num 3)))))))) (Ap (Prim Add) (Prim (Num (-3))))))))))))) (Ap (Ap (Prim C) (Prim (Var 1208))) (Prim (Num 64)))
 -}
