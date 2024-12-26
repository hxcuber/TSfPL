{-# LANGUAGE GADTs #-}
module LC3 where

data Var = Var
data Abs = Abs
data Appl = Appl

data Expr a where
  EVar :: String -> Expr Var
  EAbs :: String -> Expr b -> Expr Abs
  EAppl :: Expr b -> Expr c -> Expr Appl

-- substitute :: String -> Expr a -> Expr b -> Expr c
-- substitute v e1 e2
--   = case e2 of
--       EVar v' -> if v == v' then e1 else e2
