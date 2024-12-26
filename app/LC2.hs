{-# LANGUAGE GADTs #-}
module LC2 where

data Var = Var
newtype Abs a = Abs (Expr a)
data Appl a b = Appl (Expr a) (Expr b)

data Expr a where
  EVar :: String -> Expr Var
  EAbs :: Expr Var -> Expr a -> Expr (Abs a)
  EAppl :: Expr a -> Expr b -> Expr (Appl a b)

instance Show (Expr a) where
  show (EVar s) = s
  show (EAbs v e) = "(\\" ++ show v ++ " -> " ++ show e ++ ")"
  show (EAppl e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

-- substitute :: Expr a -> Expr b -> Expr Var -> Expr c
-- substitute e1 e2 ev@(EVar v)
--   = case e1 of
--       (EVar v') -> if v == v' then e2 else e1
--       (EAbs ev'@(EVar v') e1') -> if v == v' then e1 else EAbs ev' (substitute e1' e2 ev)
--       (EAppl e1' e2') -> EAppl (substitute e1' e2 ev) (substitute e2' e2 ev)



