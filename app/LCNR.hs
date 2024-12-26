{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module LCNR where

type Name = String
type Variable = String
data Expr = Var String | Name Name | Abs String Expr | Appl Expr Expr
type Rec = Bool
newtype Def = Def (Name, (Rec, Expr))
newtype Defs = Defs [Def]
newtype Program = Program (Defs, Expr)

instance Show Expr where
  show (Var v) = v
  show (Name name) = name
  show (Abs s e)    = "\\" ++ s ++ "." ++ show e
  show (Appl e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"

instance Show Def where
  show :: Def -> String
  show (Def (name, (recu, expr)))
    = (if recu then "rec " else " ") ++ name ++ " = " ++ show expr

instance Show Defs where
  show :: Defs -> String
  show (Defs []) = ""
  show (Defs [d]) = show d
  show (Defs (d : ds)) = show d ++ ";\n"

instance Show Program where
  show :: Program -> String
  show (Program (ds, e)) = show ds ++ ":\n" ++ show e

scomb = Abs "x" (Abs "y" (Abs "z" (Appl (Appl (Var "x") (Var "z")) (Appl (Var "y") (Var "z")))))
kcomb = Abs "x" (Abs "y" (Var "x"))
icomb = Abs "x" (Var "x")
ycomb = Abs "g" (Appl (Abs "x" (Appl (Var "g") (Appl (Var "x") (Var "x")))) (Abs "x" (Appl (Var "g") (Appl (Var "x") (Var "x")))))
-- Y = S(K(SII))(S(S(KS)K))(K(SII))
ycomb' = Appl (Appl (Appl scomb (Appl kcomb (Appl (Appl scomb icomb) icomb))) (Appl scomb (Appl (Appl scomb (Appl kcomb scomb)) kcomb))) (Appl kcomb (Appl (Appl scomb icomb) icomb))
combs = [scomb, kcomb, icomb, ycomb, ycomb']
