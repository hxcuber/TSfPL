{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ML where

data Expr = Var String
            | Const String
            | Abs String Expr
            | Appl Expr Expr
            | Let String Expr Expr
            | Fix String Expr

instance Show Expr where
  show :: Expr -> String
  show (Var v) = v
  show (Const s) = s
  show (Abs s e)    = "\\" ++ s ++ "." ++ show e
  show (Appl e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
  show (Let s e1 e2) = "Let " ++ s ++ " = " ++ show e1 ++ " in " ++ show e2
  show (Fix s e) = "Fix " ++ s ++"." ++ show e

testFix = Fix "r" (Abs "x" (Abs "y" (Appl (Appl (Var "r") (Appl (Appl (Var "r") (Var "y")) (Abs "a" (Abs "b" (Var "a"))))) (Var "x"))))

scomb = Abs "x" (Abs "y" (Abs "z" (Appl (Appl (Var "x") (Var "z")) (Appl (Var "y") (Var "z")))))
kcomb = Abs "x" (Abs "y" (Var "x"))
icomb = Abs "x" (Var "x")
