module ML where

data Expr = Var String
            | Const String
            | Abs String Expr
            | Appl Expr Expr
            | Let String Expr Expr
            | Fix String Expr
