{-# LANGUAGE InstanceSigs #-}
module LC where

-- TODO turn into Abs (Var String) Expr
data Expr = Var String | Abs String Expr | Appl Expr Expr
type ReductionStratgy = Expr -> Expr

instance Show Expr where
  show :: Expr -> String
  show (Var s)      = s
  show (Abs s e)    = "\\" ++ s ++ "." ++ show e
  show (Appl e1 e2) = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"

substitute :: String -> Expr -> Expr -> Expr
substitute v e1 e2
  = case e1 of
      Var v'        -> if v == v' then e2 else e1
      Abs v' e1'    -> if v == v' then e1 else Abs v' (substitute v e1' e2)
      Appl e1' e1'' -> Appl (substitute v e1' e2) (substitute v e1'' e2)

-- TODO make type sig Abs (Var String) Expr -> Expr
betaReduce :: Expr -> Expr
betaReduce (Appl (Abs v e1) e2) = substitute v e1 e2

oneStepReduce :: Expr -> Expr
oneStepReduce = betaReduce

isNF :: Expr -> Bool
isNF (Var _)           = True
isNF (Abs _ e)         = isNF e
isNF (Appl (Var _) e2) = isNF e2
isNF _                 = False

isHNF :: Expr -> Bool
isHNF (Var _)           = True
isHNF (Abs _ e)         = isNF e
isHNF (Appl (Var _) e2) = True
isHNF _                 = False

-- TODO many steps reduction

headReduce :: Expr -> Expr
headReduce e@(Appl (Abs _ _) _) = betaReduce e
headReduce (Abs v e)            = Abs v (headReduce e)
headReduce (Appl e1 e2)         = Appl (headReduce e1) e2

callByNameReduce :: Expr -> Expr
callByNameReduce e@(Appl (Abs _ _) _) = betaReduce e
callByNameReduce (Appl e1 e2)         = Appl (headReduce e1) e2

callByValueReduce :: Expr -> Expr
callByValueReduce e@(Appl (Abs _ _) (Abs _ _)) = betaReduce e
callByValueReduce e@(Appl (Abs _ _) (Var _)) = betaReduce e

scomb = Abs "x" (Abs "y" (Abs "z" (Appl (Appl (Var "x") (Var "z")) (Appl (Var "y") (Var "z")))))
kcomb = Abs "x" (Abs "y" (Var "x"))
icomb = Abs "x" (Var "x")
ycomb = Abs "g" (Appl (Abs "x" (Appl (Var "g") (Appl (Var "x") (Var "x")))) (Abs "x" (Appl (Var "g") (Appl (Var "x") (Var "x")))))
-- Y = S(K(SII))(S(S(KS)K))(K(SII))
ycomb' = Appl (Appl (Appl scomb (Appl kcomb (Appl (Appl scomb icomb) icomb))) (Appl scomb (Appl (Appl scomb (Appl kcomb scomb)) kcomb))) (Appl kcomb (Appl (Appl scomb icomb) icomb))
combs = [scomb, kcomb, icomb, ycomb, ycomb']
