{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LCNRTypes where
import LCNR (Expr (..), Name, Defs (Defs), Rec, Program (Program), Def (Def))
import CurryTypes (CurryType (..), Variable, Context)
import Control.Monad.Trans.State (StateT (StateT, runStateT), get, State, put, runState, evalState, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad (guard)
import Utilities (unify, Substitutable (applySub, substitute), Type (unifyContexts), Substitution, TypeSub)
import Data.List (deleteFirstsBy, unionBy)
import GHC.Base (build)
import Data.Bifunctor (Bifunctor(second, first))

type LCNRType = CurryType
type Env = [(Name, (Rec, LCNRType))]

freshInstance :: LCNRType -> State Int (Substitution LCNRType)
freshInstance (TypeVar v) =
  do n <- get
     (if v <= n
      then (do let next = n + 1
               put next
               return (substitute (v, TypeVar next)))
      else return id)
freshInstance (Arrow t1 t2) =
  do s1 <- freshInstance t1
     s2 <- freshInstance (applySub s1 t2)
     return $ s2 . s1

type PPEState = StateT (Int, Env) Maybe (Context, LCNRType)
pp :: Expr -> PPEState
pp (Var v) =
  do (n, _)   <- get
     let next  = n + 1
     let fresh = TypeVar next
     modify (first (const next))
     return ([(v, fresh)], fresh)
pp (Name name) =
  do (_, env)  <- get
     (recu, t) <- lift $ lookup name env
     t'        <- if recu
                  then return t
                  else StateT (\(n, e) -> let (sub, state) = runState (freshInstance t) n in Just (applySub sub t, (state, e)))
     return ([], t')
pp (Abs v expr) =
  do (pc, pt)  <- pp expr
     (next, _) <- get
     let next'  = next + 1
     let fresh  = TypeVar next'
     case lookup v pc of
       Nothing -> do modify (first (const next'))
                     return (pc, Arrow fresh pt)
       Just t  -> return (deleteFirstsBy (\ (v1, _) (v2,_) -> v1 == v2) pc [(v, t)], Arrow t pt)
pp (Appl expr1 expr2) =
  do (pc1, pt1) <- pp expr1
     (pc2, pt2) <- pp expr2
     (next', _) <- get
     let next''  = next' + 1
     let fresh   = TypeVar next''
     s1         <- lift $ unify pt1 (Arrow pt2 fresh)
     s2         <- lift $ unifyContexts (applySub s1 pc1) (applySub s1 pc2)
     modify (first (const next''))
     return $ applySub (s2 . s1) (unionBy (\ (v1, _) (v2,_) -> v1 == v2) pc1 pc2, fresh)

type BEEState = StateT (Int, Env) Maybe ()
buildEnv :: Defs -> BEEState
buildEnv (Defs []) = return ()
buildEnv (Defs (Def (name, (recu, expr)) : ds))
  | recu      = do (n, env) <- get
                   let next      = n + 1
                   let fresh     = TypeVar next
                   put (next, (name, (True, fresh)) : env)
                   (pc, pt)     <- pp expr
                   (_, pe)      <- get
                   (recu', pt') <- lift $ lookup name pe
                   guard recu'
                   s            <- lift $ unify pt pt'
                   modify (second (const env))
                   rest         <- buildEnv (Defs ds)
                   modify (second ((name, (False, applySub s pt)):))
                   return ()
  | otherwise = do (_, env) <- get
                   (pc, pt) <- pp expr
                   modify (second (const env))
                   rest     <- buildEnv (Defs ds)
                   modify (second ((name, (False, pt)):))
                   return ()

ppProgram (Program (ds, expr)) = do buildEnv ds
                                    pp expr

pp' e = runStateT (pp e) (0, [])

ppProgram' p = runStateT (ppProgram p) (0, [])
