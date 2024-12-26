{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
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

instance Substitutable LCNRType (Context, LCNRType, Env) where
  applySub :: Substitution LCNRType -> (Context, LCNRType, Env) -> (Context, LCNRType, Env)
  applySub s (c, t, e) = (applySub s c, applySub s t, applySub s e)

  substitute :: TypeSub LCNRType -> (Context, LCNRType, Env) -> (Context, LCNRType, Env)
  substitute ts (c, t, e) = (substitute ts c, substitute ts t, substitute ts e )

instance {-# OVERLAPPING #-} Substitutable LCNRType Env where
  applySub :: Substitution LCNRType -> Env -> Env
  applySub s = map (fmap (fmap (applySub s)))

  substitute :: TypeSub LCNRType -> Env -> Env
  substitute ts = map (fmap (fmap (substitute ts)))

type PPState = StateT Int Maybe (Context, LCNRType, Env)
pp :: Expr -> Env -> PPState
pp (Var v) env =
  do n        <- get
     let next  = n + 1
     let fresh = TypeVar next
     put next
     return ([(v, fresh)], fresh, env)
pp (Name name) env =
  do (recu, t) <- lift $ lookup name env
     t'        <- if recu then return t else StateT (\s -> let (sub, state) = runState (freshInstance t) s in Just (applySub sub t, state))
     return ([], t', env)
pp (Abs v expr) env =
  do (pc, pt, pe) <- pp expr env
     next         <- get
     let next'     = next + 1
     let fresh     = TypeVar next'
     case lookup v pc of
       Nothing    -> do put next'
                        return (pc, Arrow fresh pt, pe)
       Just t     -> return (deleteFirstsBy (\ (v1, _) (v2,_) -> v1 == v2) pc [(v, t)], Arrow t pt, pe)
pp (Appl expr1 expr2) env =
  do (pc1, pt1, pe1) <- pp expr1 env
     (pc2, pt2, pe2) <- pp expr2 pe1
     next'           <- get
     let next''       = next' + 1
     let fresh        = TypeVar next''
     s1              <- lift $ unify pt1 (Arrow pt2 fresh)
     s2              <- lift $ unifyContexts (applySub s1 pc1) (applySub s1 pc2)
     put next''
     return $ applySub (s2 . s1) (unionBy (\ (v1, _) (v2,_) -> v1 == v2) pc1 pc2, fresh, pe2)

type BEState = StateT Int Maybe Env
buildEnv :: Defs -> Env -> BEState
buildEnv (Defs []) env = return env
-- recu because rec is special or something
buildEnv (Defs (Def (name, (recu, expr)) : ds)) env
  | recu      = do n            <- get
                   let next      = n + 1
                   let fresh     = TypeVar next
                   put next
                   (pc, pt, pe) <- pp expr ((name, (True, fresh)) : env)
                   (recu', pt') <- lift $ lookup name pe
                   guard recu'
                   s            <- lift $ unify pt pt'
                   rest         <- buildEnv (Defs ds) env
                   return $ (name, (False, applySub s pt)) : rest
  | otherwise = do (pc, pt, pe) <- pp expr env
                   rest         <- buildEnv (Defs ds) env
                   return $ (name, (False, pt)) : rest

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

ppProgram :: Program -> PPState
ppProgram (Program (ds, expr)) = do env <- buildEnv ds []
                                    pp expr env

pp' :: Expr -> Maybe ((Context, LCNRType, Env), Int)
pp' e = runStateT (pp e []) 0

ppProgram' :: Program -> Maybe ((Context, LCNRType, Env), Int)
ppProgram' p = runStateT (ppProgram p) 0

type PPEState = StateT (Int, Env) Maybe (Context, LCNRType)
ppes :: Expr -> PPEState
ppes (Var v) =
  do (n, _)   <- get
     let next  = n + 1
     let fresh = TypeVar next
     modify (first (const next))
     return ([(v, fresh)], fresh)
ppes (Name name) =
  do (_, env)  <- get
     (recu, t) <- lift $ lookup name env
     t'        <- if recu
                  then return t
                  else StateT (\(n, e) -> let (sub, state) = runState (freshInstance t) n in Just (applySub sub t, (state, e)))
     return ([], t')
ppes (Abs v expr) =
  do (pc, pt)  <- ppes expr
     (next, _) <- get
     let next'  = next + 1
     let fresh  = TypeVar next'
     case lookup v pc of
       Nothing -> do modify (first (const next'))
                     return (pc, Arrow fresh pt)
       Just t  -> return (deleteFirstsBy (\ (v1, _) (v2,_) -> v1 == v2) pc [(v, t)], Arrow t pt)
ppes (Appl expr1 expr2) =
  do (pc1, pt1) <- ppes expr1
     (pc2, pt2) <- ppes expr2
     (next', _) <- get
     let next''  = next' + 1
     let fresh   = TypeVar next''
     s1         <- lift $ unify pt1 (Arrow pt2 fresh)
     s2         <- lift $ unifyContexts (applySub s1 pc1) (applySub s1 pc2)
     modify (first (const next''))
     return $ applySub (s2 . s1) (unionBy (\ (v1, _) (v2,_) -> v1 == v2) pc1 pc2, fresh)

type BEEState = StateT (Int, Env) Maybe ()
buildEnves :: Defs -> BEEState
buildEnves (Defs []) = return ()
buildEnves (Defs (Def (name, (recu, expr)) : ds))
  | recu      = do (n, env) <- get
                   let next      = n + 1
                   let fresh     = TypeVar next
                   put (next, (name, (True, fresh)) : env)
                   (pc, pt)     <- ppes expr
                   (_, pe)      <- get
                   (recu', pt') <- lift $ lookup name pe
                   guard recu'
                   s            <- lift $ unify pt pt'
                   modify (second (const env))
                   rest         <- buildEnves (Defs ds)
                   modify (second ((name, (False, applySub s pt)):))
                   return ()
  | otherwise = do (_, env) <- get
                   (pc, pt) <- ppes expr
                   modify (second (const env))
                   rest     <- buildEnves (Defs ds)
                   modify (second ((name, (False, pt)):))
                   return ()

ppesProgram :: Program -> PPEState
ppesProgram (Program (ds, expr)) = do buildEnves ds
                                      ppes expr

ppes' :: Expr -> Maybe ((Context, LCNRType), (Int, Env))
ppes' e = runStateT (ppes e) (0, [])

ppesProgram' p = runStateT (ppesProgram p) (0, [])

compareMaybe :: (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool
compareMaybe _ Nothing Nothing   = True
compareMaybe _ Nothing _         = False
compareMaybe _ _ Nothing         = False
compareMaybe f (Just x) (Just y) = f x y

test e = compareMaybe (\((x, y, z), a) ((x', y'), (a', z')) -> x == x' && y == y' && z == z' && a == a') (pp' e) (ppes' e)


