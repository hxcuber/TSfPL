{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LCNRTypes where
import LCNR (Expr (..), Name, Defs (Defs), Rec, Program (Program), Def (Def), icomb, ycomb, kcomb, scomb)
import CurryTypes (CurryType (..), Variable, Context)
import Control.Monad.Trans.State (StateT (StateT, runStateT), get, State, put, runState, evalState, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad (guard)
import Utilities (unify, Substitutable (applySub, substitute), Type (unifyContexts), Substitution, TypeSub)
import Data.List (deleteFirstsBy, unionBy)
import Data.Bifunctor (Bifunctor(second, first))
import Data.Maybe (isJust)

type LCNRType = CurryType
type Env = [(Name, (Rec, LCNRType))]
type PPEState = StateT (Int, Env) Maybe (Context, LCNRType)

pp :: Expr -> PPEState
pp e =
  case e of
    Var x ->
      do (n, _)   <- get
         let next  = n + 1
         let fresh = TypeVar next
         modify (first (const next))
         return ([(x, fresh)], fresh)
    Name name ->
      do (_, e)    <- get
         (recu, a) <- lift $ lookup name e
         a'        <- if recu
                      then return a
                      else StateT (\(n, e') -> let (a'', state) = runState (freshInstance a) n in Just (a'', (state, e')))
         return ([], a')
    Abs x m ->
      do (pc, p)  <- pp m
         (n, _)   <- get
         let next  = n + 1
         let fresh = TypeVar next
         case lookup x pc of
           Nothing -> do modify (first (const next))
                         return (pc, Arrow fresh p)
           Just a  -> return (deleteFirstsBy (\ (v1, _) (v2,_) -> v1 == v2) pc [(x, a)], Arrow a p)
    Appl m n ->
      do (pc1, p1) <- pp m
         (pc2, p2) <- pp n
         (next, _) <- get
         let next'  = next + 1
         let fresh  = TypeVar next'
         s1        <- lift $ unify p1 (Arrow p2 fresh)
         s2        <- lift $ unifyContexts (applySub s1 pc1) (applySub s1 pc2)
         modify (first (const next'))
         return $ applySub (s2 . s1) (unionBy (\ (v1, _) (v2,_) -> v1 == v2) pc1 pc2, fresh)
    where
      freshInstance :: LCNRType -> State Int LCNRType
      freshInstance t = do fis <- freshInstance' t
                           return $ applySub fis t

      freshInstance' :: LCNRType -> State Int (Substitution LCNRType)
      freshInstance' (TypeVar v) =
        do n <- get
           if v <= n
           then (do let next = n + 1
                    put next
                    return (substitute (v, TypeVar next)))
           else return id
      freshInstance' (Arrow t1 t2) =
        do s1 <- freshInstance' t1
           s2 <- freshInstance' (applySub s1 t2)
           return $ s2 . s1

type BEEState = StateT (Int, Env) Maybe ()
buildEnv :: Defs -> BEEState
buildEnv (Defs []) = return ()
buildEnv (Defs (Def (name, (recu, m)) : defs))
  | recu      = do (n, e)      <- get
                   let next     = n + 1
                   let fresh    = TypeVar next
                   put (next, (name, (True, fresh)) : e)
                   (pc, a)     <- pp m
                   (_, pe)     <- get
                   (recu', b)  <- lift $ lookup name pe
                   guard recu'
                   s           <- lift $ unify a b
                   modify (second (const e))
                   buildEnv (Defs defs)
                   modify (second ((name, (False, applySub s a)):))
                   return ()
  | otherwise = do (_, e)  <- get
                   (pc, a) <- pp m
                   guard (null pc)
                   (_, pe) <- get
                   guard (e == pe)
                   buildEnv (Defs defs)
                   modify (second ((name, (False, a)):))
                   return ()

ppProgram (Program (ds, expr)) = do buildEnv ds
                                    pp expr

pp' e = runStateT (pp e) (0, [])

ppProgram' p = runStateT (ppProgram p) (0, [])

testOne (expr, exp) = helper exp actual
  where
    helper Nothing Nothing     = True
    helper _ Nothing           = False
    helper Nothing _           = False
    helper (Just t1) (Just ((_, t2), _)) = isJust (unify t1 t2)

    actual = pp' expr

testAll = map testOne

testCases = [
              (scomb, Just $ Arrow (Arrow (TypeVar 1) (Arrow (TypeVar 2) (TypeVar 3))) (Arrow (Arrow (TypeVar 1) (TypeVar 2)) (Arrow (TypeVar 1) (TypeVar 3)))),
              (kcomb, Just $ Arrow (TypeVar 1) (Arrow (TypeVar 2) (TypeVar 1))),
              (icomb, Just $ Arrow (TypeVar 1) (TypeVar 1)),
              (ycomb, Nothing)
            ]

test = testAll testCases
