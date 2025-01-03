{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module CurryTypes where
import Data.Bifunctor (bimap)
import LC (Expr (..), scomb, kcomb, icomb, ycomb)
import Data.List ((\\), deleteFirstsBy, unionBy)
import Utilities (Substitutable (..), Substitution, Type (..), TypeSub)
import Debug.Trace (trace)
import Control.Monad.Trans.State ( StateT (runStateT), get, put, State, runState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Maybe (isJust)

data CurryType where
  TypeVar :: Int -> CurryType
  Arrow :: CurryType -> CurryType -> CurryType
  deriving Eq
type TypeVariable = Int
type Variable = String
type Context = [(Variable, CurryType)]

instance Show CurryType where
  show :: CurryType -> String
  show (TypeVar i)   = show i
  show (Arrow t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2

instance Substitutable CurryType CurryType where
  applySub :: Substitution CurryType -> CurryType -> CurryType
  applySub sub = sub

  substitute :: TypeSub CurryType -> CurryType -> CurryType
  substitute s@(v, c) t =
    case t of
      TypeVar v' -> if v == v' then c else t
      Arrow a b  -> Arrow (substitute s a) (substitute s b)

instance Type CurryType where
  occurs :: TypeVariable -> CurryType -> Bool
  occurs v t =
    case t of
      TypeVar v'  -> v == v'
      Arrow a b -> occurs v a || occurs v b

  unify :: CurryType -> CurryType -> Maybe (Substitution CurryType)
  unify (TypeVar v) v'@(TypeVar _) =
    Just (substitute (v, v'))
  unify (TypeVar v) b =
    if occurs v b then Nothing else Just (substitute (v, b))
  unify b v@(TypeVar _) =
    unify v b
  unify (Arrow a b) (Arrow c d) =
    do s1 <- unify a c
       s2 <- unify (s1 b) (s1 d)
       return $ s2 . s1
  typeVar = TypeVar
  arrow = Arrow

type PPState = StateT Int Maybe (Context, CurryType)
pp :: Expr -> PPState
pp (Var x) =
  do n        <- get
     let next  = n + 1
     let fresh = TypeVar next
     put next
     return ([(x, fresh)], fresh)
pp (Abs x m) =
  do (pc, p)  <- pp m
     n        <- get
     let next  = n + 1
     let fresh = TypeVar next
     case lookup x pc of
       Nothing -> do put next
                     return (pc, Arrow fresh p)
       Just a  -> return (deleteFirstsBy (\ (v1, _) (v2,_) -> v1 == v2) pc [(x, a)], Arrow a p)
pp (Appl m n) =
  do (pc1, p1) <- pp m
     (pc2, p2) <- pp n
     next      <- get
     let next'  = next + 1
     let fresh  = TypeVar next'
     s1        <- lift $ unify p1 (Arrow p2 fresh)
     s2        <- lift $ unifyContexts (applySub s1 pc1) (applySub s1 pc2)
     put next'
     return $ applySub (s2 . s1) (unionBy (\ (v1, _) (v2,_) -> v1 == v2) pc1 pc2, fresh)

pp' e = runStateT (pp e) 0

type PPMaybe = MaybeT (State Int) (Context, CurryType)

ppm :: Expr -> PPMaybe
ppm (Var x) =
  do n        <- lift get
     let next  = n + 1
     let fresh = TypeVar next
     lift $ put next
     return ([(x, fresh)], fresh)
ppm (Abs x m) =
  do (pc, p)  <- ppm m
     n        <- lift get
     let next  = n + 1
     let fresh = TypeVar next
     case lookup x pc of
       Nothing -> do lift $ put next
                     return (pc, Arrow fresh p)
       Just t  -> return (deleteFirstsBy (\ (v1, _) (v2,_) -> v1 == v2) pc [(x, t)], Arrow t p)
ppm (Appl m n) =
  do (pc1, p1) <- ppm m
     (pc2, p2) <- ppm n
     next      <- lift get
     let next'  = next + 1
     let fresh  = TypeVar next'
     s1        <- liftMaybe $ unify p1 (Arrow p2 fresh)
     s2        <- liftMaybe $ unifyContexts (applySub s1 pc1) (applySub s1 pc2)
     lift $ put next'
     return $ applySub (s2 . s1) (unionBy (\ (v1, _) (v2,_) -> v1 == v2) pc1 pc2, fresh)

liftMaybe :: Maybe a -> MaybeT (State b) a
liftMaybe = MaybeT . return

ppm' e = runState (runMaybeT (ppm e)) 0

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
