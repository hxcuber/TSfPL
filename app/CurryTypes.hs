{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module CurryTypes where
import Data.Bifunctor (bimap)
import LC (Expr (..))
import Data.List ((\\), deleteFirstsBy, unionBy)
import Utilities (Substitutable (..), Substitution, Type (..), TypeSub)
import Debug.Trace (trace)
import Control.Monad.Trans.State ( StateT (runStateT), get, put, State, runState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))

data CurryType = TypeVar Int | Arrow CurryType CurryType deriving Eq
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
  substitute ts@(v, t1) t2 =
    case t2 of
      TypeVar v'    -> if v == v' then t1 else t2
      Arrow t1' t2' -> Arrow (substitute ts t1') (substitute ts t2')

instance Type CurryType where
  occurs :: TypeVariable -> CurryType -> Bool
  occurs v t =
    case t of
      TypeVar v'  -> v == v'
      Arrow t1 t2 -> occurs v t1 || occurs v t2

  unify :: CurryType -> CurryType -> Maybe (Substitution CurryType)
  unify (TypeVar v) tv'@(TypeVar v') =
    Just (substitute (v, tv'))
  unify (TypeVar v) tv' =
    if occurs v tv' then Nothing else Just (substitute (v,tv'))
  unify tv tv'@(TypeVar _) =
    unify tv' tv
  unify (Arrow t1 t2) (Arrow t1' t2') =
    do s1 <- unify t1 t1'
       s2 <- unify (s1 t2) (s1 t2')
       return $ s2 . s1
  typeVar = TypeVar
  arrow = Arrow

pp :: Int -> Expr -> Maybe ((Context, CurryType), Int)
pp n (Var v) =
  Just (([(v, fresh)], fresh), next)
    where
      next = n+1
      fresh = TypeVar next
pp n (Abs v e) =
  do ((pc, pt), next) <- pp n e
     let next'         = next+1
     let fresh         = TypeVar next'
     let doesntOccur   = ((pc, Arrow fresh pt), next')
     let doesOccur t   = ((deleteFirstsBy (\ (v1, _) (v2,_) -> v1 == v2) pc [(v, t)], Arrow t pt), next)
     return $ maybe doesntOccur doesOccur (lookup v pc)
pp n (Appl e1 e2) =
  do ((pc1, pt1), next)  <- pp n e1
     ((pc2, pt2), next') <- pp next e2
     let next''           = next' + 1
     let fresh            = TypeVar next''
     s1                  <- unify pt1 (Arrow pt2 fresh)
     s2                  <- unifyContexts (applySub s1 pc1) (applySub s1 pc2)
     return (applySub (s2 . s1) (unionBy (\ (v1, _) (v2,_) -> v1 == v2) pc1 pc2, fresh), next'')

pp' = pp 0

type PPState = StateT Int Maybe (Context, CurryType)

pps :: Expr -> PPState
pps (Var v) =
  do n <- get
     let next  = n + 1
     let fresh = TypeVar next
     put next
     return ([(v, fresh)], fresh)
pps (Abs v e) =
  do (pc, pt)  <- pps e
     next      <- get
     let next'  = next + 1
     let fresh  = TypeVar next'
     case lookup v pc of
       Nothing -> do put next'
                     return (pc, Arrow fresh pt)
       Just t  -> return (deleteFirstsBy (\ (v1, _) (v2,_) -> v1 == v2) pc [(v, t)], Arrow t pt)
pps (Appl e1 e2) =
  do (pc1, pt1) <- pps e1
     (pc2, pt2) <- pps e2
     next'      <- get
     let next''  = next' + 1
     let fresh   = TypeVar next''
     s1         <- lift $ unify pt1 (Arrow pt2 fresh)
     s2         <- lift $ unifyContexts (applySub s1 pc1) (applySub s1 pc2)
     put next''
     return $ applySub (s2 . s1) (unionBy (\ (v1, _) (v2,_) -> v1 == v2) pc1 pc2, fresh)

pps' e = runStateT (pps e) 0

type PPMaybe = MaybeT (State Int) (Context, CurryType)

ppm :: Expr -> PPMaybe
ppm (Var v) =
  do n        <- lift get
     let next  = n + 1
     let fresh = TypeVar next
     lift $ put next
     return ([(v, fresh)], fresh)
ppm (Abs v e) =
  do (pc, pt)  <- ppm e
     next      <- lift get
     let next'  = next + 1
     let fresh  = TypeVar next'
     case lookup v pc of
       Nothing -> do lift $ put next'
                     return (pc, Arrow fresh pt)
       Just t  -> return (deleteFirstsBy (\ (v1, _) (v2,_) -> v1 == v2) pc [(v, t)], Arrow t pt)
ppm (Appl e1 e2) =
  do (pc1, pt1) <- ppm e1
     (pc2, pt2) <- ppm e2
     next'      <- lift get
     let next''  = next' + 1
     let fresh   = TypeVar next''
     s1         <- liftMaybe $ unify pt1 (Arrow pt2 fresh)
     s2         <- liftMaybe $ unifyContexts (applySub s1 pc1) (applySub s1 pc2)
     lift $ put next''
     return $ applySub (s2 . s1) (unionBy (\ (v1, _) (v2,_) -> v1 == v2) pc1 pc2, fresh)

liftMaybe :: Maybe a -> MaybeT (State b) a
liftMaybe = MaybeT . return

ppm' e = runState (runMaybeT (ppm e)) 0

test e = pp' e == pps' e
