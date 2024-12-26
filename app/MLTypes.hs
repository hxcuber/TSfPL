{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module MLTypes where
import Utilities (Substitutable (..), Substitution, Type (..), Variable, TypeSub)
import Data.Bifunctor (Bifunctor(second))

data BasicType = TypeVar Int | Const String | Arrow BasicType BasicType
data PolyType = BasicType BasicType | Forall Int PolyType
type Context = [(String, PolyType)]

instance Substitutable BasicType BasicType where
  applySub :: Substitution BasicType -> BasicType -> BasicType
  applySub s = s

  substitute :: TypeSub BasicType -> BasicType -> BasicType
  substitute ts@(v, t1) t2
    = case t2 of
        TypeVar v'    -> if v == v' then t1 else t2
        c@(Const t)   -> c
        Arrow t1' t2' -> Arrow (substitute ts t1') (substitute ts t2')

instance Substitutable BasicType PolyType where
  applySub :: Substitution BasicType -> PolyType -> PolyType
  applySub s (BasicType bt) = BasicType (applySub s bt)
  applySub s (Forall v pt)  = Forall v (applySub s pt)

  substitute :: TypeSub BasicType -> PolyType -> PolyType
  substitute ts@(v, t1) t2 =
    case t2 of
      BasicType bt -> BasicType $ substitute ts bt
      Forall i pt -> Forall i (substitute ts pt)

instance Substitutable BasicType [(String, BasicType)] where
  applySub :: Substitution BasicType -> [(String, BasicType)] -> [(String, BasicType)]
  applySub s = map (fmap (applySub s))

  substitute :: TypeSub BasicType -> [(String, BasicType)] -> [(String, BasicType)]
  substitute ts = map (fmap (substitute ts))

instance Substitutable BasicType Context where
  applySub :: Substitution BasicType -> Context -> Context
  applySub s = map (fmap (applySub s))

  substitute :: TypeSub BasicType -> Context -> Context
  substitute ts = map (fmap (substitute ts))

instance Type BasicType where
  typeVar :: Int -> BasicType
  typeVar = TypeVar

  arrow :: BasicType -> BasicType -> BasicType
  arrow = Arrow

  occurs :: Int -> BasicType -> Bool
  occurs v t =
    case t of
      TypeVar v'  -> v == v'
      Arrow t1 t2 -> occurs v t1 || occurs v t2

  unify :: BasicType -> BasicType -> Maybe (Substitution BasicType)
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
