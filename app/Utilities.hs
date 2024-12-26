{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
module Utilities where
import Data.Bifunctor (bimap)

type Substitution a = a -> a
type TypeSub a = (Int, a)
{- |
Given a type a and a substitution a -> a, a substitution b -> b can be created.
b is the larger structure that has a as its elements.
Note that b can be a.
-}
class Substitutable a b | b -> a where
  applySub :: Substitution a -> b -> b
  substitute :: TypeSub a -> b -> b

{- |
Free instance for a context
-}
instance Substitutable a b => Substitutable a [(c, b)] where
  applySub :: Substitutable a b => Substitution a -> [(c, b)] -> [(c, b)]
  applySub s = map (fmap (applySub s))

  substitute :: Substitutable a b => TypeSub a -> [(c, b)] -> [(c, b)]
  substitute ts = map (fmap (substitute ts))

{- |
Free instance for the return of a principal pair algorithm.
Note that this is overlappable because some usages incidentally have the same shape.
-}
instance {-# OVERLAPPABLE #-} (Substitutable a b, Substitutable a c) => Substitutable a (c, b) where
  applySub :: (Substitutable a b, Substitutable a c) => Substitution a -> (c, b) -> (c, b)
  applySub s = bimap (applySub s) (applySub s)

  substitute :: (Substitutable a b, Substitutable a c) => TypeSub a -> (c, b) -> (c, b)
  substitute ts = bimap (substitute ts) (substitute ts)

type Variable = String
{- |
A type.
-}
class Substitutable a a => Type a where
  typeVar :: Int -> a
  arrow :: a -> a -> a
  occurs :: Int -> a -> Bool
  unify :: a -> a -> Maybe (Substitution a)
  unifyContexts :: [(Variable, a)] -> [(Variable, a)] -> Maybe (Substitution a)
  unifyContexts ((v1, t1) : c1s) c2s
    = maybe doesntOccur doesOccur (lookup v1 c2s)
      where
        doesntOccur  = unifyContexts c1s c2s
        doesOccur t2 = do s1 <- unify t1 t2
                          s2 <- unifyContexts (applySub s1 c1s) (applySub s1 c2s)
                          return $ s2 . s1
  unifyContexts [] c2s
    = Just id



