{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module MLTypes where
import Utilities (Substitutable (..), Substitution, Type (..), Variable, TypeSub)
import Data.Bifunctor (Bifunctor(second), first)
import ML (Expr (..))
import Control.Monad.Trans.State (StateT (StateT), State, get, put, modify, runState, runStateT)
import Data.Set (Set, empty, singleton, union, (\\), toList, filter, fold, notMember, member, insert)
import Foreign (free)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Class (lift)
import Data.List (maximumBy)
import Debug.Trace (trace)

data BasicType = TypeVar Int | TypeConst String | Arrow BasicType BasicType
data PolyType = BasicType BasicType | Forall Int PolyType
type Context = [(String, PolyType)]
-- | Has no free type variables.
type ConstTypeMap = [(String, PolyType)]

instance Show BasicType where
  show :: BasicType -> String
  show (TypeVar i)   = show i
  show (Arrow t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (TypeConst s) = s

instance Show PolyType where
  show :: PolyType -> String
  show (BasicType bt) = show bt
  show (Forall v pt)  = "forall " ++ show v ++ ". " ++ show pt

instance Substitutable BasicType BasicType where
  applySub :: Substitution BasicType -> BasicType -> BasicType
  applySub s = s

  substitute :: TypeSub BasicType -> BasicType -> BasicType
  substitute ts@(v, t1) t2
    = case t2 of
        TypeVar v'    -> if v == v' then t1 else t2
        c@(TypeConst t)   -> c
        Arrow t1' t2' -> Arrow (substitute ts t1') (substitute ts t2')

instance Substitutable BasicType PolyType where
  applySub :: Substitution BasicType -> PolyType -> PolyType
  applySub s (BasicType bt) = BasicType (applySub s bt)
  applySub s (Forall v pt)  = Forall v (applySub s pt)

  substitute :: TypeSub BasicType -> PolyType -> PolyType
  substitute ts t2 =
    case t2 of
      BasicType bt -> BasicType $ substitute ts bt
      Forall v pt -> Forall v (substitute ts pt)

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
      TypeConst _ -> False

  unify :: BasicType -> BasicType -> Maybe (Substitution BasicType)
  unify (TypeVar v) tv'@(TypeVar v')  = Just (substitute (v, tv'))
  unify (TypeVar v) c@(TypeConst _)       = Just (substitute (v, c))
  unify (TypeVar v) tv'
    | occurs v tv'                    = Nothing
    | otherwise                       = Just (substitute (v,tv'))
  unify t tv@(TypeVar _)              = unify tv t
  unify c@(TypeConst s) c'@(TypeConst s')
    | s == s'                         = Just id
    | otherwise                       = Nothing
  unify (Arrow t1 t2) (Arrow t1' t2') = do s1 <- unify t1 t1'
                                           s2 <- unify (s1 t2) (s1 t2')
                                           return $ s2 . s1
  unify _ _                           = Nothing

type WState = ReaderT ConstTypeMap (StateT Int Maybe) (Substitution BasicType, BasicType)
w :: Context -> Expr -> WState
w ctx expr =
  case expr of
    Const s    ->
      do ctm <- ask
         vc  <- lift' $ lookup s ctm
         n   <- lift get
         t   <- lift $ StateT (\s -> let (t', (n', _)) = runState (freshInstance vc) (n, empty) in Just (t', n'))
         return (id, t)
    Var v       ->
      do vt <- lift' $ lookup v ctx
         n  <- lift get
         t  <- lift $ StateT (\s -> let (t', (n', _)) = runState (freshInstance vt) (n, empty) in Just (t', n'))
         return (id, t)
    Abs v e     ->
      do n        <- lift get
         let next  = n + 1
         let fresh = TypeVar next
         lift $ put next
         (s, t)   <- w ((v, BasicType fresh) : ctx) e
         return (s, applySub s (Arrow fresh t))
    Let v e1 e2 ->
      do (s1, t1) <- w ctx e1
         let ctx'  = applySub s1 ctx
         let pt    = closure (BasicType t1) ctx'
         (s2, t2) <- w ((v, pt) : ctx') e2
         return (s2 . s1, t2)
    Fix v e     ->
      do n        <- lift get
         let next  = n + 1
         let fresh = TypeVar next
         lift $ put next
         (s1, t)  <- w ((v, BasicType fresh) : ctx) e
         s2       <- lift' $ unify (applySub s1 fresh) t
         return (s2 . s1, applySub s2 t)
    Appl e1 e2  ->
      do n        <- lift get
         let next  = n + 1
         let fresh = TypeVar next
         lift $ put next
         (s1, t1) <- w ctx e1
         (s2, t2) <- w (applySub s1 ctx) e2
         s3       <- lift' $ unify (applySub s2 t1) (Arrow t2 fresh)
         return (s3 . s2 . s1, applySub s3 fresh)
    where
      lift' :: Maybe a -> ReaderT r (StateT s Maybe) a
      lift' m = lift $ lift m

v :: String -> ConstTypeMap -> Maybe PolyType
v = lookup

closure :: PolyType -> Context -> PolyType
closure pt ctx = fold Forall pt freeInCtx
  where
    frees = freeTypeVariables pt
    freeInCtx = Data.Set.filter (not . flip occursInCtx ctx) frees

    occurs' :: Int -> PolyType -> Bool
    occurs' v (BasicType bt) = occurs v bt
    occurs' v (Forall v' pt) = (v /= v') && occurs' v pt

    occursInCtx :: Int -> Context -> Bool
    occursInCtx v = any (snd . fmap (occurs' v))

    freeTypeVariables :: PolyType -> Set Int
    freeTypeVariables (Forall v pt)  = freeTypeVariables pt \\ singleton v
    freeTypeVariables (BasicType bt) = ftvbt bt
      where
        ftvbt (TypeVar v)   = singleton v
        ftvbt (Arrow t1 t2) = ftvbt t1 `union` ftvbt t2
        ftvbt (TypeConst _)    = empty

freshInstance :: PolyType -> State (Int, Set Int) BasicType
freshInstance (Forall v pt)  = do modify (second (insert v))
                                  freshInstance pt
freshInstance (BasicType bt) = do s <- fi' bt
                                  return $ applySub s bt
  where
    fi' :: BasicType -> State (Int, Set Int) (Substitution BasicType)
    fi' (TypeVar v)  = do (n, sn) <- get
                          if v <= n && v `member` sn
                          then (do let next = n + 1
                                   modify (first (const next))
                                   return (substitute (v, TypeVar next)))
                          else return id
    fi' (Arrow t1 t2) = do s1 <- fi' t1
                           s2 <- fi' (applySub s1 t2)
                           return $ s2 . s1
    fi' (TypeConst _)    = return id

w' :: Context -> ConstTypeMap -> Expr -> Maybe ((Substitution BasicType, BasicType), Int)
w' ctx ctm e = runStateT (runReaderT (w ctx e) ctm) (foldr (flip maxNum . snd) 0 ctx)
  where
    maxNum :: Int -> PolyType -> Int
    maxNum n (Forall i pt)  = maxNum n pt
    maxNum n (BasicType bt) =
      case bt of
        TypeVar v   -> max v n
        TypeConst _    -> n
        Arrow t1 t2 -> max (maxNum n (BasicType t1)) (maxNum n (BasicType t2))

sampleCtm :: ConstTypeMap
sampleCtm = [
              ("Succ", BasicType (Arrow (TypeConst "Num") (TypeConst "Num"))),
              ("Pred", BasicType (Arrow (TypeConst "Num") (TypeConst "Num"))),
              ("IsZero", BasicType (Arrow (TypeConst "Num") (TypeConst "Bool")) ),
              ("Cond", Forall 1 (BasicType (Arrow (TypeConst "Bool") (Arrow (TypeConst "Num") (Arrow (TypeConst "Num") (TypeConst "Num"))))))
            ]

