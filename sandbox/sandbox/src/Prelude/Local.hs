{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prelude.Local where

import Control.Monad
import Control.Monad.Loops
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Maybe
import Safe.Foldable
  
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

type Predicate a = a -> Bool
type Comparison a = a -> a -> Ordering
type Qualification a = (Predicate a, Comparison a)
type Accum a = a -> a -> a
type ListAccum a = [a] -> a -> [a]

type PredicateM m a = a -> m Bool
type ComparisonM m a = a -> a -> m Ordering
type QualificationM m a = (PredicateM m a, ComparisonM m a)
type AccumM m a = a -> a -> m a
type ListAccumM m a = [a] -> a -> m [a]

ifLE :: a -> a -> Ordering -> a
ifLE a b = \case
  GT -> b
  _ -> a

ifGE :: a -> a -> Ordering -> a
ifGE a b = \case
  LT -> b
  _ -> a

minBy :: Comparison a -> Accum a
minBy cmp a b = cmp a b & ifLE a b

maxBy :: Comparison a -> Accum a
maxBy cmp a b = cmp a b & ifGE a b

minByM :: Functor m => ComparisonM m a -> AccumM m a
minByM cmpM a b = cmpM a b <&> ifLE a b

maxByM :: Functor m => ComparisonM m a -> AccumM m a
maxByM cmpM a b = cmpM a b <&> ifGE a b

keepIfLesser :: [a] -> a -> Ordering -> [a]
keepIfLesser as b = \case
  GT -> [b]
  EQ -> b:as
  LT -> as

keepIfGreater :: [a] -> a -> Ordering -> [a]
keepIfGreater as b = \case
  LT -> [b]
  EQ -> b:as
  GT -> as

minlBy :: Comparison a -> ListAccum a
minlBy _ [] b = [b]
minlBy cmp as b = (cmp . head) as b & keepIfLesser as b

maxlBy :: Comparison a -> ListAccum a
maxlBy _ [] b = [b]
maxlBy cmp as b = (cmp . head) as b & keepIfGreater as b

minlByM :: Applicative m => ComparisonM m a -> ListAccumM m a
minlByM _ [] b = pure [b]
minlByM cmpM as b = (cmpM . head) as b <&> keepIfLesser as b

maxlByM :: Applicative m => ComparisonM m a -> ListAccumM m a
maxlByM _ [] b = pure [b]
maxlByM cmpM as b = (cmpM . head) as b <&> keepIfGreater as b

maximalsBy :: forall a t. Foldable t => Comparison a -> t a -> [a]
maximalsBy cmp = foldl (maxlBy cmp) []

minimalsBy :: forall a t. Foldable t => Comparison a -> t a -> [a]
minimalsBy cmp = foldl (minlBy cmp) []

maximalsByM :: forall m a t. (Monad m, Foldable t) => ComparisonM m a -> t a -> m [a]
maximalsByM cmpM = foldM (maxlByM cmpM) []

minimalsByM :: forall m a t. (Monad m, Foldable t) => ComparisonM m a -> t a -> m [a]
minimalsByM cmpM = foldM (minlByM cmpM) []

pickBy :: Qualification a -> [a] -> Maybe a
pickBy (isQualified, compare) = maximumByMay compare . filter isQualified

picksBy :: Qualification a -> [a] -> [a]
picksBy (isQualified, compare) = maximalsBy compare . filter isQualified

pickByM :: Monad m => QualificationM m a -> [a] -> m (Maybe a)
pickByM (isQualifiedM, compareM) = maximumByM compareM <=< filterM isQualifiedM

picksByM :: Monad m => QualificationM m a -> [a] -> m [a]
picksByM (isQualifiedM, compareM) = maximalsByM compareM <=< filterM isQualifiedM

preferWhere :: [Predicate a] -> Comparison a
preferWhere [] _ _ = EQ
preferWhere (p:ps) a b
  | p a = if p b then EQ else GT
  | p b = LT
  | otherwise = preferWhere ps a b

prefer :: Eq a => [a] -> Comparison a
prefer = preferWhere . fmap (==)

preferWhereM :: Monad m => [PredicateM m a] -> ComparisonM m a
preferWhereM [] _ _ = return EQ
preferWhereM (p:ps) a b = do
  pa <- p a
  pb <- p b
  if pa then
    return $ if pb then EQ else GT
  else if pb then
    return LT
  else
    preferWhereM ps a b

headOr :: a -> [a] -> a
headOr a [] = a
headOr _ (h:_) = h
{-# INLINE headOr #-}

fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM a mb = fromMaybe a =<< fmap return <$> mb
{-# INLINE fromMaybeM #-}
