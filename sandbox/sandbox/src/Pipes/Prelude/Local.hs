{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Pipes.Prelude.Local (
  module Pipes.Prelude,
  concatWith,
  minimumBy,
  maximumBy,
  concatWithM,
  minimumByM,
  maximumByM,
  minimalsBy,
  maximalsBy,
  minimalsByM,
  maximalsByM,
  pickBy,
  picksBy,
  pickByM,
  picksByM
) where

import Prelude hiding (filter, fold, map)
import qualified Prelude.Local as Prelude
import Prelude.Local (Comparison, ComparisonM, Accum, AccumM, Qualification, QualificationM, minBy, maxBy, minByM, maxByM, minlBy, maxlBy, minlByM, maxlByM)

import Control.Applicative
import Data.Function
import Data.Functor
import Pipes
import Pipes.Prelude

concatWith :: forall m a. Monad m => Accum a -> Producer a m () -> m (Maybe a)
concatWith binOp = fold step Nothing id
  where
    step ma (pure -> mb) = (binOp <$> ma <*> mb) <|> mb

minimumBy :: forall m a. Monad m => Comparison a -> Producer a m () -> m (Maybe a)
minimumBy cmp = concatWith (minBy cmp)

maximumBy :: forall m a. Monad m => Comparison a -> Producer a m () -> m (Maybe a)
maximumBy cmp = concatWith (maxBy cmp)

concatWithM :: forall m a. Monad m => AccumM m a -> Producer a m () -> m (Maybe a)
concatWithM binOpM = foldM stepM (pure Nothing) pure
  where
    stepM ma (pure -> mb) = Prelude.sequence $ (binOpM <$> ma <*> mb) <|> fmap pure mb

minimumByM :: forall m a. Monad m => ComparisonM m a -> Producer a m () -> m (Maybe a)
minimumByM cmpM = concatWithM (minByM cmpM)

maximumByM :: forall m a. Monad m => ComparisonM m a -> Producer a m () -> m (Maybe a)
maximumByM cmpM = concatWithM (maxByM cmpM)

minimalsBy :: forall m a. Monad m => Comparison a -> Producer a m () -> m [a]
minimalsBy cmp = fold (minlBy cmp) [] id

maximalsBy :: forall m a. Monad m => Comparison a -> Producer a m () -> m [a]
maximalsBy cmp = fold (maxlBy cmp) [] id

minimalsByM :: forall m a. Monad m => ComparisonM m a -> Producer a m () -> m [a]
minimalsByM cmpM = foldM (minlByM cmpM) (pure []) pure

maximalsByM :: forall m a. Monad m => ComparisonM m a -> Producer a m () -> m [a]
maximalsByM cmpM = foldM (maxlByM cmpM) (pure []) pure

pickBy :: Monad m => Qualification a -> Producer a m () -> m (Maybe a)
pickBy (isQualified, compare) = maximumBy compare . (filter isQualified <-<)

picksBy :: Monad m => Qualification a -> Producer a m () -> m [a]
picksBy (isQualified, compare) = maximalsBy compare . (filter isQualified <-<)

pickByM :: Monad m => QualificationM m a -> Producer a m () -> m (Maybe a)
pickByM (isQualifiedM, compareM) = maximumByM compareM . (filterM isQualifiedM <-<)

picksByM :: Monad m => QualificationM m a -> Producer a m () -> m [a]
picksByM (isQualifiedM, compareM) = maximalsByM compareM . (filterM isQualifiedM <-<)
