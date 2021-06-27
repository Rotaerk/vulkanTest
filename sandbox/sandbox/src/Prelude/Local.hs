{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prelude.Local where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.Bool
import Data.Function
import Data.Functor
import Data.List
import Safe.Foldable
  
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 9 .:

(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) = (.:) . (.)
infixr 9 .:.

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

pickFrom :: [a] -> Qualification a -> Maybe a
pickFrom = flip pickBy

selectionBy :: Traversable t => t (Qualification a) -> [a] -> Maybe (t a)
selectionBy = flip selectionFrom

selectionFrom :: Traversable t => [a] -> t (Qualification a) -> Maybe (t a)
selectionFrom = traverse . pickFrom

picksBy :: Qualification a -> [a] -> [a]
picksBy (isQualified, compare) = maximalsBy compare . filter isQualified

picksFrom :: [a] -> Qualification a -> [a]
picksFrom = flip picksBy

selectionsBy :: Traversable t => t (Qualification a) -> [a] -> [t a]
selectionsBy = flip selectionsFrom

selectionsFrom :: Traversable t => [a] -> t (Qualification a) -> [t a]
selectionsFrom = traverse . picksFrom

pickByM :: Monad m => QualificationM m a -> [a] -> m (Maybe a)
pickByM (isQualifiedM, compareM) = maximumByM compareM <=< filterM isQualifiedM

pickFromM :: Monad m => [a] -> QualificationM m a -> m (Maybe a)
pickFromM = flip pickByM

selectionByM :: (Traversable t, Monad m) => t (QualificationM m a) -> [a] -> m (Maybe (t a))
selectionByM = flip selectionFromM

selectionFromM :: (Traversable t, Monad m) => [a] -> t (QualificationM m a) -> m (Maybe (t a))
selectionFromM as = fmap sequence . traverse (pickFromM as)

picksByM :: Monad m => QualificationM m a -> [a] -> m [a]
picksByM (isQualifiedM, compareM) = maximalsByM compareM <=< filterM isQualifiedM

picksFromM :: Monad m => [a] -> QualificationM m a -> m [a]
picksFromM = flip picksByM

selectionsByM :: (Traversable t, Monad m) => t (QualificationM m a) -> [a] -> m [t a]
selectionsByM = flip selectionsFromM

selectionsFromM :: (Traversable t, Monad m) => [a] -> t (QualificationM m a) -> m [t a]
selectionsFromM as = fmap sequence . traverse (picksFromM as)

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

ioPutStrLn :: MonadIO io => String -> io ()
ioPutStrLn = liftIO . putStrLn

replace :: Eq a => a -> a -> a -> a
replace match replacement value | value == match = replacement
replace _ _ value = value

clamp :: Ord a => a -> a -> a -> a
clamp a b =
  case compare a b of
    LT -> min b . max a
    GT -> min a . max b
    EQ -> const a

alignTo :: Integral n => n -> n -> n
alignTo b n =
  case n `rem` b of
    0 -> n
    x -> n + b - x

assertM_ :: Monad m => Bool -> m ()
assertM_ cond = assert cond (return ())

assertPred :: (a -> Bool) -> a -> a
assertPred p a = assert (p a) a

doWhileM :: Monad m => m Bool -> m ()
doWhileM a = fix $ \loop -> a >>= bool (return ()) loop

lengthNum :: (Foldable t, Num n) => t a -> n
lengthNum = fromIntegral . length
{-# INLINE lengthNum #-}

groupValuesByKeyWith :: (k -> k -> Ordering) -> (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupValuesByKeyWith compareKeys getKey getValue =
  fmap buildGroup . groupBy (keysEqual `on` getKey) . sortBy (compareKeys `on` getKey)
  where
    keysEqual = (EQ ==) .: compareKeys
    buildGroup as = (getKey (head as), fmap getValue as)

groupValuesByKey :: Ord k => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupValuesByKey = groupValuesByKeyWith compare

groupByKey :: Ord k => (a -> k) -> [a] -> [(k, [a])]
groupByKey getKey = groupValuesByKey getKey id
