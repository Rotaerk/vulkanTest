{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Data.Array.MArray.Local (
  module Data.Array.MArray,
  produceAssocs,
  produceElems,
  listAssocs,
  listElems
) where

import Data.Array.Base
import Data.Array.MArray
import Pipes
import qualified Pipes.Prelude as P

listAssocs :: forall a i e m. (Monad m, MArray a e m, Ix i) => a i e -> ListT m (i, e)
listAssocs array = do
  bounds <- lift $ getBounds array
  let unsafeRead' = unsafeRead array . index bounds
  i <- Select . each $ range bounds
  lift $ (i,) <$> unsafeRead' i

produceAssocs :: forall a i e m. (Monad m, MArray a e m, Ix i) => a i e -> Producer (i, e) m ()
produceAssocs = enumerate . listAssocs

listElems :: forall a i e m. (Monad m, MArray a e m, Ix i) => a i e -> ListT m e
listElems = fmap snd . listAssocs

produceElems :: forall a i e m. (Monad m, MArray a e m, Ix i) => a i e -> Producer e m ()
produceElems = enumerate . listElems
