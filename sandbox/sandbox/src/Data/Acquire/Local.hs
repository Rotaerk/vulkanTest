module Data.Acquire.Local (
  module Data.Acquire,
  allocateAcquire_,
  with_
) where

import Control.Monad.Trans.Resource
import Data.Acquire

allocateAcquire_ :: MonadResource m => Acquire a -> m a
allocateAcquire_ = fmap snd . allocateAcquire

with_ :: MonadUnliftIO m => Acquire a -> m b -> m b
with_ a m = with a $ const m
