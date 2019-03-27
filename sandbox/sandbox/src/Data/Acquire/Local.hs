module Data.Acquire.Local (
  module Data.Acquire,
  allocate_,
  allocateAcquire_,
  with_
) where

import Control.Monad.Trans.Resource
import Data.Acquire

allocate_ :: MonadResource m => IO a -> (a -> IO ()) -> m a
allocate_ acquire free = snd <$> allocate acquire free

allocateAcquire_ :: MonadResource m => Acquire a -> m a
allocateAcquire_ = (snd <$>) . allocateAcquire

with_ :: MonadUnliftIO m => Acquire a -> m b -> m b
with_ a m = with a $ const m
