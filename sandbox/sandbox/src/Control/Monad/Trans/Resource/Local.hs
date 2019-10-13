module Control.Monad.Trans.Resource.Local (
  module Control.Monad.Trans.Resource,
  register_
) where

import Control.Monad.Trans.Resource
import Data.Functor

register_ :: MonadResource m => IO () -> m ()
register_ = void . register

