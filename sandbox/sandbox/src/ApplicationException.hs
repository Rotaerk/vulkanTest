module ApplicationException where

import Control.Exception
import Control.Monad.Catch

data ApplicationException = ApplicationException String deriving (Eq, Show, Read)

instance Exception ApplicationException where
  displayException (ApplicationException message) =
    "Application error: " ++ message

throwAppEx :: String -> a
throwAppEx message = throw $ ApplicationException message

throwAppExM :: MonadThrow m => String -> m a
throwAppExM message = throwM $ ApplicationException message

