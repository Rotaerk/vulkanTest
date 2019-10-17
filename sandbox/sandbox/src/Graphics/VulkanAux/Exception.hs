module Graphics.VulkanAux.Exception where

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Data.Function
import Graphics.Vulkan.Core_1_0

data VkResultException =
  VkResultException {
    vkaResultException'functionName :: String,
    vkaResultException'result :: VkResult
  } deriving (Eq, Show, Read)

instance Exception VkResultException where
  displayException (VkResultException functionName result) =
    functionName ++ " failed with: " ++ show result

onVkFailureThrow :: String -> [VkResult] -> IO VkResult -> IO VkResult
onVkFailureThrow functionName successResults vkAction = do
  result <- vkAction
  unless (result `elem` successResults) $ throwIO (VkResultException functionName result)
  return result

onVkFailureThrow_ :: String -> IO VkResult -> IO ()
onVkFailureThrow_ functionName vkAction = onVkFailureThrow functionName [VK_SUCCESS] vkAction & void

data VkaException = VkaException String deriving (Eq, Show, Read)

instance Exception VkaException where
  displayException (VkaException message) = "VkaException: " ++ message

throwVkaException :: String -> a
throwVkaException = throw . VkaException

throwVkaExceptionM :: MonadThrow m => String -> m a
throwVkaExceptionM = throwM . VkaException

