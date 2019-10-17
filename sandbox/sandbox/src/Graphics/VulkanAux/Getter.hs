module Graphics.VulkanAux.Getter where

import Control.Monad.IO.Class
import Data.Function
import Data.Functor
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan.Core_1_0
import Graphics.VulkanAux.Exception

type VkaGetter vk r = Ptr vk -> IO r

vkaGet :: (MonadIO io, Storable vk) => VkaGetter vk r -> io (r, vk)
vkaGet get =
  liftIO $
  alloca $ \ptr -> do
    result <- get ptr
    value <- peek ptr
    return (result, value)

vkaGet_ :: (MonadIO io, Storable vk) => VkaGetter vk r -> io vk
vkaGet_ = fmap snd . vkaGet

onGetterFailureThrow :: String -> [VkResult] -> VkaGetter vk VkResult -> VkaGetter vk VkResult
onGetterFailureThrow functionName successResults get ptr = get ptr & onVkFailureThrow functionName successResults

onGetterFailureThrow_ :: String -> VkaGetter vk VkResult -> VkaGetter vk ()
onGetterFailureThrow_ functionName get ptr = onGetterFailureThrow functionName [VK_SUCCESS] get ptr & void
