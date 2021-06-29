module Graphics.VulkanAux.Fence where

import Prelude.Local

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource.Local
import Data.Bits.Local
import Data.Function
import Data.Reflection
import Foreign.Marshal.Array
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Exception
import Graphics.VulkanAux.Resource

vkaFenceResource :: Given VkDevice => VkaResource VkFenceCreateInfo VkFence
vkaFenceResource = vkaSimpleParamResource_ vkCreateFence vkDestroyFence "vkCreateFence" given

initStandardFenceCreateInfo :: CreateVkStruct VkFenceCreateInfo '["sType", "pNext"] ()
initStandardFenceCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO &*
  set @"pNext" VK_NULL

setFenceSignaled :: Bool -> CreateVkStruct VkFenceCreateInfo '["flags"] ()
setFenceSignaled isSignaled = set @"flags" (if isSignaled then VK_FENCE_CREATE_SIGNALED_BIT else zeroBits)

vkaCreateFence :: (MonadResource m, Given VkDevice) => Bool -> m VkFence
vkaCreateFence signaled = vkaAllocateResource_ vkaFenceResource $ createVk $ initStandardFenceCreateInfo &* setFenceSignaled signaled

vkaWaitForFences :: (MonadIO m, Given VkDevice) => [VkFence] -> VkBool32 -> Word64 -> m VkResult
vkaWaitForFences fences waitAll timeout =
  liftIO $ withArray fences $ \fencesPtr ->
  vkWaitForFences given (lengthNum fences) fencesPtr waitAll timeout & onVkFailureThrow "vkWaitForFences" [VK_SUCCESS, VK_TIMEOUT]

vkaWaitForFence :: (MonadIO m, Given VkDevice) => VkFence -> Word64 -> m VkResult
vkaWaitForFence fence timeout = liftIO $ vkaWaitForFences [fence] VK_TRUE timeout

vkaResetFences :: (MonadIO m, Given VkDevice) => [VkFence] -> m ()
vkaResetFences fences =
  liftIO $ withArray fences $ \fencesPtr ->
  vkResetFences given (lengthNum fences) fencesPtr & onVkFailureThrow_ "vkResetFences"

vkaResetFence :: (MonadIO m, Given VkDevice) => VkFence -> m ()
vkaResetFence fence = vkaResetFences [fence]
