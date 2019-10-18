module Graphics.VulkanAux.Fence where

import Prelude.Local

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource.Local
import Data.Bits.Local
import Data.Function
import Foreign.Marshal.Array
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Exception
import Graphics.VulkanAux.Resource

vkaFenceResource :: VkDevice -> VkaResource VkFenceCreateInfo VkFence
vkaFenceResource = vkaSimpleParamResource_ vkCreateFence vkDestroyFence "vkCreateFence"

initStandardFenceCreateInfo :: CreateVkStruct VkFenceCreateInfo '["sType", "pNext"] ()
initStandardFenceCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO &*
  set @"pNext" VK_NULL

setFenceSignaled :: Bool -> CreateVkStruct VkFenceCreateInfo '["flags"] ()
setFenceSignaled isSignaled = set @"flags" (if isSignaled then VK_FENCE_CREATE_SIGNALED_BIT else zeroBits)

vkaCreateFence :: MonadIO m => VkDevice -> Bool -> ResourceT m VkFence
vkaCreateFence device signaled = vkaAllocateResource_ (vkaFenceResource device) $ createVk $ initStandardFenceCreateInfo &* setFenceSignaled signaled

vkaWaitForFences :: VkDevice -> [VkFence] -> VkBool32 -> Word64 -> IO VkResult
vkaWaitForFences device fences waitAll timeout =
  withArray fences $ \fencesPtr ->
  vkWaitForFences device (lengthNum fences) fencesPtr waitAll timeout & onVkFailureThrow "vkWaitForFences" [VK_SUCCESS, VK_TIMEOUT]

vkaWaitForFence :: VkDevice -> VkFence -> Word64 -> IO VkResult
vkaWaitForFence device fence timeout = vkaWaitForFences device [fence] VK_TRUE timeout

vkaResetFences :: VkDevice -> [VkFence] -> IO ()
vkaResetFences device fences =
  withArray fences $ \fencesPtr ->
  vkResetFences device (lengthNum fences) fencesPtr & onVkFailureThrow_ "vkResetFences"

vkaResetFence :: VkDevice -> VkFence -> IO ()
vkaResetFence device fence = vkaResetFences device [fence]
