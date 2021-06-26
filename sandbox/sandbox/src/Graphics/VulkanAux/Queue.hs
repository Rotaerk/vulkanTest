module Graphics.VulkanAux.Queue where

import Prelude.Local

import Control.Monad.IO.Class
import Data.Function
import Data.Reflection
import Foreign.Marshal.Array
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.VulkanAux.Getter
import Graphics.VulkanAux.Exception

vkaQueueWaitIdle :: VkQueue -> IO ()
vkaQueueWaitIdle queue = vkQueueWaitIdle queue & onVkFailureThrow_ "vkQueueWaitIdle"

vkaQueueSubmit :: VkQueue -> VkFence -> [VkSubmitInfo] -> IO ()
vkaQueueSubmit queue fence submitInfos =
  withArray submitInfos $ \submitInfosPtr ->
  vkQueueSubmit queue (lengthNum submitInfos) submitInfosPtr fence & onVkFailureThrow_ "vkQueueSubmit"

initStandardSubmitInfo :: CreateVkStruct VkSubmitInfo '["sType", "pNext"] ()
initStandardSubmitInfo =
  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO &*
  set @"pNext" VK_NULL

vkaGetDeviceQueue :: (MonadIO io, Given VkDevice) => Word32 -> Word32 -> io VkQueue
vkaGetDeviceQueue = vkaGet_ .: vkGetDeviceQueue given

vkaQueuePresentKHR :: VkQueue -> VkPresentInfoKHR -> IO VkResult
vkaQueuePresentKHR queue presentInfo =
  withPtr presentInfo $ \presentInfoPtr ->
  vkQueuePresentKHR queue presentInfoPtr & onVkFailureThrow "vkQueuePresentKHR" [VK_SUCCESS, VK_SUBOPTIMAL_KHR]

initStandardPresentInfoKHR :: CreateVkStruct VkPresentInfoKHR '["sType", "pNext"] ()
initStandardPresentInfoKHR =
  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR &*
  set @"pNext" VK_NULL

setSubmitWaitSemaphoresAndStageFlags ::
  [(VkSemaphore, VkPipelineStageFlags)] ->
  CreateVkStruct VkSubmitInfo '["waitSemaphoreCount", "pWaitSemaphores", "pWaitDstStageMask"] ()
setSubmitWaitSemaphoresAndStageFlags waitSemaphoresAndStageFlags =
  setListCountAndRef @"waitSemaphoreCount" @"pWaitSemaphores" (fst <$> waitSemaphoresAndStageFlags) &*
  setListRef @"pWaitDstStageMask" (snd <$> waitSemaphoresAndStageFlags)
