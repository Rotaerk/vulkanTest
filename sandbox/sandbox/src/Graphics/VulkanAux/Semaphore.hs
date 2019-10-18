module Graphics.VulkanAux.Semaphore where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource.Local
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaSemaphoreResource :: VkDevice -> VkaResource VkSemaphoreCreateInfo VkSemaphore
vkaSemaphoreResource = vkaSimpleParamResource_ vkCreateSemaphore vkDestroySemaphore "vkCreateSemaphore"

initStandardSemaphoreCreateInfo :: CreateVkStruct VkSemaphoreCreateInfo '["sType", "pNext"] ()
initStandardSemaphoreCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO &*
  set @"pNext" VK_NULL

vkaCreateSemaphore :: MonadIO m => VkDevice -> ResourceT m VkSemaphore
vkaCreateSemaphore device = vkaAllocateResource_ (vkaSemaphoreResource device) $ createVk initStandardSemaphoreCreateInfo
