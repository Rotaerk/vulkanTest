module Graphics.VulkanAux.Semaphore where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource.Local
import Data.Reflection
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaSemaphoreResource :: Given VkDevice => VkaResource VkSemaphoreCreateInfo VkSemaphore
vkaSemaphoreResource = vkaSimpleParamResource_ vkCreateSemaphore vkDestroySemaphore "vkCreateSemaphore" given

initStandardSemaphoreCreateInfo :: CreateVkStruct VkSemaphoreCreateInfo '["sType", "pNext"] ()
initStandardSemaphoreCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO &*
  set @"pNext" VK_NULL

vkaCreateSemaphore :: (MonadIO m, Given VkDevice) => ResourceT m VkSemaphore
vkaCreateSemaphore = vkaAllocateResource_ vkaSemaphoreResource $ createVk initStandardSemaphoreCreateInfo
