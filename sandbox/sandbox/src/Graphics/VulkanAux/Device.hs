module Graphics.VulkanAux.Device where

import Control.Monad.IO.Class
import Data.Bits.Local
import Data.Function
import Data.Reflection
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Exception
import Graphics.VulkanAux.Resource

vkaDeviceResource :: VkPhysicalDevice -> VkaResource VkDeviceCreateInfo VkDevice
vkaDeviceResource = vkaSimpleParamResource_ vkCreateDevice (const vkDestroyDevice) "vkCreateDevice"

initStandardDeviceCreateInfo :: CreateVkStruct VkDeviceCreateInfo '["sType", "pNext", "flags", "enabledLayerCount", "ppEnabledLayerNames"] ()
initStandardDeviceCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" zeroBits &*
  set @"enabledLayerCount" 0 &*
  set @"ppEnabledLayerNames" VK_NULL

initStandardDeviceQueueCreateInfo :: CreateVkStruct VkDeviceQueueCreateInfo '["sType", "pNext"] ()
initStandardDeviceQueueCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO &*
  set @"pNext" VK_NULL

vkaDeviceWaitIdle :: (MonadIO io, Given VkDevice) => io ()
vkaDeviceWaitIdle = liftIO $ vkDeviceWaitIdle given & onVkFailureThrow_ "vkDeviceWaitIdle"
