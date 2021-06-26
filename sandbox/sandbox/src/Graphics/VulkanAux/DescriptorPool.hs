module Graphics.VulkanAux.DescriptorPool where

import Data.Reflection
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaDescriptorPoolResource :: Given VkDevice => VkaResource VkDescriptorPoolCreateInfo VkDescriptorPool
vkaDescriptorPoolResource = vkaSimpleParamResource_ vkCreateDescriptorPool vkDestroyDescriptorPool "vkCreateDescriptorPool" given

initStandardDescriptorPoolCreateInfo :: CreateVkStruct VkDescriptorPoolCreateInfo '["sType", "pNext"] ()
initStandardDescriptorPoolCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO &*
  set @"pNext" VK_NULL
