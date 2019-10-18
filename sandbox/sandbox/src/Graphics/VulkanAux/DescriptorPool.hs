module Graphics.VulkanAux.DescriptorPool where

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaDescriptorPoolResource :: VkDevice -> VkaResource VkDescriptorPoolCreateInfo VkDescriptorPool
vkaDescriptorPoolResource = simpleParamVkaResource_ vkCreateDescriptorPool vkDestroyDescriptorPool "vkCreateDescriptorPool"

initStandardDescriptorPoolCreateInfo :: CreateVkStruct VkDescriptorPoolCreateInfo '["sType", "pNext"] ()
initStandardDescriptorPoolCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO &*
  set @"pNext" VK_NULL

