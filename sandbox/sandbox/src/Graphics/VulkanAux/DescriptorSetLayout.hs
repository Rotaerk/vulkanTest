module Graphics.VulkanAux.DescriptorSetLayout where

import Data.Reflection
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaDescriptorSetLayoutResource :: Given VkDevice => VkaResource VkDescriptorSetLayoutCreateInfo VkDescriptorSetLayout
vkaDescriptorSetLayoutResource = vkaSimpleParamResource_ vkCreateDescriptorSetLayout vkDestroyDescriptorSetLayout "vkCreateDescriptorSetLayout" given

initStandardDescriptorSetLayoutCreateInfo :: CreateVkStruct VkDescriptorSetLayoutCreateInfo '["sType", "pNext"] ()
initStandardDescriptorSetLayoutCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO &*
  set @"pNext" VK_NULL
