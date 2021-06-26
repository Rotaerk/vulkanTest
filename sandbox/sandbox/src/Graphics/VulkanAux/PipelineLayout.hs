module Graphics.VulkanAux.PipelineLayout where

import Data.Bits.Local
import Data.Reflection
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaPipelineLayoutResource :: Given VkDevice => VkaResource VkPipelineLayoutCreateInfo VkPipelineLayout
vkaPipelineLayoutResource = vkaSimpleParamResource_ vkCreatePipelineLayout vkDestroyPipelineLayout "vkCreatePipelineLayout" given

initStandardPipelineLayoutCreateInfo :: CreateVkStruct VkPipelineLayoutCreateInfo '["sType", "pNext", "flags"] ()
initStandardPipelineLayoutCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" zeroBits
