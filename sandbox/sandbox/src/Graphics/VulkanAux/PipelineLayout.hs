module Graphics.VulkanAux.PipelineLayout where

import Data.Bits.Local
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaPipelineLayoutResource :: VkDevice -> VkaResource VkPipelineLayoutCreateInfo VkPipelineLayout
vkaPipelineLayoutResource = simpleParamVkaResource_ vkCreatePipelineLayout vkDestroyPipelineLayout "vkCreatePipelineLayout"

initStandardPipelineLayoutCreateInfo :: CreateVkStruct VkPipelineLayoutCreateInfo '["sType", "pNext", "flags"] ()
initStandardPipelineLayoutCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" zeroBits
