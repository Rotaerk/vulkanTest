module Graphics.VulkanAux.Sampler where

import Data.Reflection
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaSamplerResource :: Given VkDevice => VkaResource VkSamplerCreateInfo VkSampler
vkaSamplerResource = vkaSimpleParamResource_ vkCreateSampler vkDestroySampler "vkCreateSampler" given

initStandardSamplerCreateInfo :: CreateVkStruct VkSamplerCreateInfo '["sType", "pNext"] ()
initStandardSamplerCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO &*
  set @"pNext" VK_NULL

