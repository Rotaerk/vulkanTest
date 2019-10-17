module Graphics.VulkanAux.Sampler where

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaSamplerResource :: VkDevice -> VkaResource VkSamplerCreateInfo VkSampler
vkaSamplerResource = simpleParamVkaResource_ vkCreateSampler vkDestroySampler "vkCreateSampler"

initStandardSamplerCreateInfo :: CreateVkStruct VkSamplerCreateInfo '["sType", "pNext"] ()
initStandardSamplerCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO &*
  set @"pNext" VK_NULL

