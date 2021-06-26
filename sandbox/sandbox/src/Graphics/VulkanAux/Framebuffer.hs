module Graphics.VulkanAux.Framebuffer where

import Data.Reflection
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaFramebufferResource :: Given VkDevice => VkaResource VkFramebufferCreateInfo VkFramebuffer
vkaFramebufferResource = vkaSimpleParamResource_ vkCreateFramebuffer vkDestroyFramebuffer "vkCreateFramebuffer" given

initStandardFramebufferCreateInfo :: CreateVkStruct VkFramebufferCreateInfo '["sType", "pNext"] ()
initStandardFramebufferCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO &*
  set @"pNext" VK_NULL
