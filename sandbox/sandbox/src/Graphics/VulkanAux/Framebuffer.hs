module Graphics.VulkanAux.Framebuffer where

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaFramebufferResource :: VkDevice -> VkaResource VkFramebufferCreateInfo VkFramebuffer
vkaFramebufferResource = simpleParamVkaResource_ vkCreateFramebuffer vkDestroyFramebuffer "vkCreateFramebuffer"

initStandardFramebufferCreateInfo :: CreateVkStruct VkFramebufferCreateInfo '["sType", "pNext"] ()
initStandardFramebufferCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO &*
  set @"pNext" VK_NULL
