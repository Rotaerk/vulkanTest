module Graphics.VulkanAux.RenderPass where

import Data.Bits.Local
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaRenderPassResource :: VkDevice -> VkaResource VkRenderPassCreateInfo VkRenderPass
vkaRenderPassResource = vkaSimpleParamResource_ vkCreateRenderPass vkDestroyRenderPass "vkCreateRenderPass"

initStandardRenderPassCreateInfo :: CreateVkStruct VkRenderPassCreateInfo '["sType", "pNext", "flags"] ()
initStandardRenderPassCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" zeroBits
