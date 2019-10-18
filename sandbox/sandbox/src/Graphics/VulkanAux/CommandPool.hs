module Graphics.VulkanAux.CommandPool where

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaCommandPoolResource :: VkDevice -> VkaResource VkCommandPoolCreateInfo VkCommandPool
vkaCommandPoolResource = vkaSimpleParamResource_ vkCreateCommandPool vkDestroyCommandPool "vkCreateCommandPool"

initStandardCommandPoolCreateInfo :: CreateVkStruct VkCommandPoolCreateInfo '["sType", "pNext"] ()
initStandardCommandPoolCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO &*
  set @"pNext" VK_NULL
