module Graphics.VulkanAux.ImageView where

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaImageViewResource :: VkDevice -> VkaResource VkImageViewCreateInfo VkImageView
vkaImageViewResource = simpleParamVkaResource_ vkCreateImageView vkDestroyImageView "vkCreateImageView"

initStandardImageViewCreateInfo :: CreateVkStruct VkImageViewCreateInfo '["sType", "pNext"] ()
initStandardImageViewCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO &*
  set @"pNext" VK_NULL
