module Graphics.VulkanAux.ImageView where

import Data.Reflection
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaImageViewResource :: Given VkDevice => VkaResource VkImageViewCreateInfo VkImageView
vkaImageViewResource = vkaSimpleParamResource_ vkCreateImageView vkDestroyImageView "vkCreateImageView" given

initStandardImageViewCreateInfo :: CreateVkStruct VkImageViewCreateInfo '["sType", "pNext"] ()
initStandardImageViewCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO &*
  set @"pNext" VK_NULL
