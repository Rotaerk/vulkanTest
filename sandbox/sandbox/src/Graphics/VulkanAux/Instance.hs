module Graphics.VulkanAux.Instance where

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource

vkaInstanceResource :: VkaResource VkInstanceCreateInfo VkInstance
vkaInstanceResource = vkaSimpleResource_ vkCreateInstance vkDestroyInstance "vkCreateInstance"

initStandardInstanceCreateInfo :: CreateVkStruct VkInstanceCreateInfo '["sType", "pNext"] ()
initStandardInstanceCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardApplicationInfo :: CreateVkStruct VkApplicationInfo '["sType", "pNext"] ()
initStandardApplicationInfo =
  set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO &*
  set @"pNext" VK_NULL

