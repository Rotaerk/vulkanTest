{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.VulkanAux.Swapchain where

import Prelude.Local

import Data.Function
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.ArrayFiller
import Graphics.VulkanAux.Getter
import Graphics.VulkanAux.Resource

vkaSwapchainResource :: VkDevice -> VkaResource VkSwapchainCreateInfoKHR VkSwapchainKHR
vkaSwapchainResource = simpleParamVkaResource_ vkCreateSwapchainKHR vkDestroySwapchainKHR "vkCreateSwapchainKHR"

initStandardSwapchainCreateInfo :: CreateVkStruct VkSwapchainCreateInfoKHR '["sType", "pNext"] ()
initStandardSwapchainCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR &*
  set @"pNext" VK_NULL

-- Generalize this and the similar function in Buffer.hs once the CreateVkStruct type is redesigned.
setImageSharingQueueFamilyIndices ::
  (
    CanWriteField "imageSharingMode" a,
    CanWriteField "queueFamilyIndexCount" a,
    CanWriteField "pQueueFamilyIndices" a,
    FieldType "imageSharingMode" a ~ VkSharingMode,
    FieldType "queueFamilyIndexCount" a ~ Word32,
    FieldType "pQueueFamilyIndices" a ~ Ptr Word32
  ) =>
  [Word32] ->
  CreateVkStruct a '["imageSharingMode", "queueFamilyIndexCount", "pQueueFamilyIndices"] ()
setImageSharingQueueFamilyIndices qfis =
  set @"imageSharingMode" (if null qfis' then VK_SHARING_MODE_EXCLUSIVE else VK_SHARING_MODE_CONCURRENT) &*
  setListCountAndRef @"queueFamilyIndexCount" @"pQueueFamilyIndices" qfis'
  where
    -- If just one QFI is provided, it's the same as providing none; both are exclusive mode,
    -- and the QFI list is ignored in that case.
    qfis' = if length qfis > 1 then qfis else []

vkaAcquireNextImageKHR :: VkDevice -> VkSwapchainKHR -> Word64 -> VkSemaphore -> VkFence -> VkaGetter Word32 VkResult
vkaAcquireNextImageKHR device swapchain timeout semaphore fence =
  vkAcquireNextImageKHR device swapchain timeout semaphore fence &
  onGetterFailureThrow "vkAcquireNextImageKHR" [VK_SUCCESS, VK_TIMEOUT, VK_NOT_READY, VK_SUBOPTIMAL_KHR]

vkaGetSwapchainImagesKHR :: VkDevice -> VkSwapchainKHR -> VkaArrayFiller VkImage VkResult
vkaGetSwapchainImagesKHR = onArrayFillerFailureThrow "vkGetSwapchainImagesKHR" [VK_SUCCESS] .: vkGetSwapchainImagesKHR

