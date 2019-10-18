module Graphics.VulkanAux.PhysicalDevice where

import Prelude.Local

import Data.Bits
import Data.Function
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Marshal.Create.DataFrame
import Graphics.VulkanAux.ArrayFiller
import Graphics.VulkanAux.Getter
import Numeric.DataFrame
import Numeric.Dimensions

vkaGetPhysicalDeviceSurfaceSupportKHR :: VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> VkaGetter VkBool32 ()
vkaGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi surface =
  vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi surface & onGetterFailureThrow_ "vkGetPhysicalDeviceSurfaceSupportKHR"

vkaGetPhysicalDeviceSurfaceCapabilitiesKHR :: VkPhysicalDevice -> VkSurfaceKHR -> VkaGetter VkSurfaceCapabilitiesKHR ()
vkaGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface =
  vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface & onGetterFailureThrow_ "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"

vkaEnumeratePhysicalDevices :: VkInstance -> VkaArrayFiller VkPhysicalDevice VkResult
vkaEnumeratePhysicalDevices = onArrayFillerFailureThrow "vkEnumeratePhysicalDevices" [VK_SUCCESS] . vkEnumeratePhysicalDevices

vkaGetPhysicalDeviceSurfaceFormatsKHR :: VkPhysicalDevice -> VkSurfaceKHR -> VkaArrayFiller VkSurfaceFormatKHR VkResult
vkaGetPhysicalDeviceSurfaceFormatsKHR = onArrayFillerFailureThrow "vkGetPhysicalDeviceSurfaceFormatsKHR" [VK_SUCCESS] .: vkGetPhysicalDeviceSurfaceFormatsKHR

vkaGetPhysicalDeviceSurfacePresentModesKHR :: VkPhysicalDevice -> VkSurfaceKHR -> VkaArrayFiller VkPresentModeKHR VkResult
vkaGetPhysicalDeviceSurfacePresentModesKHR = onArrayFillerFailureThrow "vkGetPhysicalDeviceSurfacePresentModesKHR" [VK_SUCCESS] .: vkGetPhysicalDeviceSurfacePresentModesKHR

vkaGetPhysicalDeviceLocalMemorySize :: VkPhysicalDeviceMemoryProperties -> VkDeviceSize
vkaGetPhysicalDeviceLocalMemorySize memoryProperties =
  iwfoldr @VkMemoryHeap @'[_] @'[] (
    \(Idx i :* U) (S x) s ->
      if i < memoryHeapCount && isDeviceLocal x then s + getField @"size" x else s
  ) 0 .
  getVec @"memoryHeaps" $
  memoryProperties
  where
    isDeviceLocal = (zeroBits /=) . (VK_MEMORY_HEAP_DEVICE_LOCAL_BIT .&.) . getField @"flags"
    memoryHeapCount = fromIntegral $ getField @"memoryHeapCount" memoryProperties
