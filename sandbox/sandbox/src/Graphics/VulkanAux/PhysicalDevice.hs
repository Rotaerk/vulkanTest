module Graphics.VulkanAux.PhysicalDevice where

import Prelude.Local

import Control.Monad.IO.Class
import Data.Bits
import Data.Function
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Marshal.Create.DataFrame
import Graphics.VulkanAux.Array
import Graphics.VulkanAux.ArrayFiller
import Graphics.VulkanAux.Getter
import Numeric.DataFrame
import Numeric.Dimensions

vkaGetPhysicalDeviceSurfaceSupportKHR :: VkPhysicalDevice -> VkSurfaceKHR -> Word32 -> VkaGetter VkBool32 ()
vkaGetPhysicalDeviceSurfaceSupportKHR physicalDevice surface qfi =
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

data QueueFamily =
  QueueFamily {
    queueFamily'index :: Word32,
    queueFamily'properties :: VkQueueFamilyProperties
  }

vkaGetPhysicalDeviceQueueFamilies :: MonadIO m => VkPhysicalDevice -> m [QueueFamily]
vkaGetPhysicalDeviceQueueFamilies physicalDevice =
  fmap (uncurry QueueFamily) . vkaAssocs <$> vkaGetArray_ (vkGetPhysicalDeviceQueueFamilyProperties physicalDevice)
