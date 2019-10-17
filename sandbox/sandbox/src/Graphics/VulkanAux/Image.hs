module Graphics.VulkanAux.Image where

import Prelude.Local

import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource.Local
import Data.Function
import Data.Maybe
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Exception
import Graphics.VulkanAux.Memory
import Graphics.VulkanAux.Resource

vkaImageResource :: VkDevice -> VkaResource VkImageCreateInfo VkImage
vkaImageResource = simpleParamVkaResource_ vkCreateImage vkDestroyImage "vkCreateImage"

initStandardImageCreateInfo :: CreateVkStruct VkImageCreateInfo '["sType", "pNext"] ()
initStandardImageCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO &*
  set @"pNext" VK_NULL

vkaAllocateAndBindImageMemory ::
  MonadUnliftIO io =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  VkImage ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe VkDeviceMemory)
vkaAllocateAndBindImageMemory = vkaAllocateAndBindVulkanMemory vkGetImageMemoryRequirements vkaBindImageMemory

vkaBindImageMemory :: VkDevice -> VkImage -> VkDeviceMemory -> VkDeviceSize -> IO ()
vkaBindImageMemory device buffer memory memoryOffset =
  vkBindImageMemory device buffer memory memoryOffset & onVkFailureThrow_ "vkBindImageMemory"

vkaCreateBoundImage ::
  (MonadUnliftIO m, MonadThrow m) =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  QualificationM m (Int, VkMemoryType) ->
  VkImageCreateInfo ->
  ResourceT m (VkImage, VkDeviceMemory)
vkaCreateBoundImage device pdmp qualification imageCreateInfo = runResourceT $ do
  (imageReleaseKey, image) <- allocateAcquireVk (vkaImageResource device) imageCreateInfo
  memory <-
    lift $
    fromMaybeM (throwVkaExceptionM "Failed to find a suitable memory type for the image.") $
    vkaAllocateAndBindImageMemory device pdmp image qualification
  -- To force the image to be freed before the memory it's bound to.
  lift . register_ =<< fromJust <$> unprotect imageReleaseKey
  return (image, memory)

