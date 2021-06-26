module Graphics.VulkanAux.Image where

import Prelude.Local

import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource.Local
import Data.Function
import Data.Maybe
import Data.Reflection
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Exception
import Graphics.VulkanAux.Getter
import Graphics.VulkanAux.Memory
import Graphics.VulkanAux.Resource

vkaImageResource :: Given VkDevice => VkaResource VkImageCreateInfo VkImage
vkaImageResource = vkaSimpleParamResource_ vkCreateImage vkDestroyImage "vkCreateImage" given

initStandardImageCreateInfo :: CreateVkStruct VkImageCreateInfo '["sType", "pNext"] ()
initStandardImageCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO &*
  set @"pNext" VK_NULL

vkaGetImageMemoryRequirements :: (MonadIO io, Given VkDevice) => VkImage -> io VkMemoryRequirements
vkaGetImageMemoryRequirements = vkaGet_ . vkGetImageMemoryRequirements given

vkaAllocateAndBindImageMemory ::
  (MonadUnliftIO io, Given VkDevice) =>
  VkPhysicalDeviceMemoryProperties ->
  VkImage ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe VkDeviceMemory)
vkaAllocateAndBindImageMemory = vkaAllocateAndBindVulkanMemory vkaGetImageMemoryRequirements vkaBindImageMemory

vkaBindImageMemory :: Given VkDevice => VkImage -> VkDeviceMemory -> VkDeviceSize -> IO ()
vkaBindImageMemory buffer memory memoryOffset =
  vkBindImageMemory given buffer memory memoryOffset & onVkFailureThrow_ "vkBindImageMemory"

vkaCreateBoundImage ::
  (MonadUnliftIO m, MonadThrow m, Given VkDevice) =>
  VkPhysicalDeviceMemoryProperties ->
  QualificationM m (Int, VkMemoryType) ->
  VkImageCreateInfo ->
  ResourceT m (VkImage, VkDeviceMemory)
vkaCreateBoundImage pdmp qualification imageCreateInfo = runResourceT $ do
  (imageReleaseKey, image) <- vkaAllocateResource vkaImageResource imageCreateInfo
  memory <-
    lift $
    fromMaybeM (throwVkaExceptionM "Failed to find a suitable memory type for the image.") $
    vkaAllocateAndBindImageMemory pdmp image qualification
  -- To force the image to be freed before the memory it's bound to.
  lift . register_ =<< fromJust <$> unprotect imageReleaseKey
  return (image, memory)

