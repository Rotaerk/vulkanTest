module Graphics.VulkanAux.Memory where

import Prelude.Local

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource.Local
import Data.Acquire.Local
import Data.Bits.Local
import Data.Function
import Data.Reflection
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Local
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Exception
import Graphics.VulkanAux.Resource

vkaAllocatedMemoryResource :: Given VkDevice => VkaResource VkMemoryAllocateInfo VkDeviceMemory
vkaAllocatedMemoryResource = vkaSimpleParamResource_ vkAllocateMemory vkFreeMemory "vkAllocateMemory" given

initStandardMemoryAllocateInfo :: CreateVkStruct VkMemoryAllocateInfo '["sType", "pNext"] ()
initStandardMemoryAllocateInfo =
  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO &*
  set @"pNext" VK_NULL

vkaMapMemory :: Given VkDevice => VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> VkMemoryMapFlags -> IO (Ptr Void)
vkaMapMemory deviceMemory offset size flags =
  alloca $ \ptrPtr -> do
    vkMapMemory given deviceMemory offset size flags ptrPtr & onVkFailureThrow_ "vkMapMemory"
    peek ptrPtr

vkaMappedMemory :: Given VkDevice => VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> Acquire (Ptr Void)
vkaMappedMemory deviceMemory offset size =
  vkaMapMemory deviceMemory offset size zeroBits
  `mkAcquire`
  const (vkUnmapMemory given deviceMemory)

vkaAllocateAndBindVulkanMemory ::
  (MonadUnliftIO io, Given VkDevice) =>
  (Given VkDevice => obj -> io VkMemoryRequirements) ->
  (Given VkDevice => obj -> VkDeviceMemory -> VkDeviceSize -> IO ()) ->
  VkPhysicalDeviceMemoryProperties ->
  obj ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe VkDeviceMemory)
vkaAllocateAndBindVulkanMemory getMemoryRequirements bindMemory pdmp obj qualification =
  lift (getMemoryRequirements obj) >>= \memReqs ->
  lift (
    pickByM qualification .
    filter (testBit (getField @"memoryTypeBits" memReqs) . fst) .
    take (fromIntegral $ getField @"memoryTypeCount" pdmp) $
    getFieldArrayAssocs @"memoryTypes" pdmp
  ) >>=
    mapM (\(chosenMemoryTypeIndex, _) -> do
      memory <-
        vkaAllocateResource_ vkaAllocatedMemoryResource $
        createVk $
        initStandardMemoryAllocateInfo &*
        set @"allocationSize" (getField @"size" memReqs) &*
        set @"memoryTypeIndex" (fromIntegral chosenMemoryTypeIndex)
      liftIO $ bindMemory obj memory 0
      return memory
    )

