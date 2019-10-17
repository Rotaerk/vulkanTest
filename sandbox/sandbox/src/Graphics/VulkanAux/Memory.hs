module Graphics.VulkanAux.Memory where

import Prelude.Local

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource.Local
import Data.Acquire.Local
import Data.Bits.Local
import Data.Function
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Local
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Exception
import Graphics.VulkanAux.Getter
import Graphics.VulkanAux.Resource

vkaAllocatedMemoryResource :: VkDevice -> VkaResource VkMemoryAllocateInfo VkDeviceMemory
vkaAllocatedMemoryResource = simpleParamVkaResource_ vkAllocateMemory vkFreeMemory "vkAllocateMemory"

initStandardMemoryAllocateInfo :: CreateVkStruct VkMemoryAllocateInfo '["sType", "pNext"] ()
initStandardMemoryAllocateInfo =
  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO &*
  set @"pNext" VK_NULL

vkaMapMemory :: VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> VkMemoryMapFlags -> IO (Ptr Void)
vkaMapMemory device deviceMemory offset size flags =
  alloca $ \ptrPtr -> do
    vkMapMemory device deviceMemory offset size flags ptrPtr & onVkFailureThrow_ "vkMapMemory"
    peek ptrPtr

vkaMappedMemory :: VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> Acquire (Ptr Void)
vkaMappedMemory device deviceMemory offset size =
  vkaMapMemory device deviceMemory offset size zeroBits
  `mkAcquire`
  const (vkUnmapMemory device deviceMemory)

vkaAllocateAndBindVulkanMemory ::
  MonadUnliftIO io =>
  (VkDevice -> obj -> Ptr VkMemoryRequirements -> IO ()) ->
  (VkDevice -> obj -> VkDeviceMemory -> VkDeviceSize -> IO ()) ->
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  obj ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe VkDeviceMemory)
vkaAllocateAndBindVulkanMemory getMemoryRequirements bindMemory device pdmp obj qualification =
  vkaGet_ (getMemoryRequirements device obj) >>= \memReqs ->
  lift (
    pickByM qualification .
    filter (testBit (getField @"memoryTypeBits" memReqs) . fst) .
    take (fromIntegral $ getField @"memoryTypeCount" pdmp) $
    getFieldArrayAssocs @"memoryTypes" pdmp
  ) >>=
    mapM (\(chosenMemoryTypeIndex, _) -> do
      memory <-
        allocateAcquireVk_ (vkaAllocatedMemoryResource device) $
        createVk $
        initStandardMemoryAllocateInfo &*
        set @"allocationSize" (getField @"size" memReqs) &*
        set @"memoryTypeIndex" (fromIntegral chosenMemoryTypeIndex)
      liftIO $ bindMemory device obj memory 0
      return memory
    )

