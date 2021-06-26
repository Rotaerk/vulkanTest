{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.VulkanAux.Buffer where

import Prelude.Local

import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource.Local
import Data.Acquire.Local
import Data.Bits.Local
import Data.Function
import Data.Maybe
import Data.Reflection
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.CommandBuffer
import Graphics.VulkanAux.Getter
import Graphics.VulkanAux.Exception
import Graphics.VulkanAux.Memory
import Graphics.VulkanAux.Resource
import Numeric.DataFrame

vkaBufferResource :: Given VkDevice => VkaResource VkBufferCreateInfo VkBuffer
vkaBufferResource = vkaSimpleParamResource_ vkCreateBuffer vkDestroyBuffer "vkCreateBuffer" given

initStandardBufferCreateInfo :: CreateVkStruct VkBufferCreateInfo '["sType", "pNext"] ()
initStandardBufferCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO &*
  set @"pNext" VK_NULL

vkaCmdCopyBuffer ::
  VkCommandBuffer ->
  VkBuffer ->
  VkBuffer ->
  [VkBufferCopy] ->
  IO ()
vkaCmdCopyBuffer commandBuffer srcBuffer dstBuffer bufferCopies =
  withArray bufferCopies $ \bufferCopiesPtr ->
  vkCmdCopyBuffer commandBuffer srcBuffer dstBuffer (lengthNum bufferCopies) bufferCopiesPtr

vkaCreateBoundBuffer ::
  (MonadUnliftIO m, MonadThrow m, Given VkDevice) =>
  VkPhysicalDeviceMemoryProperties ->
  QualificationM m (Int, VkMemoryType) ->
  VkBufferCreateInfo ->
  ResourceT m (VkBuffer, VkDeviceMemory)
vkaCreateBoundBuffer pdmp qualification bufferCreateInfo = runResourceT $ do
  (bufferReleaseKey, buffer) <- vkaAllocateResource vkaBufferResource bufferCreateInfo
  memory <-
    lift $
    fromMaybeM (throwVkaExceptionM "Failed to find a suitable memory type for the buffer.") $
    vkaAllocateAndBindBufferMemory pdmp buffer qualification
  -- To force the buffer to be freed before the memory it's bound to.
  lift . register_ =<< fromJust <$> unprotect bufferReleaseKey
  return (buffer, memory)

vkaCreateStagingBuffer ::
  (MonadUnliftIO m, MonadThrow m, Given VkDevice) =>
  VkPhysicalDeviceMemoryProperties ->
  [Word32] ->
  VkDeviceSize ->
  ResourceT m (VkBuffer, VkDeviceMemory)
vkaCreateStagingBuffer pdmp qfis size =
  vkaCreateBoundBuffer pdmp (
    return . allAreSet (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) . getField @"propertyFlags" . snd,
    \_ _ -> return EQ
  ) $
  createVk $
  initStandardBufferCreateInfo &*
  set @"flags" zeroBits &*
  set @"size" size &*
  set @"usage" VK_BUFFER_USAGE_TRANSFER_SRC_BIT &*
  setSharingQueueFamilyIndices qfis

vkaCreateUniformBuffer ::
  (MonadUnliftIO m, MonadThrow m, Given VkDevice) =>
  VkPhysicalDeviceMemoryProperties ->
  [Word32] ->
  VkDeviceSize ->
  ResourceT m (VkBuffer, VkDeviceMemory)
vkaCreateUniformBuffer pdmp qfis size =
  vkaCreateBoundBuffer pdmp (
    return . allAreSet (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) . getField @"propertyFlags" . snd,
    \_ _ -> return EQ
  ) $
  createVk $
  initStandardBufferCreateInfo &*
  set @"flags" zeroBits &*
  set @"size" size &*
  set @"usage" VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT &*
  setSharingQueueFamilyIndices qfis

vkaCreateUniformBufferForPrimBytes ::
  forall pb m.
  (MonadUnliftIO m, MonadThrow m, PrimBytes pb, Given VkDevice) =>
  VkPhysicalDeviceMemoryProperties ->
  [Word32] ->
  ResourceT m (VkBuffer, VkDeviceMemory)
vkaCreateUniformBufferForPrimBytes pdmp qfis = vkaCreateUniformBuffer pdmp qfis (bSizeOf @pb undefined)

vkaCreateFilledBuffer ::
  (MonadUnliftIO m, MonadThrow m, MonadFail m, Given VkDevice) =>
  VkPhysicalDeviceMemoryProperties ->
  VkCommandPool ->
  VkQueue ->
  VkBufferUsageFlags ->
  [Word32] ->
  VkDeviceSize ->
  (Ptr Void -> IO ()) ->
  ResourceT m (VkBuffer, VkDeviceMemory)
vkaCreateFilledBuffer pdmp commandPool queue usageFlags qfis dataSize fillBuffer = runResourceT $ do
  (stagingBuffer, stagingBufferMemory) <- vkaCreateStagingBuffer pdmp [] dataSize

  liftIO $ with (vkaMappedMemory stagingBufferMemory 0 dataSize) fillBuffer

  (buffer, bufferMemory) <-
    lift $ vkaCreateBoundBuffer pdmp (
      return . allAreSet VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT . getField @"propertyFlags" . snd,
      \_ _ -> return EQ
    ) $
    createVk $
    initStandardBufferCreateInfo &*
    set @"flags" zeroBits &*
    set @"size" dataSize &*
    set @"usage" (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. usageFlags) &*
    setSharingQueueFamilyIndices qfis

  vkaExecuteCommands commandPool queue $ \commandBuffer -> liftIO $
    vkaCmdCopyBuffer commandBuffer stagingBuffer buffer [createVk $ set @"size" dataSize &* set @"srcOffset" 0 &* set @"dstOffset" 0]

  return (buffer, bufferMemory)

vkaCreateFilledBufferFromPrimBytes ::
  (MonadUnliftIO m, MonadThrow m, MonadFail m, Storable pb, PrimBytes pb, Given VkDevice) =>
  VkPhysicalDeviceMemoryProperties ->
  VkCommandPool ->
  VkQueue ->
  VkBufferUsageFlags ->
  [Word32] ->
  pb ->
  ResourceT m (VkBuffer, VkDeviceMemory)
vkaCreateFilledBufferFromPrimBytes pdmp commandPool queue usageFlags qfis pb =
  vkaCreateFilledBuffer pdmp commandPool queue usageFlags qfis (bSizeOf pb) (flip poke pb . castPtr)

vkaGetBufferMemoryRequirements :: (MonadIO io, Given VkDevice) => VkBuffer -> io VkMemoryRequirements
vkaGetBufferMemoryRequirements = vkaGet_ . vkGetBufferMemoryRequirements given

vkaAllocateAndBindBufferMemory ::
  (MonadUnliftIO io, Given VkDevice) =>
  VkPhysicalDeviceMemoryProperties ->
  VkBuffer ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe VkDeviceMemory)
vkaAllocateAndBindBufferMemory = vkaAllocateAndBindVulkanMemory vkaGetBufferMemoryRequirements vkaBindBufferMemory

vkaBindBufferMemory :: Given VkDevice => VkBuffer -> VkDeviceMemory -> VkDeviceSize -> IO ()
vkaBindBufferMemory buffer memory memoryOffset =
  vkBindBufferMemory given buffer memory memoryOffset & onVkFailureThrow_ "vkBindBufferMemory"

setSharingQueueFamilyIndices ::
  (
    CanWriteField "sharingMode" a,
    CanWriteField "queueFamilyIndexCount" a,
    CanWriteField "pQueueFamilyIndices" a,
    FieldType "sharingMode" a ~ VkSharingMode,
    FieldType "queueFamilyIndexCount" a ~ Word32,
    FieldType "pQueueFamilyIndices" a ~ Ptr Word32
  ) =>
  [Word32] ->
  CreateVkStruct a '["sharingMode", "queueFamilyIndexCount", "pQueueFamilyIndices"] ()
setSharingQueueFamilyIndices qfis =
  set @"sharingMode" (if null qfis' then VK_SHARING_MODE_EXCLUSIVE else VK_SHARING_MODE_CONCURRENT) &*
  setListCountAndRef @"queueFamilyIndexCount" @"pQueueFamilyIndices" qfis'
  where
    -- If just one QFI is provided, it's the same as providing none; both are exclusive mode,
    -- and the QFI list is ignored in that case.
    qfis' = if length qfis > 1 then qfis else []
