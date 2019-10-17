module Graphics.VulkanAux.Command where

import Prelude.Local

import Foreign.Marshal.Array
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

vkaCmdPipelineBarrier ::
  VkCommandBuffer ->
  VkPipelineStageFlags ->
  VkPipelineStageFlags ->
  VkDependencyFlags ->
  [VkMemoryBarrier] ->
  [VkBufferMemoryBarrier] ->
  [VkImageMemoryBarrier] ->
  IO ()
vkaCmdPipelineBarrier
  commandBuffer
  srcStageMask
  dstStageMask
  depFlags
  memoryBarriers
  bufferMemoryBarriers
  imageMemoryBarriers
  =
  withArray memoryBarriers $ \memoryBarriersPtr ->
  withArray bufferMemoryBarriers $ \bufferMemoryBarriersPtr ->
  withArray imageMemoryBarriers $ \imageMemoryBarriersPtr ->
  vkCmdPipelineBarrier
    commandBuffer
    srcStageMask
    dstStageMask
    depFlags
    (lengthNum memoryBarriers)
    memoryBarriersPtr
    (lengthNum bufferMemoryBarriers)
    bufferMemoryBarriersPtr
    (lengthNum imageMemoryBarriers)
    imageMemoryBarriersPtr

initStandardImageMemoryBarrier :: CreateVkStruct VkImageMemoryBarrier '["sType", "pNext"] ()
initStandardImageMemoryBarrier =
  set @"sType" VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER &*
  set @"pNext" VK_NULL

vkaCmdCopyBufferToImage ::
  VkCommandBuffer ->
  VkBuffer ->
  VkImage ->
  VkImageLayout ->
  [VkBufferImageCopy] ->
  IO ()
vkaCmdCopyBufferToImage commandBuffer buffer image imageLayout bufferImageCopies =
  withArray bufferImageCopies $ \bufferImageCopiesPtr ->
  vkCmdCopyBufferToImage commandBuffer buffer image imageLayout (lengthNum bufferImageCopies) bufferImageCopiesPtr

vkaCmdBeginRenderPass :: VkCommandBuffer -> VkSubpassContents -> VkRenderPassBeginInfo -> IO ()
vkaCmdBeginRenderPass commandBuffer contents beginInfo =
  withPtr beginInfo $ \beginInfoPtr -> vkCmdBeginRenderPass commandBuffer beginInfoPtr contents

initStandardRenderPassBeginInfo :: CreateVkStruct VkRenderPassBeginInfo '["sType", "pNext"] ()
initStandardRenderPassBeginInfo =
  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO &*
  set @"pNext" VK_NULL

vkaCmdBindVertexBuffers :: VkCommandBuffer -> Word32 -> [(VkBuffer, VkDeviceSize)] -> IO ()
vkaCmdBindVertexBuffers commandBuffer firstBinding bindings =
  withArray (fst <$> bindings) $ \buffersPtr ->
  withArray (snd <$> bindings) $ \offsetsPtr ->
  vkCmdBindVertexBuffers commandBuffer firstBinding (lengthNum bindings) buffersPtr offsetsPtr

vkaCmdBindDescriptorSets :: VkCommandBuffer -> VkPipelineBindPoint -> VkPipelineLayout -> Word32 -> [VkDescriptorSet] -> [Word32] -> IO ()
vkaCmdBindDescriptorSets commandBuffer bindPoint pipelineLayout firstSet descriptorSets dynamicOffsets =
  withArray descriptorSets $ \descriptorSetsPtr ->
  withArray dynamicOffsets $ \dynamicOffsetsPtr ->
  vkCmdBindDescriptorSets commandBuffer bindPoint pipelineLayout firstSet (lengthNum descriptorSets) descriptorSetsPtr (lengthNum dynamicOffsets) dynamicOffsetsPtr

