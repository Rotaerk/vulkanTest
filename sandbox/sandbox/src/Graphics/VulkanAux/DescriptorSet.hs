module Graphics.VulkanAux.DescriptorSet where

import Prelude.Local

import Data.Acquire.Local
import Data.Function
import Data.Reflection
import Foreign.Marshal.Array
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Array
import Graphics.VulkanAux.Exception

-- Warning: This will free the descriptor sets at the end of the ResourceT scope.  Only use this if the descriptor
-- pool was created with the VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag set.  If you don't have that
-- set, use vkaAllocateDescriptorSets instead.
vkaAcquireAllocatedDescriptorSets :: Given VkDevice => VkDescriptorSetAllocateInfo -> Acquire (VkaArray VkDescriptorSet)
vkaAcquireAllocatedDescriptorSets allocateInfo =
  if descriptorSetCount > 0 then
    vkaAcquireArray_ descriptorSetCount
      (\arrayPtr ->
        withPtr allocateInfo $ \allocateInfoPtr ->
          vkAllocateDescriptorSets given allocateInfoPtr arrayPtr & onVkFailureThrow_ "vkAllocateDescriptorSets"
      )
      (\arrayPtr ->
        vkFreeDescriptorSets given descriptorPool descriptorSetCount arrayPtr & onVkFailureThrow_ "vkFreeDescriptorSets"
      )
  else
    throwVkaException "Cannot allocate 0 descriptor sets."

  where
    descriptorSetCount = getField @"descriptorSetCount" allocateInfo
    descriptorPool = getField @"descriptorPool" allocateInfo

vkaAllocateDescriptorSets :: Given VkDevice => VkDescriptorSetAllocateInfo -> IO (VkaArray VkDescriptorSet)
vkaAllocateDescriptorSets allocateInfo =
  if descriptorSetCount > 0 then
    vkaNewArray_ descriptorSetCount $ \arrayPtr ->
      withPtr allocateInfo $ \allocateInfoPtr ->
        vkAllocateDescriptorSets given allocateInfoPtr arrayPtr & onVkFailureThrow_ "vkAllocateDescriptorSets"
  else
    throwVkaException "Cannot allocate 0 descriptor sets."

  where
    descriptorSetCount = getField @"descriptorSetCount" allocateInfo

vkaUpdateDescriptorSets :: Given VkDevice => [VkWriteDescriptorSet] -> [VkCopyDescriptorSet] -> IO ()
vkaUpdateDescriptorSets writes copies =
  withArray writes $ \writesPtr ->
  withArray copies $ \copiesPtr ->
    vkUpdateDescriptorSets given (lengthNum writes) writesPtr (lengthNum copies) copiesPtr

initStandardWriteDescriptorSet :: CreateVkStruct VkWriteDescriptorSet '["sType", "pNext"] ()
initStandardWriteDescriptorSet =
  set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET &*
  set @"pNext" VK_NULL

initStandardCopyDescriptorSet :: CreateVkStruct VkCopyDescriptorSet '["sType", "pNext"] ()
initStandardCopyDescriptorSet =
  set @"sType" VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET &*
  set @"pNext" VK_NULL

initStandardDescriptorSetAllocateInfo :: CreateVkStruct VkDescriptorSetAllocateInfo '["sType", "pNext"] ()
initStandardDescriptorSetAllocateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO &*
  set @"pNext" VK_NULL

