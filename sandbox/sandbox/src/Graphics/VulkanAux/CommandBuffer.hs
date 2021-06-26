{-# LANGUAGE RankNTypes #-}

module Graphics.VulkanAux.CommandBuffer where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource.Local
import Data.Acquire.Local
import Data.Function
import Data.Functor
import Data.Reflection
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Array
import Graphics.VulkanAux.Exception
import Graphics.VulkanAux.Fence
import Graphics.VulkanAux.Queue
import Graphics.VulkanAux.Resource

vkaAllocatedCommandBuffers :: Given VkDevice => VkCommandBufferAllocateInfo -> Acquire (VkaArray VkCommandBuffer)
vkaAllocatedCommandBuffers allocateInfo =
  if commandBufferCount > 0 then
    vkaAcquireArray_ commandBufferCount
      (\arrayPtr ->
        withPtr allocateInfo $ \allocateInfoPtr ->
          vkAllocateCommandBuffers given allocateInfoPtr arrayPtr & onVkFailureThrow_ "vkAllocateCommandBuffers"
      )
      (vkFreeCommandBuffers given commandPool commandBufferCount)
  else
    throwVkaException "Cannot allocate 0 command buffers."

  where
    commandBufferCount = getField @"commandBufferCount" allocateInfo
    commandPool = getField @"commandPool" allocateInfo

initStandardCommandBufferAllocateInfo :: CreateVkStruct VkCommandBufferAllocateInfo '["sType", "pNext"] ()
initStandardCommandBufferAllocateInfo =
  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO &*
  set @"pNext" VK_NULL

vkaRecordingCommandBuffer :: VkCommandBuffer -> VkCommandBufferBeginInfo -> Acquire ()
vkaRecordingCommandBuffer commandBuffer beginInfo =
  (
    withPtr beginInfo $ \beginInfoPtr ->
      vkBeginCommandBuffer commandBuffer beginInfoPtr & onVkFailureThrow_ "vkBeginCommandBuffer"
  )
  `mkAcquire`
  const (
    vkEndCommandBuffer commandBuffer & onVkFailureThrow_ "vkEndCommandBuffer"
  )

initStandardCommandBufferBeginInfo :: CreateVkStruct VkCommandBufferBeginInfo '["sType", "pNext"] ()
initStandardCommandBufferBeginInfo =
  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO &*
  set @"pNext" VK_NULL

initPrimaryCommandBufferBeginInfo :: CreateVkStruct VkCommandBufferBeginInfo '["sType", "pNext", "pInheritanceInfo"] ()
initPrimaryCommandBufferBeginInfo =
  initStandardCommandBufferBeginInfo &*
  set @"pInheritanceInfo" VK_NULL

vkaExecuteCommands :: (MonadUnliftIO m, MonadFail m, Given VkDevice) => VkCommandPool -> VkQueue -> (forall n. MonadIO n => VkCommandBuffer -> n a) -> m a
vkaExecuteCommands commandPool submissionQueue fillCommandBuffer = runResourceT $ do
  [commandBuffer] <-
    fmap vkaElems $
    allocateAcquire_ $ vkaAllocatedCommandBuffers $
    createVk $
    initStandardCommandBufferAllocateInfo &*
    set @"commandPool" commandPool &*
    set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY &*
    set @"commandBufferCount" 1

  result <-
    with_ (
      vkaRecordingCommandBuffer commandBuffer $
      createVk $
      initPrimaryCommandBufferBeginInfo &*
      set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    )
    (fillCommandBuffer commandBuffer)

  executionCompleteFence <-
    vkaAllocateResource_ vkaFenceResource $
    createVk $
    initStandardFenceCreateInfo &*
    setFenceSignaled False

  liftIO $ do
    vkaQueueSubmit submissionQueue executionCompleteFence
      [
        createVk $
        initStandardSubmitInfo &*
        setSubmitWaitSemaphoresAndStageFlags [] &*
        setListCountAndRef @"commandBufferCount" @"pCommandBuffers" [commandBuffer] &*
        setListCountAndRef @"signalSemaphoreCount" @"pSignalSemaphores" []
      ]
    vkaWaitForFence executionCompleteFence maxBound & void

  return result
