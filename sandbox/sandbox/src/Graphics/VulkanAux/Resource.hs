{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Graphics.VulkanAux.Resource where

import Prelude.Local

import Control.Monad.Trans.Resource.Local
import Data.Acquire.Local
import Data.Function
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan.Core_1_0
import Graphics.VulkanAux.Exception

data VkaResource ci vk =
  VkaResource {
    vkr'getCreate :: IO (Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult),
    vkr'getDestroy :: IO (vk -> Ptr VkAllocationCallbacks -> IO ()),
    vkr'createName :: String,
    vkr'successResults :: [VkResult]
  }

simpleVkaResource ::
  (Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult) ->
  (vk -> Ptr VkAllocationCallbacks -> IO ()) ->
  String ->
  [VkResult] ->
  VkaResource ci vk
simpleVkaResource create destroy = VkaResource (return create) (return destroy)

simpleVkaResource_ ::
  (Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult) ->
  (vk -> Ptr VkAllocationCallbacks -> IO ()) ->
  String ->
  VkaResource ci vk
simpleVkaResource_ create destroy name = simpleVkaResource create destroy name [VK_SUCCESS]

simpleParamVkaResource ::
  (a -> Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult) ->
  (a -> vk -> Ptr VkAllocationCallbacks -> IO ()) ->
  String ->
  [VkResult] ->
  a ->
  VkaResource ci vk
simpleParamVkaResource create destroy name successResults device = simpleVkaResource (create device) (destroy device) name successResults

simpleParamVkaResource_ ::
  (a -> Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult) ->
  (a -> vk -> Ptr VkAllocationCallbacks -> IO ()) ->
  String ->
  a ->
  VkaResource ci vk
simpleParamVkaResource_ create destroy name = simpleParamVkaResource create destroy name [VK_SUCCESS]

newVkWithResult :: (Storable vk, VulkanMarshal ci) => VkaResource ci vk -> ci -> IO (VkResult, vk)
newVkWithResult VkaResource{..} createInfo =
  withPtr createInfo $ \createInfoPtr ->
  alloca $ \vkPtr -> do
    create <- vkr'getCreate
    result <- create createInfoPtr VK_NULL vkPtr & onVkFailureThrow vkr'createName vkr'successResults
    (result,) <$> peek vkPtr

newVk :: (Storable vk, VulkanMarshal ci) => VkaResource ci vk -> ci -> IO vk
newVk = fmap snd .: newVkWithResult

acquireVkWithResult :: (Storable vk, VulkanMarshal ci) => VkaResource ci vk -> ci -> Acquire (VkResult, vk)
acquireVkWithResult r createInfo =
  newVkWithResult r createInfo
  `mkAcquire`
  \(_, vk) -> do
    destroy <- vkr'getDestroy r
    destroy vk VK_NULL

acquireVk :: (Storable vk, VulkanMarshal ci) => VkaResource ci vk -> ci -> Acquire vk
acquireVk = fmap snd .: acquireVkWithResult

allocateAcquireVk :: (Storable vk, VulkanMarshal ci, MonadResource m) => VkaResource ci vk -> ci -> m (ReleaseKey, vk)
allocateAcquireVk = allocateAcquire .: acquireVk

allocateAcquireVk_ :: (Storable vk, VulkanMarshal ci, MonadResource m) => VkaResource ci vk -> ci -> m vk
allocateAcquireVk_ = allocateAcquire_ .: acquireVk
