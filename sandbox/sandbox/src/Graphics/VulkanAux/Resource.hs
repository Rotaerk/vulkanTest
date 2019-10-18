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
    vkaResource'getCreate :: IO (Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult),
    vkaResource'getDestroy :: IO (vk -> Ptr VkAllocationCallbacks -> IO ()),
    vkaResource'createName :: String,
    vkaResource'successResults :: [VkResult]
  }

vkaSimpleResource ::
  (Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult) ->
  (vk -> Ptr VkAllocationCallbacks -> IO ()) ->
  String ->
  [VkResult] ->
  VkaResource ci vk
vkaSimpleResource create destroy = VkaResource (return create) (return destroy)

vkaSimpleResource_ ::
  (Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult) ->
  (vk -> Ptr VkAllocationCallbacks -> IO ()) ->
  String ->
  VkaResource ci vk
vkaSimpleResource_ create destroy name = vkaSimpleResource create destroy name [VK_SUCCESS]

vkaSimpleParamResource ::
  (a -> Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult) ->
  (a -> vk -> Ptr VkAllocationCallbacks -> IO ()) ->
  String ->
  [VkResult] ->
  a ->
  VkaResource ci vk
vkaSimpleParamResource create destroy name successResults device = vkaSimpleResource (create device) (destroy device) name successResults

vkaSimpleParamResource_ ::
  (a -> Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult) ->
  (a -> vk -> Ptr VkAllocationCallbacks -> IO ()) ->
  String ->
  a ->
  VkaResource ci vk
vkaSimpleParamResource_ create destroy name = vkaSimpleParamResource create destroy name [VK_SUCCESS]

vkaNewWithResult :: (Storable vk, VulkanMarshal ci) => VkaResource ci vk -> ci -> IO (VkResult, vk)
vkaNewWithResult VkaResource{..} createInfo =
  withPtr createInfo $ \createInfoPtr ->
  alloca $ \vkPtr -> do
    create <- vkaResource'getCreate
    result <- create createInfoPtr VK_NULL vkPtr & onVkFailureThrow vkaResource'createName vkaResource'successResults
    (result,) <$> peek vkPtr

vkaNew :: (Storable vk, VulkanMarshal ci) => VkaResource ci vk -> ci -> IO vk
vkaNew = fmap snd .: vkaNewWithResult

vkaAcquireWithResult :: (Storable vk, VulkanMarshal ci) => VkaResource ci vk -> ci -> Acquire (VkResult, vk)
vkaAcquireWithResult r createInfo =
  vkaNewWithResult r createInfo
  `mkAcquire`
  \(_, vk) -> do
    destroy <- vkaResource'getDestroy r
    destroy vk VK_NULL

vkaAcquire :: (Storable vk, VulkanMarshal ci) => VkaResource ci vk -> ci -> Acquire vk
vkaAcquire = fmap snd .: vkaAcquireWithResult

vkaAllocateResource :: (Storable vk, VulkanMarshal ci, MonadResource m) => VkaResource ci vk -> ci -> m (ReleaseKey, vk)
vkaAllocateResource = allocateAcquire .: vkaAcquire

vkaAllocateResource_ :: (Storable vk, VulkanMarshal ci, MonadResource m) => VkaResource ci vk -> ci -> m vk
vkaAllocateResource_ = allocateAcquire_ .: vkaAcquire
