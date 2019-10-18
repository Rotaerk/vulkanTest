module Graphics.VulkanAux.ArrayFiller where

import Control.Monad.IO.Class
import Data.Function
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan.Core_1_0
import Graphics.VulkanAux.Array
import Graphics.VulkanAux.Exception

type VkaArrayFiller vk r = Ptr Word32 -> Ptr vk -> IO r

vkaGetArray :: (MonadIO io, Storable vk) => VkaArrayFiller vk r -> io (r, VkaArray vk)
vkaGetArray fillArray =
  liftIO $
  alloca $ \countPtr -> do
    let fillArray' = fillArray countPtr
    getCountResult <- fillArray' VK_NULL
    count <- peek countPtr
    vkaNewArray count $ \arrPtr -> if count > 0 then fillArray' arrPtr else return getCountResult

vkaGetArray_ :: (MonadIO io, Storable vk) => VkaArrayFiller vk r -> io (VkaArray vk)
vkaGetArray_ = fmap snd . vkaGetArray

-- When a VkaArrayFiller is used with vkaGetArray, VK_INCOMPLETE will never be returned, since
-- vkaGetArray is checking for available count first. Thus, don't provide it as a success result.
onArrayFillerFailureThrow :: String -> [VkResult] -> VkaArrayFiller vk VkResult -> VkaArrayFiller vk VkResult
onArrayFillerFailureThrow functionName successResults fillArray countPtr arrayPtr = fillArray countPtr arrayPtr & onVkFailureThrow functionName successResults
