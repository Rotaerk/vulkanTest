{-# LANGUAGE RecordWildCards #-}

module Graphics.VulkanAux.ShaderModule (
  vkaShaderModuleResource,
  initStandardShaderModuleCreateInfo,
  vkaCreateShaderModuleFromFile
) where

import Prelude.Local

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource.Local
import Data.Acquire.Local
import Data.Bits.Local
import Foreign.Marshal.Array
import Foreign.Marshal.Array.Sized
import Foreign.Ptr
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Resource
import System.IO

vkaShaderModuleResource :: VkDevice -> VkaResource VkShaderModuleCreateInfo VkShaderModule
vkaShaderModuleResource = vkaSimpleParamResource_ vkCreateShaderModule vkDestroyShaderModule "vkCreateShaderModule"

initStandardShaderModuleCreateInfo :: CreateVkStruct VkShaderModuleCreateInfo '["sType", "pNext", "flags"] ()
initStandardShaderModuleCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" zeroBits

vkaCreateShaderModuleFromFile :: MonadUnliftIO m => VkDevice -> FilePath -> ResourceT m VkShaderModule
vkaCreateShaderModuleFromFile device filePath = runResourceT $ do
  SizedArray{..} <- fillArrayFromSpirvFile filePath
  lift $ vkaAllocateResource_ (vkaShaderModuleResource device) $
    createVk $
    initStandardShaderModuleCreateInfo &*
    set @"codeSize" (fromIntegral sizedArray'size) &*
    set @"pCode" (castPtr sizedArray'ptr)

fillArrayFromSpirvFile :: MonadUnliftIO m => FilePath -> ResourceT m (SizedArray Word8)
fillArrayFromSpirvFile filePath = runResourceT $ do
  h <- allocate_ (openBinaryFile filePath ReadMode) hClose

  -- Vulkan requires SPIR-V bytecode to have an alignment of 4 bytes.
  alignedSize <- liftIO $ alignTo 4 . fromIntegral <$> hFileSize h
  array <- lift $ allocateAcquire_ (acquireSizedArray @Word8 alignedSize)

  let ptr = sizedArray'ptr array

  liftIO $ do
    bytesRead <- hGetBuf h ptr alignedSize
    pokeArray @Word8 (plusPtr ptr bytesRead) $ replicate (alignedSize - bytesRead) 0

  return array

