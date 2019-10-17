{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics.VulkanAux.KTX where

import Prelude.Local
  
import qualified Codec.Image.Ktx.Read as KTX
import qualified Codec.Image.Ktx.VkConstants as KTX
import Control.Applicative
import Control.Exception
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Data.Acquire.Local
import Data.Bits.Local
import Data.Functor
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Foreign.Ptr
import Graphics.Vulkan.Constants
import Graphics.Vulkan.Core_1_1
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Buffer
import Graphics.VulkanAux.Command
import Graphics.VulkanAux.CommandBuffer
import Graphics.VulkanAux.Exception
import Graphics.VulkanAux.Image
import Graphics.VulkanAux.ImageView
import Graphics.VulkanAux.Memory
import Graphics.VulkanAux.Resource
import Graphics.VulkanAux.Sampler

depthBearingFormats :: Set VkFormat
depthBearingFormats =
  Set.fromList [
    VK_FORMAT_D16_UNORM,
    VK_FORMAT_D16_UNORM_S8_UINT,
    VK_FORMAT_X8_D24_UNORM_PACK32,
    VK_FORMAT_D24_UNORM_S8_UINT,
    VK_FORMAT_D32_SFLOAT,
    VK_FORMAT_D32_SFLOAT_S8_UINT
  ]

stencilBearingFormats :: Set VkFormat
stencilBearingFormats =
  Set.fromList [
    VK_FORMAT_S8_UINT,
    VK_FORMAT_D16_UNORM_S8_UINT,
    VK_FORMAT_D24_UNORM_S8_UINT,
    VK_FORMAT_D32_SFLOAT_S8_UINT
  ]

createImageFromKtxTexture ::
  (MonadUnliftIO m, MonadThrow m, MonadFail m) =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  VkCommandPool ->
  VkQueue ->
  VkSampleCountFlagBits ->
  VkImageTiling ->
  VkImageUsageFlags ->
  [Word32] ->
  VkImageLayout ->
  FilePath ->
  ResourceT m (VkImage, VkDeviceMemory, VkImageView, VkSampler)
createImageFromKtxTexture device pdmp commandPool queue sampleCountFlagBit tiling usageFlags qfis initialLayout filePath = runResourceT $ do
  (h@KTX.Header{..}, stagingBufferRegions, stagingBuffer, stagingBufferMemory) <-
    KTX.readKtxFile filePath KTX.skipMetadata $ \header () (fromIntegral -> textureDataSize) readTextureDataInto -> do
      (buffer, bufferMemory) <- lift . lift $ vkaCreateStagingBuffer device pdmp [] textureDataSize
      bufferRegions <- with (vkaMappedMemory device bufferMemory 0 textureDataSize) $ readTextureDataInto . castPtr
      return (header, bufferRegions, buffer, bufferMemory)

  let
    isCubeMap = KTX.isCubeMap h
    isArray = KTX.isArray h
    imageWidth = header'pixelWidth
    imageHeight = replace 0 1 header'pixelHeight
    imageDepth = replace 0 1 header'pixelDepth
    numArrayLayers = replace 0 1 header'numberOfArrayElements
    numMipLevels = KTX.effectiveNumberOfMipmapLevels h
    formatNum =
      fromMaybe (throwVkaException "Unsupported KTX format.") $
      KTX.getVkFormatFromGlTypeAndFormat header'glType header'glFormat <|>
      KTX.getVkFormatFromGlInternalFormat header'glInternalFormat
    pixelSize = (`div` 8) . KTX.vkFormatSize'blockSizeInBits . KTX.getVkFormatSize $ formatNum
    format = VkFormat formatNum
    aspectMask :: VkImageAspectFlags =
      replace zeroBits VK_IMAGE_ASPECT_COLOR_BIT $
      setIf (Set.member format depthBearingFormats) VK_IMAGE_ASPECT_DEPTH_BIT .|.
      setIf (Set.member format stencilBearingFormats) VK_IMAGE_ASPECT_STENCIL_BIT

  (image, imageMemory) <-
    lift $
    vkaCreateBoundImage device pdmp (
      return . allAreSet VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT . getField @"propertyFlags" . snd,
      \_ _ -> return EQ
    ) $
    createVk $
    initStandardImageCreateInfo &*
    set @"flags" (
      setIf isCubeMap VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT .|.
      setIf isArray VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
    ) &*
    set @"imageType" (
      case (header'pixelHeight, header'pixelDepth) of
        (0, 0) -> VK_IMAGE_TYPE_1D
        (_, 0) -> VK_IMAGE_TYPE_2D
        _ -> VK_IMAGE_TYPE_3D
    ) &*
    set @"format" format &*
    setVk @"extent" (
      set @"width" imageWidth &*
      set @"height" imageHeight &*
      set @"depth" imageDepth
    ) &*
    set @"mipLevels" numMipLevels &*
    set @"arrayLayers" numArrayLayers &*
    set @"samples" sampleCountFlagBit &*
    set @"tiling" tiling &*
    set @"usage" usageFlags &*
    setSharingQueueFamilyIndices qfis &*
    set @"initialLayout" initialLayout

  vkaExecuteCommands device commandPool queue $ \commandBuffer -> liftIO $ do
    vkaCmdPipelineBarrier commandBuffer
      VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
      VK_PIPELINE_STAGE_TRANSFER_BIT
      zeroBits [] []
      [
        createVk $
        initStandardImageMemoryBarrier &*
        set @"srcAccessMask" zeroBits &*
        set @"dstAccessMask" VK_ACCESS_TRANSFER_WRITE_BIT &*
        set @"oldLayout" VK_IMAGE_LAYOUT_UNDEFINED &*
        set @"newLayout" VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &*
        set @"srcQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED &*
        set @"dstQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED &*
        set @"image" image &*
        setVk @"subresourceRange" (
          set @"aspectMask" aspectMask &*
          set @"baseMipLevel" 0 &*
          set @"levelCount" numMipLevels &*
          set @"baseArrayLayer" 0 &*
          set @"layerCount" numArrayLayers
        )
      ]

    vkaCmdCopyBufferToImage commandBuffer stagingBuffer image VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL $
      case stagingBufferRegions of
        KTX.SimpleBufferRegions brs ->
          zip [0..] brs <&> \(mipLevelInt@(fromIntegral -> mipLevel), (size, fromIntegral -> offset)) ->
            let
              width = replace 0 1 $ imageWidth `shiftR` mipLevelInt
              height = replace 0 1 $ imageHeight `shiftR` mipLevelInt
              depth = replace 0 1 $ imageDepth `shiftR` mipLevelInt
            in
              assert (size == fromIntegral (width * height * depth * pixelSize)) $
              createVk $
              set @"bufferOffset" offset &*
              set @"bufferRowLength" 0 &*
              set @"bufferImageHeight" 0 &*
              setVk @"imageSubresource" (
                set @"aspectMask" aspectMask &*
                set @"mipLevel" mipLevel &*
                set @"baseArrayLayer" 0 &*
                set @"layerCount" 1
              ) &*
              setVk @"imageOffset" (set @"x" 0 &* set @"y" 0 &* set @"z" 0) &*
              setVk @"imageExtent" (set @"width" width &* set @"height" height &* set @"depth" depth)

        KTX.NonArrayCubeMapBufferRegions brs -> do
          undefined
          {-
          (mipLevel, assertPred ((6 ==) . length) -> offsets) <- zip [0..] (snd <$> brs)
          (arrayLayer, offset) <- zip [0..] (fromIntegral <$> offsets)
          return $
            createVk $
            set @"bufferOffset" offset &*
            set @"bufferRowLength" 0 &*
            set @"bufferImageHeight" 0 &*
            setVk @"imageSubresource" (
              set @"aspectMask" aspectMask &*
              set @"mipLevel" mipLevel &*
              set @"baseArrayLayer" arrayLayer &*
              set @"layerCount" 1
            ) &*
            setVk @"imageOffset" (set @"x" 0 &* set @"y" 0 &* set @"z" 0) &*
            setVk @"imageExtent" (set @"width" 0 &* set @"height" 0 &* set @"depth" 0)
          -}

    vkaCmdPipelineBarrier commandBuffer
      VK_PIPELINE_STAGE_TRANSFER_BIT
      VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
      zeroBits [] []
      [
        createVk $
        initStandardImageMemoryBarrier &*
        set @"srcAccessMask" VK_ACCESS_TRANSFER_WRITE_BIT &*
        set @"dstAccessMask" VK_ACCESS_SHADER_READ_BIT &*
        set @"oldLayout" VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &*
        set @"newLayout" VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL &*
        set @"srcQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED &*
        set @"dstQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED &*
        set @"image" image &*
        setVk @"subresourceRange" (
          set @"aspectMask" aspectMask &*
          set @"baseMipLevel" 0 &*
          set @"levelCount" numMipLevels &*
          set @"baseArrayLayer" 0 &*
          set @"layerCount" numArrayLayers
        )
      ]

  imageView <-
    lift . allocateAcquireVk_ (vkaImageViewResource device) $
    createVk $
    initStandardImageViewCreateInfo &*
    set @"flags" zeroBits &*
    set @"image" image &*
    set @"viewType" (
      case (header'pixelHeight, header'pixelDepth) of
        (0, 0) -> VK_IMAGE_VIEW_TYPE_1D
        (_, 0) -> VK_IMAGE_VIEW_TYPE_2D
        _ -> VK_IMAGE_VIEW_TYPE_3D
    ) &*
    set @"format" format &*
    setVk @"components" (
      set @"r" VK_COMPONENT_SWIZZLE_IDENTITY &*
      set @"g" VK_COMPONENT_SWIZZLE_IDENTITY &*
      set @"b" VK_COMPONENT_SWIZZLE_IDENTITY &*
      set @"a" VK_COMPONENT_SWIZZLE_IDENTITY
    ) &*
    setVk @"subresourceRange" (
      set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT &*
      set @"baseMipLevel" 0 &*
      set @"levelCount" numMipLevels &*
      set @"baseArrayLayer" 0 &*
      set @"layerCount" numArrayLayers
    )

  sampler <-
    lift . allocateAcquireVk_ (vkaSamplerResource device) $
    createVk $
    initStandardSamplerCreateInfo &*
    set @"magFilter" VK_FILTER_LINEAR &*
    set @"minFilter" VK_FILTER_LINEAR &*
    set @"addressModeU" VK_SAMPLER_ADDRESS_MODE_REPEAT &*
    set @"addressModeV" VK_SAMPLER_ADDRESS_MODE_REPEAT &*
    set @"addressModeW" VK_SAMPLER_ADDRESS_MODE_REPEAT &*
    set @"anisotropyEnable" VK_FALSE &*
    set @"maxAnisotropy" 0 &*
    set @"borderColor" VK_BORDER_COLOR_INT_OPAQUE_BLACK &*
    set @"unnormalizedCoordinates" VK_FALSE &*
    set @"compareEnable" VK_FALSE &*
    set @"compareOp" VK_COMPARE_OP_ALWAYS &*
    set @"mipmapMode" VK_SAMPLER_MIPMAP_MODE_LINEAR &*
    set @"minLod" 0 &*
    set @"maxLod" (fromIntegral numMipLevels) &*
    set @"mipLodBias" 0

  return (image, imageMemory, imageView, sampler)

