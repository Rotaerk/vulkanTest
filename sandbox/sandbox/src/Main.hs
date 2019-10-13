{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude.Local

import Paths_sandbox

import qualified Codec.Image.Ktx.Read as KTX
import qualified Codec.Image.Ktx.VkConstants as KTX
import Control.Applicative
import Control.Exception (throw)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Resource.Local
import Data.Acquire.Local
import Data.Array.Base as DAB
import Data.Array.Storable
import Data.Bits.Local
import Data.Foldable
import Data.Function
import Data.Functor
import Data.IORef
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Array.Sized
import Foreign.Ptr
import Foreign.Storable

import GHC.Generics (Generic)

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Core_1_1
import Graphics.Vulkan.Ext.VK_EXT_debug_report
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame

import Numeric.DataFrame hiding (sortBy)
import Numeric.Dimensions

import System.Clock
import System.IO
import System.IO.Unsafe
import UnliftIO.Exception

main :: IO ()
main =
  runResourceT resourceMain
  `catch` (
    \(e :: ApplicationException) ->
      putStrLn $ displayException e
  )
  `catch` (
    \(e :: VkResultException) ->
      putStrLn $ displayException e
  )
  `catch` (
    \(e :: GLFWException) ->
      putStrLn $ displayException e
  )

resourceMain :: ResourceT IO ()
resourceMain = do
  liftIO . GLFW.setErrorCallback . Just $ \errorCode errorMessage ->
    putStr $ "GLFW error callback: " ++ show errorCode ++ " - " ++ errorMessage
  ioPutStrLn "GLFW error callback set."

  allocateAcquire_ initializedGLFW
  ioPutStrLn "GLFW initialized."

  glfwExtensions <- liftIO GLFW.getRequiredInstanceExtensions

  let allExtensions = glfwExtensions ++ extensions

  vulkanInstance <-
    allocateAcquireVk_ vulkanInstanceResource $
    createVk $
    initStandardInstanceCreateInfo &*
    setVkRef @"pApplicationInfo" (
      createVk $
      initStandardApplicationInfo &*
      setStrRef @"pApplicationName" "Vulkan Sandbox" &*
      set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0) &*
      setStrRef @"pEngineName" "" &*
      set @"engineVersion" 0 &*
      set @"apiVersion" VK_API_VERSION_1_1
    ) &*
    setStrListCountAndRef @"enabledLayerCount" @"ppEnabledLayerNames" validationLayers &*
    setListCountAndRef @"enabledExtensionCount" @"ppEnabledExtensionNames" allExtensions
  ioPutStrLn "Vulkan instance created."

#ifndef NDEBUG
  vkaRegisterDebugCallback vulkanInstance
    (
      VK_DEBUG_REPORT_ERROR_BIT_EXT .|.
      VK_DEBUG_REPORT_WARNING_BIT_EXT .|.
      VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT .|.
      VK_DEBUG_REPORT_INFORMATION_BIT_EXT .|.
      VK_DEBUG_REPORT_DEBUG_BIT_EXT
    ) $
    \flags objectType object location messageCode layerPrefixPtr messagePtr userDataPtr -> do
      message <- peekCString messagePtr
      putStrLn $ "Validation layer: " ++ message
      return VK_FALSE
  ioPutStrLn "Vulkan debug callback registered."
#endif

  physicalDeviceArray <- getVkArray (vkaEnumeratePhysicalDevices vulkanInstance)

  (physicalDevice, physicalDeviceProperties, physicalDeviceMemoryProperties) <-
    liftIO $
    forM (vkaElems physicalDeviceArray) (\pd ->
      liftM2 (pd,,)
        (getVk . vkGetPhysicalDeviceProperties $ pd)
        (getVk . vkGetPhysicalDeviceMemoryProperties $ pd)
    ) <&>
    sortBy (
      mconcat [
        prefer [VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU, VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU] `on` getField @"deviceType" . snd3,
        compare `on` pdmpDeviceLocalMemorySize . thd3
      ]
    ) <&>
    headOr (throwAppEx "No physical device found")
  ioPutStrLn "Physical device selected."

  window <- allocateAcquire_ $ newVulkanGLFWWindow 800 600 "Vulkan Sandbox"
  ioPutStrLn "Window created."

  lastWindowResizeTimeRef <- liftIO $ newIORef Nothing

  liftIO $ GLFW.setFramebufferSizeCallback window $ Just $ \_ _ _ -> do
    time <- getTime Monotonic
    writeIORef lastWindowResizeTimeRef $ Just time
  ioPutStrLn "Window framebuffer size callback registered."

  windowSurface <- allocateAcquire_ $ newVulkanGLFWWindowSurface vulkanInstance window
  ioPutStrLn "Window surface created."

  physicalDeviceQueueFamilyPropertiesArray <- getVkArray (vkGetPhysicalDeviceQueueFamilyProperties physicalDevice)

  qfis@[graphicsQfi, computeQfi, transferQfi, presentQfi] <-
    fmap fst . minimumBy (compare `on` length . nub) <$> selectionsFromM (vkaAssocs physicalDeviceQueueFamilyPropertiesArray) [
      (
        return . someAreSet VK_QUEUE_GRAPHICS_BIT . getField @"queueFlags" . snd,
        preferWhereM [
          return . noneAreSet VK_QUEUE_COMPUTE_BIT . getField @"queueFlags" . snd
        ]
      ),
      (
        return . someAreSet VK_QUEUE_COMPUTE_BIT . getField @"queueFlags" . snd,
        preferWhereM [
          return . noneAreSet VK_QUEUE_GRAPHICS_BIT . getField @"queueFlags" . snd
        ]
      ),
      (
        return . someAreSet (VK_QUEUE_GRAPHICS_BIT .|. VK_QUEUE_COMPUTE_BIT .|. VK_QUEUE_TRANSFER_BIT) . getField @"queueFlags" . snd,
        preferWhereM [
          return . noneAreSet (VK_QUEUE_GRAPHICS_BIT .|. VK_QUEUE_COMPUTE_BIT) . getField @"queueFlags" . snd
        ]
      ),
      (
        \(qfi, _) -> (VK_TRUE ==) <$> getVk (vkaGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi windowSurface),
        \_ _ -> return EQ
      )
    ]
  ioPutStrLn "Queue family indices selected."

  device <-
    allocateAcquireVk_ (deviceResource physicalDevice) $
    createVk $
    initStandardDeviceCreateInfo &*
    setListCountAndRef @"queueCreateInfoCount" @"pQueueCreateInfos" (
      nub qfis <&> \qfi ->
        createVk $
        initStandardDeviceQueueCreateInfo &*
        set @"flags" zeroBits &*
        set @"queueFamilyIndex" qfi &*
        setListCountAndRef @"queueCount" @"pQueuePriorities" [1.0]
    ) &*
    setListCountAndRef @"enabledExtensionCount" @"ppEnabledExtensionNames" deviceExtensions &*
    set @"pEnabledFeatures" VK_NULL
  ioPutStrLn "Vulkan device created."

  [(graphicsQueue, graphicsCommandPool), (computeQueue, computeCommandPool), (transferQueue, transferCommandPool), (presentQueue, presentCommandPool)] <-
    forM qfis $ \qfi -> liftM2 (,)
      (getVk $ vkGetDeviceQueue device qfi 0)
      (
        allocateAcquireVk_ (commandPoolResource device) $
        createVk $
        initStandardCommandPoolCreateInfo &*
        set @"flags" zeroBits &*
        set @"queueFamilyIndex" qfi
      )
  ioPutStrLn "Device queues obtained, and corresponding command pools created."

  descriptorSetLayout <-
    allocateAcquireVk_ (descriptorSetLayoutResource device) $
    createVk $
    initStandardDescriptorSetLayoutCreateInfo &*
    set @"flags" zeroBits &*
    setListCountAndRef @"bindingCount" @"pBindings" (
      fmap createVk [
        set @"binding" 0 &*
        set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER &*
        set @"descriptorCount" 1 &*
        set @"stageFlags" VK_SHADER_STAGE_VERTEX_BIT &*
        set @"pImmutableSamplers" VK_NULL,

        set @"binding" 1 &*
        set @"descriptorType" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER &*
        set @"descriptorCount" 1 &*
        set @"stageFlags" VK_SHADER_STAGE_FRAGMENT_BIT &*
        set @"pImmutableSamplers" VK_NULL
      ]
    )
  ioPutStrLn "Descriptor set layout created."

  pipelineLayout <-
    allocateAcquireVk_ (pipelineLayoutResource device) $
    createVk $
    initStandardPipelineLayoutCreateInfo &*
    setListCountAndRef @"setLayoutCount" @"pSetLayouts" [descriptorSetLayout] &*
    setListCountAndRef @"pushConstantRangeCount" @"pPushConstantRanges" []
  ioPutStrLn "Pipeline layout created."

  (textureImage, textureImageMemory, textureImageView, textureSampler) <-
    createImageFromKtxTexture
      device
      physicalDeviceMemoryProperties
      graphicsCommandPool
      graphicsQueue
      VK_SAMPLE_COUNT_1_BIT
      VK_IMAGE_TILING_OPTIMAL
      (VK_IMAGE_USAGE_TRANSFER_SRC_BIT .|. VK_IMAGE_USAGE_TRANSFER_DST_BIT .|. VK_IMAGE_USAGE_SAMPLED_BIT)
      [graphicsQfi]
      VK_IMAGE_LAYOUT_UNDEFINED
      "../ktx-rw/ktx-rw/textures/oak_bark.ktx"
  ioPutStrLn "Loaded KTX texture to an image."

  (vertexBuffer, vertexBufferMemory) <-
    createFilledBufferFromPrimBytes
      device
      physicalDeviceMemoryProperties
      transferCommandPool
      transferQueue
      VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
      [graphicsQfi]
      (
        packDF @Vertex @4 @'[]
          (svertex (vec2 (-0.5) (-0.5)) (vec2 1 0))
          (svertex (vec2 (0.5) (-0.5)) (vec2 0 0))
          (svertex (vec2 (0.5) (0.5)) (vec2 0 1))
          (svertex (vec2 (-0.5) (0.5)) (vec2 1 1))
      )
  ioPutStrLn "Vertex buffer created."

  (indexBuffer, indexBufferMemory) <-
    createFilledBufferFromPrimBytes
      device
      physicalDeviceMemoryProperties
      transferCommandPool
      transferQueue
      VK_BUFFER_USAGE_INDEX_BUFFER_BIT
      [graphicsQfi]
      (packDF @Word32 @6 @'[] 0 2 1 0 3 2)
  ioPutStrLn "Index buffer created."

  frameSyncs <- replicateM maxFramesInFlight $ FrameSync <$> createFence device True <*> createSemaphore device <*> createSemaphore device
  ioPutStrLn "Frame syncs created."

  renderStartTimeRef <- liftIO $ newIORef Nothing

  doWhileM $ runResourceT $ do
    (windowFramebufferWidth, windowFramebufferHeight) <- liftIO $ GLFW.getFramebufferSize window

    surfaceCapabilities <- getVk $ vkaGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice windowSurface
    surfaceFormatArray <- getVkArray $ vkaGetPhysicalDeviceSurfaceFormatsKHR physicalDevice windowSurface
    surfacePresentModeArray <- getVkArray $ vkaGetPhysicalDeviceSurfacePresentModesKHR physicalDevice windowSurface

    let
      swapchainImageFormat = VK_FORMAT_B8G8R8A8_UNORM
      swapchainImageColorSpace = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
      swapchainSurfaceFormat =
        createVk $
        set @"format" swapchainImageFormat &*
        set @"colorSpace" swapchainImageColorSpace

    -- Generally I will want to pick the "best" format according to some criteria.
    -- However, I don't know how those criteria would even look.  So for now, just
    -- require some ideal format to be supported, and fail if it isn't.
    when (swapchainSurfaceFormat `notElem` vkaElems surfaceFormatArray) (throwAppEx "Required surface format not supported by device.")

    let
      -- FIFO mode is always supported, but mailbox mode is preferred if available
      -- because it's lower latency.  We don't want immediate mode, because we have
      -- no need to present so quickly that there is tearing.
      swapchainPresentMode = fromMaybe VK_PRESENT_MODE_FIFO_KHR $ find (== VK_PRESENT_MODE_MAILBOX_KHR) (vkaElems surfacePresentModeArray)

      swapchainImageExtent =
        -- Reminder: If currentExtent is (maxBound, maxBound), the surface extent is
        -- flexible, so we can use the window framebuffer extent clamped to the
        -- extent bounds of the surface.  Otherwise, we have to use currentExtent.
        let
          currentExtent = getField @"currentExtent" surfaceCapabilities
          minImageExtent = getField @"minImageExtent" surfaceCapabilities
          maxImageExtent = getField @"maxImageExtent" surfaceCapabilities
        in
          if getField @"width" currentExtent /= maxBound then
            currentExtent
          else
            createVk $
            set @"width" (fromIntegral windowFramebufferWidth & clamp (getField @"width" minImageExtent) (getField @"width" maxImageExtent)) &*
            set @"height" (fromIntegral windowFramebufferHeight & clamp (getField @"height" minImageExtent) (getField @"height" maxImageExtent))

    swapchain <-
      allocateAcquireVk_ (swapchainResource device) $
      createVk $
      initStandardSwapchainCreateInfo &*
      set @"flags" zeroBits &*
      set @"surface" windowSurface &*
      set @"minImageCount" (
        -- Ideally want one more than the minimum, but can't go over the maximum.
        -- (Reminder: Maximum of 0 means no maximum.)
        let
          idealImageCount = getField @"minImageCount" surfaceCapabilities + 1
          maxImageCount = getField @"maxImageCount" surfaceCapabilities
        in
          if maxImageCount > 0 then
            min idealImageCount maxImageCount
          else
            idealImageCount
      ) &*
      set @"imageFormat" swapchainImageFormat &*
      set @"imageColorSpace" swapchainImageColorSpace &*
      set @"imageExtent" swapchainImageExtent &*
      set @"imageArrayLayers" 1 &*
      set @"imageUsage" VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT &*
      setImageSharingQueueFamilyIndices (nub [graphicsQfi, presentQfi]) &*
      set @"preTransform" (getField @"currentTransform" surfaceCapabilities) &*
      set @"compositeAlpha" VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR &*
      set @"presentMode" swapchainPresentMode &*
      set @"clipped" VK_TRUE &*
      set @"oldSwapchain" VK_NULL
    ioPutStrLn "Swapchain created."

    swapchainImageArray <- getVkArray $ vkaGetSwapchainImagesKHR device swapchain

    swapchainImageViews <-
      forM (vkaElems swapchainImageArray) $ \image ->
        allocateAcquireVk_ (imageViewResource device) $
        createVk $
        initStandardImageViewCreateInfo &*
        set @"flags" zeroBits &*
        set @"image" image &*
        set @"viewType" VK_IMAGE_VIEW_TYPE_2D &*
        set @"format" swapchainImageFormat &*
        setVk @"components" (
          set @"r" VK_COMPONENT_SWIZZLE_IDENTITY &*
          set @"g" VK_COMPONENT_SWIZZLE_IDENTITY &*
          set @"b" VK_COMPONENT_SWIZZLE_IDENTITY &*
          set @"a" VK_COMPONENT_SWIZZLE_IDENTITY
        ) &*
        setVk @"subresourceRange" (
          set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT &*
          set @"baseMipLevel" 0 &*
          set @"levelCount" 1 &*
          set @"baseArrayLayer" 0 &*
          set @"layerCount" 1
        )
    ioPutStrLn "Swapchain image views created."

    let swapchainImageCount@(fromIntegral -> swapchainImageCountWord32) = fromIntegral $ vkaNumElements swapchainImageArray

    (uniformBuffers, uniformBufferMemories) <-
      fmap unzip . replicateM swapchainImageCount $
      createUniformBufferForPrimBytes @UniformBufferObject device physicalDeviceMemoryProperties []
    ioPutStrLn "Uniform buffers created."

    descriptorPool <-
      allocateAcquireVk_ (descriptorPoolResource device) $
      createVk $
      initStandardDescriptorPoolCreateInfo &*
      set @"flags" zeroBits &*
      set @"maxSets" swapchainImageCountWord32 &*
      setListCountAndRef @"poolSizeCount" @"pPoolSizes" (
        createVk . (set @"descriptorCount" swapchainImageCountWord32 &*) <$> [
          set @"type" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
          set @"type" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        ]
      )
    ioPutStrLn "Descriptor pool created."

    descriptorSets <-
      liftIO . fmap vkaElems . allocateDescriptorSets device . createVk $
      initStandardDescriptorSetAllocateInfo &*
      set @"descriptorPool" descriptorPool &*
      setListCountAndRef @"descriptorSetCount" @"pSetLayouts" (replicate swapchainImageCount descriptorSetLayout)
    ioPutStrLn "Descriptor sets allocated."

    liftIO $
      vkaUpdateDescriptorSets device
        (
          zip uniformBuffers descriptorSets >>= \(uniformBuffer, descriptorSet) ->
          createVk <$> [
            initStandardWriteDescriptorSet &*
            set @"dstSet" descriptorSet &*
            set @"dstBinding" 0 &*
            set @"dstArrayElement" 0 &*
            set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER &*
            set @"descriptorCount" 1 &*
            setListRef @"pBufferInfo" [
              createVk $
              set @"buffer" uniformBuffer &*
              set @"offset" 0 &*
              set @"range" (bSizeOf @UniformBufferObject undefined)
            ] &*
            set @"pImageInfo" VK_NULL &*
            set @"pTexelBufferView" VK_NULL,

            initStandardWriteDescriptorSet &*
            set @"dstSet" descriptorSet &*
            set @"dstBinding" 1 &*
            set @"dstArrayElement" 0 &*
            set @"descriptorType" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER &*
            set @"descriptorCount" 1 &*
            set @"pBufferInfo" VK_NULL &*
            setListRef @"pImageInfo" [
              createVk $
              set @"imageLayout" VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL &*
              set @"imageView" textureImageView &*
              set @"sampler" textureSampler
            ] &*
            set @"pTexelBufferView" VK_NULL
          ]
        )
        []
    ioPutStrLn "Descriptor sets written."

    -- Per nVidia, for optimal performance, we should prefer a 24-bit depth format, and we should prefer packed.
    -- Source: https://devblogs.nvidia.com/vulkan-dos-donts/
    --
    -- I might build this list of preferred formats differently depending on the GPU.
    -- (AMD and Intel might have their own recommendations.)
    depthFormat <-
      [VK_FORMAT_X8_D24_UNORM_PACK32, VK_FORMAT_D24_UNORM_S8_UINT] &
      findM (
        fmap (allAreSet VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT . getField @"optimalTilingFeatures") .
        getVk . vkGetPhysicalDeviceFormatProperties physicalDevice
      ) &
      fromMaybeM (throwAppEx "No 24-bit depth formats support being used by a depth attachment on this device.")
    ioPutStrLn $ "Depth format chosen: " ++ show depthFormat ++ "."

    renderPass <-
      allocateAcquireVk_ (renderPassResource device) $
      createVk $
      initStandardRenderPassCreateInfo &*
      setListCountAndRef @"attachmentCount" @"pAttachments" (
        createVk <$> [
          -- Attachment 0: Color buffer
          set @"format" swapchainImageFormat &*
          set @"samples" VK_SAMPLE_COUNT_1_BIT &*
          set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR &*
          set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE &*
          set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE &*
          set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE &*
          set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED &*
          set @"finalLayout" VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,

          -- Attachment 1: Depth buffer
          set @"format" depthFormat &*
          set @"samples" VK_SAMPLE_COUNT_1_BIT &*
          set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR &*
          set @"storeOp" VK_ATTACHMENT_STORE_OP_DONT_CARE &*
          set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE &*
          set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE &*
          set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED &*
          set @"finalLayout" VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        ]
      ) &*
      setListCountAndRef @"subpassCount" @"pSubpasses" (
        createVk <$> [
          set @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS &*
          setListCountAndRef @"colorAttachmentCount" @"pColorAttachments" (
            createVk <$> [
              set @"attachment" 0 &*
              set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
            ]
          ) &*
          setVkRef @"pDepthStencilAttachment" (
            createVk $
            set @"attachment" 1 &*
            set @"layout" VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          ) &*
          set @"pResolveAttachments" VK_NULL &*
          set @"pInputAttachments" VK_NULL &*
          set @"pPreserveAttachments" VK_NULL
        ]
      ) &*
      setListCountAndRef @"dependencyCount" @"pDependencies" (
        createVk <$> [
          set @"srcSubpass" VK_SUBPASS_EXTERNAL &*
          set @"dstSubpass" 0 &*
          set @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT &*
          set @"srcAccessMask" zeroBits &*
          set @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT &*
          set @"dstAccessMask" (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
        ]
      )
    ioPutStrLn "Render pass created."

    [graphicsPipeline] <- runResourceT $ do
      vertShaderModule <- createShaderModuleFromFile device =<< liftIO (getDataFileName "shaders/shader.vert.spv")
      ioPutStrLn "Vertex shader module created."
      fragShaderModule <- createShaderModuleFromFile device =<< liftIO (getDataFileName "shaders/shader.frag.spv")
      ioPutStrLn "Fragment shader module created."

      liftIO $
        vkaElems <$> vkaCreateGraphicsPipelines device VK_NULL_HANDLE (
          createVk . (initStandardGraphicsPipelineCreateInfo &*) <$> [
            setListCountAndRef @"stageCount" @"pStages" (
              createVk . (
                initStandardPipelineShaderStageCreateInfo &*
                set @"flags" zeroBits &*
                setStrRef @"pName" "main" &*
                set @"pSpecializationInfo" VK_NULL &*
              ) <$> [
                set @"stage" VK_SHADER_STAGE_VERTEX_BIT &*
                set @"module" vertShaderModule,

                set @"stage" VK_SHADER_STAGE_FRAGMENT_BIT &*
                set @"module" fragShaderModule
              ]
            ) &*
            setVkRef @"pVertexInputState" (
              createVk $
              initStandardPipelineVertexInputStateCreateInfo &*
              setListCountAndRef @"vertexBindingDescriptionCount" @"pVertexBindingDescriptions" (
                createVk <$> [
                  set @"binding" 0 &*
                  set @"stride" (bSizeOf @SVertex undefined) &*
                  set @"inputRate" VK_VERTEX_INPUT_RATE_VERTEX
                ]
              ) &*
              setListCountAndRef @"vertexAttributeDescriptionCount" @"pVertexAttributeDescriptions" (
                createVk . (set @"binding" 0 &*) <$> [
                  set @"location" 0 &*
                  set @"format" VK_FORMAT_R32G32_SFLOAT &*
                  set @"offset" (bFieldOffsetOf @"vtxPos" @Vertex undefined),

                  set @"location" 1 &*
                  set @"format" VK_FORMAT_R32G32_SFLOAT &*
                  set @"offset" (bFieldOffsetOf @"vtxTexCoord" @Vertex undefined)
                ]
              )
            ) &*
            setVkRef @"pInputAssemblyState" (
              createVk $
              initStandardPipelineInputAssemblyStateCreateInfo &*
              set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST &*
              set @"primitiveRestartEnable" VK_FALSE
            ) &*
            setVkRef @"pViewportState" (
              createVk $
              initStandardPipelineViewportStateCreateInfo &*
              setListCountAndRef @"viewportCount" @"pViewports" (
                createVk <$> [
                  set @"x" 0 &*
                  set @"y" 0 &*
                  set @"width" (fromIntegral . getField @"width" $ swapchainImageExtent) &*
                  set @"height" (fromIntegral . getField @"height" $ swapchainImageExtent) &*
                  set @"minDepth" 0 &*
                  set @"maxDepth" 1
                ]
              ) &*
              setListCountAndRef @"scissorCount" @"pScissors" (
                createVk <$> [
                  setVk @"offset" (set @"x" 0 &* set @"y" 0) &*
                  set @"extent" swapchainImageExtent
                ]
              )
            ) &*
            setVkRef @"pRasterizationState" (
              createVk $
              initStandardPipelineRasterizationStateCreateInfo &*
              set @"depthClampEnable" VK_FALSE &*
              set @"rasterizerDiscardEnable" VK_FALSE &*
              set @"polygonMode" VK_POLYGON_MODE_FILL &*
              set @"lineWidth" 1 &*
              set @"cullMode" VK_CULL_MODE_BACK_BIT &*
              set @"frontFace" VK_FRONT_FACE_COUNTER_CLOCKWISE &*
              set @"depthBiasEnable" VK_FALSE &*
              set @"depthBiasConstantFactor" 0 &*
              set @"depthBiasClamp" 0 &*
              set @"depthBiasSlopeFactor" 0
            ) &*
            setVkRef @"pMultisampleState" (
              createVk $
              initStandardPipelineMultisampleStateCreateInfo &*
              set @"sampleShadingEnable" VK_FALSE &*
              set @"rasterizationSamples" VK_SAMPLE_COUNT_1_BIT &*
              set @"minSampleShading" 1 &*
              set @"pSampleMask" VK_NULL &*
              set @"alphaToCoverageEnable" VK_FALSE &*
              set @"alphaToOneEnable" VK_FALSE
            ) &*
            setVkRef @"pDepthStencilState" (
              createVk $
              initStandardPipelineDepthStencilStateCreateInfo &*
              set @"depthTestEnable" VK_TRUE &*
              set @"depthWriteEnable" VK_TRUE &*
              set @"depthCompareOp" VK_COMPARE_OP_LESS &*
              set @"depthBoundsTestEnable" VK_FALSE &*
              set @"minDepthBounds" 0 &*
              set @"maxDepthBounds" 1 &*
              set @"stencilTestEnable" VK_FALSE &*
              setVk @"front" (
                set @"failOp" VK_STENCIL_OP_KEEP &*
                set @"passOp" VK_STENCIL_OP_KEEP &*
                set @"depthFailOp" VK_STENCIL_OP_KEEP &*
                set @"compareOp" VK_COMPARE_OP_NEVER &*
                set @"compareMask" 0 &*
                set @"writeMask" 0 &*
                set @"reference" 0
              ) &*
              setVk @"back" (
                set @"failOp" VK_STENCIL_OP_KEEP &*
                set @"passOp" VK_STENCIL_OP_KEEP &*
                set @"depthFailOp" VK_STENCIL_OP_KEEP &*
                set @"compareOp" VK_COMPARE_OP_NEVER &*
                set @"compareMask" 0 &*
                set @"writeMask" 0 &*
                set @"reference" 0
              )
            ) &*
            setVkRef @"pColorBlendState" (
              createVk $
              initStandardPipelineColorBlendStateCreateInfo &*
              set @"logicOpEnable" VK_FALSE &*
              set @"logicOp" VK_LOGIC_OP_COPY &*
              setListCountAndRef @"attachmentCount" @"pAttachments" (
                createVk <$> [
                  set @"colorWriteMask" (VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT) &*
                  set @"blendEnable" VK_FALSE &*
                  set @"srcColorBlendFactor" VK_BLEND_FACTOR_ONE &*
                  set @"dstColorBlendFactor" VK_BLEND_FACTOR_ZERO &*
                  set @"colorBlendOp" VK_BLEND_OP_ADD &*
                  set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE &*
                  set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO &*
                  set @"alphaBlendOp" VK_BLEND_OP_ADD
                ]
              ) &*
              setVec @"blendConstants" (vec4 0 0 0 0)
            ) &*
            set @"pDynamicState" VK_NULL &*
            set @"renderPass" renderPass &*
            set @"subpass" 0 &*
            set @"layout" pipelineLayout &*
            set @"basePipelineHandle" VK_NULL_HANDLE &*
            set @"basePipelineIndex" (-1)
          ]
        )
    registerGraphicsPipelineForDestruction_ device graphicsPipeline
    ioPutStrLn "Graphics pipeline created."

    (depthImage, depthImageMemory) <-
      createBoundImage device physicalDeviceMemoryProperties (
        return . allAreSet VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT . getField @"propertyFlags" . snd,
        \_ _ -> return EQ
      ) $
      createVk $
      initStandardImageCreateInfo &*
      set @"flags" zeroBits &*
      set @"imageType" VK_IMAGE_TYPE_2D &*
      set @"format" depthFormat &*
      setVk @"extent" (
        set @"width" (fromIntegral . getField @"width" $ swapchainImageExtent) &*
        set @"height" (fromIntegral . getField @"height" $ swapchainImageExtent) &*
        set @"depth" 1
      ) &*
      set @"mipLevels" 1 &*
      set @"arrayLayers" 1 &*
      set @"samples" VK_SAMPLE_COUNT_1_BIT &*
      set @"tiling" VK_IMAGE_TILING_OPTIMAL &*
      set @"usage" VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT &*
      setSharingQueueFamilyIndices [graphicsQfi] &*
      set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED

    executeCommands device graphicsCommandPool graphicsQueue $ \commandBuffer -> liftIO $ do
      vkaCmdPipelineBarrier commandBuffer
        VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
        VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
        zeroBits [] []
        [
          createVk $
          initStandardImageMemoryBarrier &*
          set @"srcAccessMask" zeroBits &*
          set @"dstAccessMask" (VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT .|. VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT) &*
          set @"oldLayout" VK_IMAGE_LAYOUT_UNDEFINED &*
          set @"newLayout" VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL &*
          set @"srcQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED &*
          set @"dstQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED &*
          set @"image" depthImage &*
          setVk @"subresourceRange" (
            set @"aspectMask" VK_IMAGE_ASPECT_DEPTH_BIT &*
            set @"baseMipLevel" 0 &*
            set @"levelCount" 1 &*
            set @"baseArrayLayer" 0 &*
            set @"layerCount" 1
          )
        ]
    ioPutStrLn "Depth image created."

    depthImageView <-
      allocateAcquireVk_ (imageViewResource device) $
      createVk $
      initStandardImageViewCreateInfo &*
      set @"flags" zeroBits &*
      set @"image" depthImage &*
      set @"viewType" VK_IMAGE_VIEW_TYPE_2D &*
      set @"format" depthFormat &*
      setVk @"components" (
        set @"r" VK_COMPONENT_SWIZZLE_IDENTITY &*
        set @"g" VK_COMPONENT_SWIZZLE_IDENTITY &*
        set @"b" VK_COMPONENT_SWIZZLE_IDENTITY &*
        set @"a" VK_COMPONENT_SWIZZLE_IDENTITY
      ) &*
      setVk @"subresourceRange" (
        set @"aspectMask" VK_IMAGE_ASPECT_DEPTH_BIT &*
        set @"baseMipLevel" 0 &*
        set @"levelCount" 1 &*
        set @"baseArrayLayer" 0 &*
        set @"layerCount" 1
      )
    ioPutStrLn "Depth image view created."

    swapchainFramebuffers <-
      forM swapchainImageViews $ \swapchainImageView ->
      allocateAcquireVk_ (framebufferResource device) $
      createVk $
      initStandardFramebufferCreateInfo &*
      set @"renderPass" renderPass &*
      setListCountAndRef @"attachmentCount" @"pAttachments" [swapchainImageView, depthImageView] &*
      set @"width" (getField @"width" swapchainImageExtent) &*
      set @"height" (getField @"height" swapchainImageExtent) &*
      set @"layers" 1
    ioPutStrLn "Swapchain framebuffers created."

    swapchainCommandBuffers <-
      fmap vkaElems . allocateAcquire_ . allocatedCommandBuffers device . createVk $
      initStandardCommandBufferAllocateInfo &*
      set @"commandPool" graphicsCommandPool &*
      set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY &*
      set @"commandBufferCount" (lengthNum swapchainImageViews)
    ioPutStrLn "Swapchain command buffers created."

    forM_ (zip3 swapchainCommandBuffers swapchainFramebuffers descriptorSets) $ \(commandBuffer, framebuffer, descriptorSet) ->
      with_ (
        recordingCommandBuffer commandBuffer . createVk $
        initPrimaryCommandBufferBeginInfo &*
        set @"flags" VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
      ) $ liftIO $ do
        vkaCmdBeginRenderPass commandBuffer VK_SUBPASS_CONTENTS_INLINE . createVk $
          initStandardRenderPassBeginInfo &*
          set @"renderPass" renderPass &*
          set @"framebuffer" framebuffer &*
          setVk @"renderArea" (
            setVk @"offset" (set @"x" 0 &* set @"y" 0) &*
            set @"extent" swapchainImageExtent
          ) &*
          setListCountAndRef @"clearValueCount" @"pClearValues" [
              createVk $ setVk @"color" (setVec @"float32" $ vec4 0 0 0 1),
              createVk $ setVk @"depthStencil" (set @"depth" 1 &* set @"stencil" 0)
          ]
        vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
        vkaCmdBindVertexBuffers commandBuffer 0 [(vertexBuffer, 0)]
        vkCmdBindIndexBuffer commandBuffer indexBuffer 0 VK_INDEX_TYPE_UINT32
        vkaCmdBindDescriptorSets commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 [descriptorSet] []
        vkCmdDrawIndexed commandBuffer 6 1 0 0 0 -- 6 is the length of the dataframe used to fill the index buffer earlier.
        vkCmdEndRenderPass commandBuffer
    ioPutStrLn "Swapchain command buffers filled."

    renderStartTime <-
      liftIO $ readIORef renderStartTimeRef >>= \case
        Just t -> return t
        Nothing -> do
          time <- getTime Monotonic
          writeIORef renderStartTimeRef $ Just time
          return time

    ioPutStrLn "Render loop starting."
    Just shouldRebuildSwapchain <- liftIO $ flip firstJustM (cycle frameSyncs) $ \FrameSync {..} ->
      getWindowStatus window lastWindowResizeTimeRef >>= \case
        WindowResized -> do
          putStrLn "Window resized!"
          return $ Just True
        WindowClosed -> do
          putStrLn "Window closed!"
          return $ Just False
        WindowReady ->
          handleJust
            (guard . (VK_ERROR_OUT_OF_DATE_KHR ==) . vkaResultException'result)
            (const $ return $ Just True)
          $ do
            vkaWaitForFence device frameSync'inFlightFence maxBound & void
            vkaResetFence device frameSync'inFlightFence

            nextImageIndexWord32@(fromIntegral -> nextImageIndex) <- getVk $ vkaAcquireNextImageKHR device swapchain maxBound frameSync'imageAvailableSemaphore VK_NULL_HANDLE

            -- Obviously there is no point in updating the UBO to the same value every time, but I'm leaving this here
            -- so that I can later easily transform the rendered image over time.
            with (mappedMemory device (uniformBufferMemories !! nextImageIndex) 0 (bSizeOf @UniformBufferObject undefined)) $ \ptr ->
              poke (castPtr ptr) . S $
              UniformBufferObject eye eye eye

            vkaQueueSubmit graphicsQueue frameSync'inFlightFence
              [
                createVk $
                initStandardSubmitInfo &*
                setListCountAndRef @"waitSemaphoreCount" @"pWaitSemaphores" [frameSync'imageAvailableSemaphore] &*
                setListRef @"pWaitDstStageMask" [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT] &*
                setListCountAndRef @"commandBufferCount" @"pCommandBuffers" [swapchainCommandBuffers !! nextImageIndex] &*
                setListCountAndRef @"signalSemaphoreCount" @"pSignalSemaphores" [frameSync'renderFinishedSemaphore]
              ]

            queuePresentResult <-
              vkaQueuePresentKHR presentQueue $
                createVk $
                initStandardPresentInfoKHR &*
                setListCountAndRef @"waitSemaphoreCount" @"pWaitSemaphores" [frameSync'renderFinishedSemaphore] &*
                setListCountAndRef @"swapchainCount" @"pSwapchains" [swapchain] &*
                setListRef @"pImageIndices" [nextImageIndexWord32] &*
                set @"pResults" VK_NULL

            case queuePresentResult of
              VK_SUBOPTIMAL_KHR -> return $ Just True
              _ -> return Nothing

    ioPutStrLn "Render loop ended.  Waiting for device to idle."
    liftIO $ vkaDeviceWaitIdle device

    ioPutStrLn "Cleaning up swapchain-related objects."
    return shouldRebuildSwapchain

  ioPutStrLn "Cleaning up the rest"

extensions :: [CString]
extensions =
  [
  ]
#ifndef NDEBUG
  ++
  [
    VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  ]
#endif

deviceExtensions :: [CString]
deviceExtensions =
  [
    VK_KHR_SWAPCHAIN_EXTENSION_NAME
  ]

validationLayers :: [String]
validationLayers =
  [
#ifndef NDEBUG
    "VK_LAYER_LUNARG_standard_validation"
#endif
  ]

maxFramesInFlight :: Int
maxFramesInFlight = 2

data ApplicationException = ApplicationException String deriving (Eq, Show, Read)

instance Exception ApplicationException where
  displayException (ApplicationException message) =
    "Application error: " ++ message

throwAppEx :: String -> a
throwAppEx message = throw $ ApplicationException message

throwAppExM :: MonadThrow m => String -> m a
throwAppExM message = throwM $ ApplicationException message

data Vertex =
  Vertex {
    vtxPos :: Vec2f,
    vtxTexCoord :: Vec2f
  } deriving (Eq, Show, Generic)

instance PrimBytes Vertex

type SVertex = Scalar Vertex

svertex :: Vec2f -> Vec2f -> SVertex
svertex = S .: Vertex

data UniformBufferObject =
  UniformBufferObject {
    uniformBufferObject'model :: Mat44f,
    uniformBufferObject'view :: Mat44f,
    uniformBufferObject'proj :: Mat44f
  } deriving (Eq, Show, Generic)

instance PrimBytes UniformBufferObject

data FrameSync =
  FrameSync {
    frameSync'inFlightFence :: VkFence,
    frameSync'imageAvailableSemaphore :: VkSemaphore,
    frameSync'renderFinishedSemaphore :: VkSemaphore
  }

-- GLFW helpers>
data GLFWException =
  GLFWException {
    glfwexFunctionName :: String
  } deriving (Eq, Show, Read)

instance Exception GLFWException where
  displayException (GLFWException functionName) = "GLFWException: " ++ functionName ++ " failed."

throwGLFWExceptionM :: MonadThrow m => String -> m a
throwGLFWExceptionM = throwM . GLFWException

initializedGLFW :: Acquire ()
initializedGLFW =
  unlessM GLFW.init (throwGLFWExceptionM "init")
  `mkAcquire`
  const GLFW.terminate

newVulkanGLFWWindow :: Int -> Int -> String -> Acquire GLFW.Window
newVulkanGLFWWindow width height title =
  do
    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
    fromMaybeM (throwGLFWExceptionM "createWindow") (GLFW.createWindow width height title Nothing Nothing)
  `mkAcquire`
  GLFW.destroyWindow

newVulkanGLFWWindowSurface :: VkInstance -> GLFW.Window -> Acquire VkSurfaceKHR
newVulkanGLFWWindowSurface vulkanInstance window =
  (
    alloca $ \surfacePtr -> do
      GLFW.createWindowSurface vulkanInstance window nullPtr surfacePtr & onVkFailureThrow_ "GLFW.createWindowSurface"
      peek surfacePtr
  )
  `mkAcquire`
  \surface -> vkDestroySurfaceKHR vulkanInstance surface VK_NULL

data WindowStatus = WindowReady | WindowResized | WindowClosed

getWindowStatus :: GLFW.Window -> IORef (Maybe TimeSpec) -> IO WindowStatus
getWindowStatus window lastResizeTimeRef =
  GLFW.windowShouldClose window >>= \case
    True -> return WindowClosed
    False -> do
      GLFW.pollEvents
      currentTime <- getTime Monotonic
      -- GLFW sends many resize events during the resizing process, and doesn't say when the user is done resizing.
      -- Thus, only consider it resized after some time has passed since the last event.
      atomicModifyIORef lastResizeTimeRef $ \case
        Just lastResizeTime | currentTime - lastResizeTime >= resizeDelay -> (Nothing, WindowResized)
        v -> (v, WindowReady)
  where
    resizeDelay = fromNanoSecs (100 * 1000 * 1000) -- 100 milliseconds
-- GLFW helpers<

-- Vulkan helpers>
data VkResultException =
  VkResultException {
    vkaResultException'functionName :: String,
    vkaResultException'result :: VkResult
  } deriving (Eq, Show, Read)

instance Exception VkResultException where
  displayException (VkResultException functionName result) =
    functionName ++ " failed with: " ++ show result

onVkFailureThrow :: String -> [VkResult] -> IO VkResult -> IO VkResult
onVkFailureThrow functionName successResults vkAction = do
  result <- vkAction
  unless (result `elem` successResults) $ throwIO (VkResultException functionName result)
  return result

onVkFailureThrow_ :: String -> IO VkResult -> IO ()
onVkFailureThrow_ functionName vkAction = onVkFailureThrow functionName [VK_SUCCESS] vkAction & void

data VkaException = VkaException String deriving (Eq, Show, Read)

instance Exception VkaException where
  displayException (VkaException message) = "VkaException: " ++ message

throwVkaException :: String -> a
throwVkaException = throw . VkaException

throwVkaExceptionM :: MonadThrow m => String -> m a
throwVkaExceptionM = throwM . VkaException

vkaRegisterDebugCallback :: MonadUnliftIO io => VkInstance -> VkDebugReportFlagsEXT -> HS_vkDebugReportCallbackEXT -> ResourceT io ()
vkaRegisterDebugCallback vulkanInstance flags debugCallback = do
  debugCallbackPtr <- allocate_ (newVkDebugReportCallbackEXT debugCallback) freeHaskellFunPtr
  void . allocateAcquireVk_ (registeredDebugReportCallbackResource vulkanInstance) $
    createVk $
    set @"sType" VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT &*
    set @"pNext" VK_NULL &*
    set @"flags" flags &*
    set @"pfnCallback" debugCallbackPtr

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

vulkanInstanceResource :: VkaResource VkInstanceCreateInfo VkInstance
vulkanInstanceResource = simpleVkaResource_ vkCreateInstance vkDestroyInstance "vkCreateInstance"

initStandardInstanceCreateInfo :: CreateVkStruct VkInstanceCreateInfo '["sType", "pNext"] ()
initStandardInstanceCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardApplicationInfo :: CreateVkStruct VkApplicationInfo '["sType", "pNext"] ()
initStandardApplicationInfo =
  set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO &*
  set @"pNext" VK_NULL

registeredDebugReportCallbackResource :: VkInstance -> VkaResource VkDebugReportCallbackCreateInfoEXT VkDebugReportCallbackEXT
registeredDebugReportCallbackResource vulkanInstance =
  VkaResource
    (vkGetInstanceProc @VkCreateDebugReportCallbackEXT vulkanInstance <*> pure vulkanInstance)
    (vkGetInstanceProc @VkDestroyDebugReportCallbackEXT vulkanInstance <*> pure vulkanInstance)
    "vkCreateDebugReportCallbackEXT"
    [VK_SUCCESS]

deviceResource :: VkPhysicalDevice -> VkaResource VkDeviceCreateInfo VkDevice
deviceResource = simpleParamVkaResource_ vkCreateDevice (const vkDestroyDevice) "vkCreateDevice"

initStandardDeviceCreateInfo :: CreateVkStruct VkDeviceCreateInfo '["sType", "pNext", "flags", "enabledLayerCount", "ppEnabledLayerNames"] ()
initStandardDeviceCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" zeroBits &*
  set @"enabledLayerCount" 0 &*
  set @"ppEnabledLayerNames" VK_NULL

initStandardDeviceQueueCreateInfo :: CreateVkStruct VkDeviceQueueCreateInfo '["sType", "pNext"] ()
initStandardDeviceQueueCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO &*
  set @"pNext" VK_NULL

commandPoolResource :: VkDevice -> VkaResource VkCommandPoolCreateInfo VkCommandPool
commandPoolResource = simpleParamVkaResource_ vkCreateCommandPool vkDestroyCommandPool "vkCreateCommandPool"

initStandardCommandPoolCreateInfo :: CreateVkStruct VkCommandPoolCreateInfo '["sType", "pNext"] ()
initStandardCommandPoolCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO &*
  set @"pNext" VK_NULL

descriptorSetLayoutResource :: VkDevice -> VkaResource VkDescriptorSetLayoutCreateInfo VkDescriptorSetLayout
descriptorSetLayoutResource = simpleParamVkaResource_ vkCreateDescriptorSetLayout vkDestroyDescriptorSetLayout "vkCreateDescriptorSetLayout"

initStandardDescriptorSetLayoutCreateInfo :: CreateVkStruct VkDescriptorSetLayoutCreateInfo '["sType", "pNext"] ()
initStandardDescriptorSetLayoutCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO &*
  set @"pNext" VK_NULL

pipelineLayoutResource :: VkDevice -> VkaResource VkPipelineLayoutCreateInfo VkPipelineLayout
pipelineLayoutResource = simpleParamVkaResource_ vkCreatePipelineLayout vkDestroyPipelineLayout "vkCreatePipelineLayout"

initStandardPipelineLayoutCreateInfo :: CreateVkStruct VkPipelineLayoutCreateInfo '["sType", "pNext", "flags"] ()
initStandardPipelineLayoutCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" zeroBits

allocatedMemoryResource :: VkDevice -> VkaResource VkMemoryAllocateInfo VkDeviceMemory
allocatedMemoryResource = simpleParamVkaResource_ vkAllocateMemory vkFreeMemory "vkAllocateMemory"

initStandardMemoryAllocateInfo :: CreateVkStruct VkMemoryAllocateInfo '["sType", "pNext"] ()
initStandardMemoryAllocateInfo =
  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO &*
  set @"pNext" VK_NULL

bufferResource :: VkDevice -> VkaResource VkBufferCreateInfo VkBuffer
bufferResource = simpleParamVkaResource_ vkCreateBuffer vkDestroyBuffer "vkCreateBuffer"

initStandardBufferCreateInfo :: CreateVkStruct VkBufferCreateInfo '["sType", "pNext"] ()
initStandardBufferCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO &*
  set @"pNext" VK_NULL

-- Generalize this and the next function once the CreateVkStruct type is redesigned.
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

setImageSharingQueueFamilyIndices ::
  (
    CanWriteField "imageSharingMode" a,
    CanWriteField "queueFamilyIndexCount" a,
    CanWriteField "pQueueFamilyIndices" a,
    FieldType "imageSharingMode" a ~ VkSharingMode,
    FieldType "queueFamilyIndexCount" a ~ Word32,
    FieldType "pQueueFamilyIndices" a ~ Ptr Word32
  ) =>
  [Word32] ->
  CreateVkStruct a '["imageSharingMode", "queueFamilyIndexCount", "pQueueFamilyIndices"] ()
setImageSharingQueueFamilyIndices qfis =
  set @"imageSharingMode" (if null qfis' then VK_SHARING_MODE_EXCLUSIVE else VK_SHARING_MODE_CONCURRENT) &*
  setListCountAndRef @"queueFamilyIndexCount" @"pQueueFamilyIndices" qfis'
  where
    -- If just one QFI is provided, it's the same as providing none; both are exclusive mode,
    -- and the QFI list is ignored in that case.
    qfis' = if length qfis > 1 then qfis else []

imageResource :: VkDevice -> VkaResource VkImageCreateInfo VkImage
imageResource = simpleParamVkaResource_ vkCreateImage vkDestroyImage "vkCreateImage"

initStandardImageCreateInfo :: CreateVkStruct VkImageCreateInfo '["sType", "pNext"] ()
initStandardImageCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO &*
  set @"pNext" VK_NULL

allocateAndBindVulkanMemory ::
  MonadUnliftIO io =>
  (VkDevice -> obj -> Ptr VkMemoryRequirements -> IO ()) ->
  (VkDevice -> obj -> VkDeviceMemory -> VkDeviceSize -> IO ()) ->
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  obj ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe VkDeviceMemory)
allocateAndBindVulkanMemory getMemoryRequirements bindMemory device pdmp obj qualification =
  getVk (getMemoryRequirements device obj) >>= \memReqs ->
  lift (
    pickByM qualification .
    filter (testBit (getField @"memoryTypeBits" memReqs) . fst) .
    take (fromIntegral $ getField @"memoryTypeCount" pdmp) $
    getFieldArrayAssocs @"memoryTypes" pdmp
  ) >>=
    mapM (\(chosenMemoryTypeIndex, _) -> do
      memory <-
        allocateAcquireVk_ (allocatedMemoryResource device) $
        createVk $
        initStandardMemoryAllocateInfo &*
        set @"allocationSize" (getField @"size" memReqs) &*
        set @"memoryTypeIndex" (fromIntegral chosenMemoryTypeIndex)
      liftIO $ bindMemory device obj memory 0
      return memory
    )

allocateAndBindBufferMemory ::
  MonadUnliftIO io =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  VkBuffer ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe VkDeviceMemory)
allocateAndBindBufferMemory = allocateAndBindVulkanMemory vkGetBufferMemoryRequirements vkaBindBufferMemory

vkaBindBufferMemory :: VkDevice -> VkBuffer -> VkDeviceMemory -> VkDeviceSize -> IO ()
vkaBindBufferMemory device buffer memory memoryOffset =
  vkBindBufferMemory device buffer memory memoryOffset & onVkFailureThrow_ "vkBindBufferMemory"

allocateAndBindImageMemory ::
  MonadUnliftIO io =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  VkImage ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe VkDeviceMemory)
allocateAndBindImageMemory = allocateAndBindVulkanMemory vkGetImageMemoryRequirements vkaBindImageMemory

vkaBindImageMemory :: VkDevice -> VkImage -> VkDeviceMemory -> VkDeviceSize -> IO ()
vkaBindImageMemory device buffer memory memoryOffset =
  vkBindImageMemory device buffer memory memoryOffset & onVkFailureThrow_ "vkBindImageMemory"

createBoundBuffer ::
  (MonadUnliftIO m, MonadThrow m) =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  QualificationM m (Int, VkMemoryType) ->
  VkBufferCreateInfo ->
  ResourceT m (VkBuffer, VkDeviceMemory)
createBoundBuffer device pdmp qualification bufferCreateInfo = runResourceT $ do
  (bufferReleaseKey, buffer) <- allocateAcquireVk (bufferResource device) bufferCreateInfo
  memory <-
    lift $
    fromMaybeM (throwVkaExceptionM "Failed to find a suitable memory type for the buffer.") $
    allocateAndBindBufferMemory device pdmp buffer qualification
  -- To force the buffer to be freed before the memory it's bound to.
  lift . register_ =<< fromJust <$> unprotect bufferReleaseKey
  return (buffer, memory)

createStagingBuffer ::
  (MonadUnliftIO m, MonadThrow m) =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  [Word32] ->
  VkDeviceSize ->
  ResourceT m (VkBuffer, VkDeviceMemory)
createStagingBuffer device pdmp qfis size =
  createBoundBuffer device pdmp (
    return . allAreSet (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) . getField @"propertyFlags" . snd,
    \_ _ -> return EQ
  ) $
  createVk $
  initStandardBufferCreateInfo &*
  set @"flags" zeroBits &*
  set @"size" size &*
  set @"usage" VK_BUFFER_USAGE_TRANSFER_SRC_BIT &*
  setSharingQueueFamilyIndices qfis

createUniformBuffer ::
  (MonadUnliftIO m, MonadThrow m) =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  [Word32] ->
  VkDeviceSize ->
  ResourceT m (VkBuffer, VkDeviceMemory)
createUniformBuffer device pdmp qfis size =
  createBoundBuffer device pdmp (
    return . allAreSet (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) . getField @"propertyFlags" . snd,
    \_ _ -> return EQ
  ) $
  createVk $
  initStandardBufferCreateInfo &*
  set @"flags" zeroBits &*
  set @"size" size &*
  set @"usage" VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT &*
  setSharingQueueFamilyIndices qfis

createUniformBufferForPrimBytes ::
  forall pb m.
  (MonadUnliftIO m, MonadThrow m, PrimBytes pb) =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  [Word32] ->
  ResourceT m (VkBuffer, VkDeviceMemory)
createUniformBufferForPrimBytes device pdmp qfis = createUniformBuffer device pdmp qfis (bSizeOf @pb undefined)

createFilledBuffer ::
  (MonadUnliftIO m, MonadThrow m, MonadFail m) =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  VkCommandPool ->
  VkQueue ->
  VkBufferUsageFlags ->
  [Word32] ->
  VkDeviceSize ->
  (Ptr Void -> IO ()) ->
  ResourceT m (VkBuffer, VkDeviceMemory)
createFilledBuffer device pdmp commandPool queue usageFlags qfis dataSize fillBuffer = runResourceT $ do
  (stagingBuffer, stagingBufferMemory) <- createStagingBuffer device pdmp [] dataSize

  liftIO $ with (mappedMemory device stagingBufferMemory 0 dataSize) fillBuffer

  (buffer, bufferMemory) <-
    lift $ createBoundBuffer device pdmp (
      return . allAreSet VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT . getField @"propertyFlags" . snd,
      \_ _ -> return EQ
    ) $
    createVk $
    initStandardBufferCreateInfo &*
    set @"flags" zeroBits &*
    set @"size" dataSize &*
    set @"usage" (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. usageFlags) &*
    setSharingQueueFamilyIndices qfis

  executeCommands device commandPool queue $ \commandBuffer -> liftIO $
    vkaCmdCopyBuffer commandBuffer stagingBuffer buffer [createVk $ set @"size" dataSize &* set @"srcOffset" 0 &* set @"dstOffset" 0]

  return (buffer, bufferMemory)

createFilledBufferFromPrimBytes ::
  (MonadUnliftIO m, MonadThrow m, MonadFail m, Storable pb, PrimBytes pb) =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  VkCommandPool ->
  VkQueue ->
  VkBufferUsageFlags ->
  [Word32] ->
  pb ->
  ResourceT m (VkBuffer, VkDeviceMemory)
createFilledBufferFromPrimBytes device pdmp commandPool queue usageFlags qfis pb =
  createFilledBuffer device pdmp commandPool queue usageFlags qfis (bSizeOf pb) (flip poke pb . castPtr)

createBoundImage ::
  (MonadUnliftIO m, MonadThrow m) =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  QualificationM m (Int, VkMemoryType) ->
  VkImageCreateInfo ->
  ResourceT m (VkImage, VkDeviceMemory)
createBoundImage device pdmp qualification imageCreateInfo = runResourceT $ do
  (imageReleaseKey, image) <- allocateAcquireVk (imageResource device) imageCreateInfo
  memory <-
    lift $
    fromMaybeM (throwVkaExceptionM "Failed to find a suitable memory type for the image.") $
    allocateAndBindImageMemory device pdmp image qualification
  -- To force the image to be freed before the memory it's bound to.
  lift . register_ =<< fromJust <$> unprotect imageReleaseKey
  return (image, memory)

vkaMapMemory :: VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> VkMemoryMapFlags -> IO (Ptr Void)
vkaMapMemory device deviceMemory offset size flags =
  alloca $ \ptrPtr -> do
    vkMapMemory device deviceMemory offset size flags ptrPtr & onVkFailureThrow_ "vkMapMemory"
    peek ptrPtr

mappedMemory :: VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> Acquire (Ptr Void)
mappedMemory device deviceMemory offset size =
  vkaMapMemory device deviceMemory offset size zeroBits
  `mkAcquire`
  const (vkUnmapMemory device deviceMemory)

imageViewResource :: VkDevice -> VkaResource VkImageViewCreateInfo VkImageView
imageViewResource = simpleParamVkaResource_ vkCreateImageView vkDestroyImageView "vkCreateImageView"

initStandardImageViewCreateInfo :: CreateVkStruct VkImageViewCreateInfo '["sType", "pNext"] ()
initStandardImageViewCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO &*
  set @"pNext" VK_NULL

samplerResource :: VkDevice -> VkaResource VkSamplerCreateInfo VkSampler
samplerResource = simpleParamVkaResource_ vkCreateSampler vkDestroySampler "vkCreateSampler"

initStandardSamplerCreateInfo :: CreateVkStruct VkSamplerCreateInfo '["sType", "pNext"] ()
initStandardSamplerCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO &*
  set @"pNext" VK_NULL

allocatedCommandBuffers :: VkDevice -> VkCommandBufferAllocateInfo -> Acquire (VkaIArray VkCommandBuffer)
allocatedCommandBuffers device allocateInfo =
  fmap VkaIArray $
  do
    array <- newArray_ (0, commandBufferCount-1)
    when (commandBufferCount > 0) $
      withPtr allocateInfo $ \allocateInfoPtr ->
        withStorableArray array (vkAllocateCommandBuffers device allocateInfoPtr) & onVkFailureThrow_ "vkAllocateCommandBuffers"
    return array
  `mkAcquire`
  if commandBufferCount > 0 then
    \commandBufferArray -> withStorableArray commandBufferArray $ vkFreeCommandBuffers device commandPool commandBufferCount
  else
    const $ return ()

  where
    commandBufferCount = getField @"commandBufferCount" allocateInfo
    commandPool = getField @"commandPool" allocateInfo

initStandardCommandBufferAllocateInfo :: CreateVkStruct VkCommandBufferAllocateInfo '["sType", "pNext"] ()
initStandardCommandBufferAllocateInfo =
  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO &*
  set @"pNext" VK_NULL

recordingCommandBuffer :: VkCommandBuffer -> VkCommandBufferBeginInfo -> Acquire ()
recordingCommandBuffer commandBuffer beginInfo =
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

-- Warning: This will free the descriptor sets at the end of the ResourceT scope.  Only use this if the descriptor
-- pool was created with the VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag set.  If you don't have that
-- set, use allocateDescriptorSets instead.
allocatedDescriptorSets :: VkDevice -> VkDescriptorSetAllocateInfo -> Acquire (VkaIArray VkDescriptorSet)
allocatedDescriptorSets device allocateInfo =
  fmap VkaIArray $
  allocateDescriptorSetsSA device allocateInfo
  `mkAcquire`
  if descriptorSetCount > 0 then
    \descriptorSetArray ->
      withStorableArray descriptorSetArray (vkFreeDescriptorSets device descriptorPool descriptorSetCount) & onVkFailureThrow_ "vkFreeDescriptorSets"
  else
    const $ return ()

  where
    descriptorSetCount = getField @"descriptorSetCount" allocateInfo
    descriptorPool = getField @"descriptorPool" allocateInfo

allocateDescriptorSets :: VkDevice -> VkDescriptorSetAllocateInfo -> IO (VkaIArray VkDescriptorSet)
allocateDescriptorSets device allocateInfo = VkaIArray <$> allocateDescriptorSetsSA device allocateInfo

allocateDescriptorSetsSA :: VkDevice -> VkDescriptorSetAllocateInfo -> IO (StorableArray Word32 VkDescriptorSet)
allocateDescriptorSetsSA device allocateInfo = do
  array <- newArray_ (0, descriptorSetCount-1)
  when (descriptorSetCount > 0) $
    withPtr allocateInfo $ \allocateInfoPtr ->
      withStorableArray array (vkAllocateDescriptorSets device allocateInfoPtr) & onVkFailureThrow_ "vkAllocateDescriptorSets"
  return array

  where
    descriptorSetCount = getField @"descriptorSetCount" allocateInfo

vkaUpdateDescriptorSets :: VkDevice -> [VkWriteDescriptorSet] -> [VkCopyDescriptorSet] -> IO ()
vkaUpdateDescriptorSets device writes copies =
  withArray writes $ \writesPtr ->
  withArray copies $ \copiesPtr ->
    vkUpdateDescriptorSets device (lengthNum writes) writesPtr (lengthNum copies) copiesPtr

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

vkaQueueWaitIdle :: VkQueue -> IO ()
vkaQueueWaitIdle queue = vkQueueWaitIdle queue & onVkFailureThrow_ "vkQueueWaitIdle"

vkaQueueSubmit :: VkQueue -> VkFence -> [VkSubmitInfo] -> IO ()
vkaQueueSubmit queue fence submitInfos =
  withArray submitInfos $ \submitInfosPtr ->
  vkQueueSubmit queue (lengthNum submitInfos) submitInfosPtr fence & onVkFailureThrow_ "vkQueueSubmit"

initStandardSubmitInfo :: CreateVkStruct VkSubmitInfo '["sType", "pNext"] ()
initStandardSubmitInfo =
  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO &*
  set @"pNext" VK_NULL

vkaQueuePresentKHR :: VkQueue -> VkPresentInfoKHR -> IO VkResult
vkaQueuePresentKHR queue presentInfo =
  withPtr presentInfo $ \presentInfoPtr ->
  vkQueuePresentKHR queue presentInfoPtr & onVkFailureThrow "vkQueuePresentKHR" [VK_SUCCESS, VK_SUBOPTIMAL_KHR]

initStandardPresentInfoKHR :: CreateVkStruct VkPresentInfoKHR '["sType", "pNext"] ()
initStandardPresentInfoKHR =
  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR &*
  set @"pNext" VK_NULL

setSubmitWaitSemaphoresAndStageFlags ::
  [(VkSemaphore, VkPipelineStageFlags)] ->
  CreateVkStruct VkSubmitInfo '["waitSemaphoreCount", "pWaitSemaphores", "pWaitDstStageMask"] ()
setSubmitWaitSemaphoresAndStageFlags waitSemaphoresAndStageFlags =
  setListCountAndRef @"waitSemaphoreCount" @"pWaitSemaphores" (fst <$> waitSemaphoresAndStageFlags) &*
  setListRef @"pWaitDstStageMask" (snd <$> waitSemaphoresAndStageFlags)

fenceResource :: VkDevice -> VkaResource VkFenceCreateInfo VkFence
fenceResource = simpleParamVkaResource_ vkCreateFence vkDestroyFence "vkCreateFence"

initStandardFenceCreateInfo :: CreateVkStruct VkFenceCreateInfo '["sType", "pNext"] ()
initStandardFenceCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO &*
  set @"pNext" VK_NULL

setFenceSignaled :: Bool -> CreateVkStruct VkFenceCreateInfo '["flags"] ()
setFenceSignaled isSignaled = set @"flags" (if isSignaled then VK_FENCE_CREATE_SIGNALED_BIT else zeroBits)

createFence :: MonadIO m => VkDevice -> Bool -> ResourceT m VkFence
createFence device signaled = allocateAcquireVk_ (fenceResource device) $ createVk $ initStandardFenceCreateInfo &* setFenceSignaled signaled

vkaWaitForFences :: VkDevice -> [VkFence] -> VkBool32 -> Word64 -> IO VkResult
vkaWaitForFences device fences waitAll timeout =
  withArray fences $ \fencesPtr ->
  vkWaitForFences device (lengthNum fences) fencesPtr waitAll timeout & onVkFailureThrow "vkWaitForFences" [VK_SUCCESS, VK_TIMEOUT]

vkaWaitForFence :: VkDevice -> VkFence -> Word64 -> IO VkResult
vkaWaitForFence device fence timeout = vkaWaitForFences device [fence] VK_TRUE timeout

vkaResetFences :: VkDevice -> [VkFence] -> IO ()
vkaResetFences device fences =
  withArray fences $ \fencesPtr ->
  vkResetFences device (lengthNum fences) fencesPtr & onVkFailureThrow_ "vkResetFences"

vkaResetFence :: VkDevice -> VkFence -> IO ()
vkaResetFence device fence = vkaResetFences device [fence]

vkaDeviceWaitIdle :: VkDevice -> IO ()
vkaDeviceWaitIdle device = vkDeviceWaitIdle device & onVkFailureThrow_ "vkDeviceWaitIdle"

semaphoreResource :: VkDevice -> VkaResource VkSemaphoreCreateInfo VkSemaphore
semaphoreResource = simpleParamVkaResource_ vkCreateSemaphore vkDestroySemaphore "vkCreateSemaphore"

initStandardSemaphoreCreateInfo :: CreateVkStruct VkSemaphoreCreateInfo '["sType", "pNext"] ()
initStandardSemaphoreCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO &*
  set @"pNext" VK_NULL

createSemaphore :: MonadIO m => VkDevice -> ResourceT m VkSemaphore
createSemaphore device = allocateAcquireVk_ (semaphoreResource device) $ createVk initStandardSemaphoreCreateInfo

swapchainResource :: VkDevice -> VkaResource VkSwapchainCreateInfoKHR VkSwapchainKHR
swapchainResource = simpleParamVkaResource_ vkCreateSwapchainKHR vkDestroySwapchainKHR "vkCreateSwapchainKHR"

initStandardSwapchainCreateInfo :: CreateVkStruct VkSwapchainCreateInfoKHR '["sType", "pNext"] ()
initStandardSwapchainCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR &*
  set @"pNext" VK_NULL

descriptorPoolResource :: VkDevice -> VkaResource VkDescriptorPoolCreateInfo VkDescriptorPool
descriptorPoolResource = simpleParamVkaResource_ vkCreateDescriptorPool vkDestroyDescriptorPool "vkCreateDescriptorPool"

initStandardDescriptorPoolCreateInfo :: CreateVkStruct VkDescriptorPoolCreateInfo '["sType", "pNext"] ()
initStandardDescriptorPoolCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO &*
  set @"pNext" VK_NULL

renderPassResource :: VkDevice -> VkaResource VkRenderPassCreateInfo VkRenderPass
renderPassResource = simpleParamVkaResource_ vkCreateRenderPass vkDestroyRenderPass "vkCreateRenderPass"

initStandardRenderPassCreateInfo :: CreateVkStruct VkRenderPassCreateInfo '["sType", "pNext", "flags"] ()
initStandardRenderPassCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" zeroBits

vkaCreateGraphicsPipelines :: VkDevice -> VkPipelineCache -> [VkGraphicsPipelineCreateInfo] -> IO (VkaIArray VkPipeline)
vkaCreateGraphicsPipelines device pipelineCache createInfos@(lengthNum -> count) =
  withArray createInfos $ \createInfosPtr -> do
    array <- newArray_ (0, count-1)
    when (count > 0) $
      withStorableArray array (vkCreateGraphicsPipelines device pipelineCache count createInfosPtr VK_NULL) & onVkFailureThrow_ "vkCreateGraphicsPipelines"
    return $ VkaIArray array

registerGraphicsPipelineForDestruction :: MonadResource m => VkDevice -> VkPipeline -> m ReleaseKey
registerGraphicsPipelineForDestruction device pipeline = register $ vkDestroyPipeline device pipeline VK_NULL

registerGraphicsPipelineForDestruction_ :: MonadResource m => VkDevice -> VkPipeline -> m ()
registerGraphicsPipelineForDestruction_ = void .: registerGraphicsPipelineForDestruction

initStandardGraphicsPipelineCreateInfo :: CreateVkStruct VkGraphicsPipelineCreateInfo '["sType", "pNext"] ()
initStandardGraphicsPipelineCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO &*
  set @"pNext" VK_NULL

shaderModuleResource :: VkDevice -> VkaResource VkShaderModuleCreateInfo VkShaderModule
shaderModuleResource = simpleParamVkaResource_ vkCreateShaderModule vkDestroyShaderModule "vkCreateShaderModule"

initStandardShaderModuleCreateInfo :: CreateVkStruct VkShaderModuleCreateInfo '["sType", "pNext", "flags"] ()
initStandardShaderModuleCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" zeroBits

createShaderModuleFromFile :: MonadUnliftIO m => VkDevice -> FilePath -> ResourceT m VkShaderModule
createShaderModuleFromFile device filePath = runResourceT $ do
  SizedArray{..} <- fillArrayFromSpirvFile filePath
  lift $ allocateAcquireVk_ (shaderModuleResource device) $
    createVk $
    initStandardShaderModuleCreateInfo &*
    set @"codeSize" (fromIntegral arraySize) &*
    set @"pCode" (castPtr arrayPtr)

fillArrayFromSpirvFile :: MonadUnliftIO m => FilePath -> ResourceT m (SizedArray Word8)
fillArrayFromSpirvFile filePath = runResourceT $ do
  h <- allocate_ (openBinaryFile filePath ReadMode) hClose

  -- Vulkan requires SPIR-V bytecode to have an alignment of 4 bytes.
  alignedSize <- liftIO $ alignTo 4 . fromIntegral <$> hFileSize h
  array <- lift $ allocateAcquire_ (acquireSizedArray @Word8 alignedSize)

  let ptr = arrayPtr array

  liftIO $ do
    bytesRead <- hGetBuf h ptr alignedSize
    pokeArray @Word8 (plusPtr ptr bytesRead) $ replicate (alignedSize - bytesRead) 0

  return array

initStandardPipelineShaderStageCreateInfo :: CreateVkStruct VkPipelineShaderStageCreateInfo '["sType", "pNext"] ()
initStandardPipelineShaderStageCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineVertexInputStateCreateInfo :: CreateVkStruct VkPipelineVertexInputStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineVertexInputStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineInputAssemblyStateCreateInfo :: CreateVkStruct VkPipelineInputAssemblyStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineInputAssemblyStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineViewportStateCreateInfo :: CreateVkStruct VkPipelineViewportStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineViewportStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineRasterizationStateCreateInfo :: CreateVkStruct VkPipelineRasterizationStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineRasterizationStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineMultisampleStateCreateInfo :: CreateVkStruct VkPipelineMultisampleStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineMultisampleStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineDepthStencilStateCreateInfo :: CreateVkStruct VkPipelineDepthStencilStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineDepthStencilStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineColorBlendStateCreateInfo :: CreateVkStruct VkPipelineColorBlendStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineColorBlendStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

framebufferResource :: VkDevice -> VkaResource VkFramebufferCreateInfo VkFramebuffer
framebufferResource = simpleParamVkaResource_ vkCreateFramebuffer vkDestroyFramebuffer "vkCreateFramebuffer"

initStandardFramebufferCreateInfo :: CreateVkStruct VkFramebufferCreateInfo '["sType", "pNext"] ()
initStandardFramebufferCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO &*
  set @"pNext" VK_NULL

executeCommands :: (MonadUnliftIO m, MonadFail m) => VkDevice -> VkCommandPool -> VkQueue -> (forall n. MonadIO n => VkCommandBuffer -> n a) -> m a
executeCommands device commandPool submissionQueue fillCommandBuffer = runResourceT $ do
  [commandBuffer] <-
    fmap vkaElems $
    allocateAcquire_ $ allocatedCommandBuffers device $
    createVk $
    initStandardCommandBufferAllocateInfo &*
    set @"commandPool" commandPool &*
    set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY &*
    set @"commandBufferCount" 1

  result <-
    with_ (
      recordingCommandBuffer commandBuffer $
      createVk $
      initPrimaryCommandBufferBeginInfo &*
      set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    )
    (fillCommandBuffer commandBuffer)

  executionCompleteFence <-
    allocateAcquireVk_ (fenceResource device) $
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
    vkaWaitForFence device executionCompleteFence maxBound & void

  return result

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

vkaCmdCopyBuffer ::
  VkCommandBuffer ->
  VkBuffer ->
  VkBuffer ->
  [VkBufferCopy] ->
  IO ()
vkaCmdCopyBuffer commandBuffer srcBuffer dstBuffer bufferCopies =
  withArray bufferCopies $ \bufferCopiesPtr ->
  vkCmdCopyBuffer commandBuffer srcBuffer dstBuffer (lengthNum bufferCopies) bufferCopiesPtr

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

stageKtxTexture ::
  (MonadUnliftIO m, MonadThrow m) =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  FilePath ->
  ResourceT m (KTX.Header, KTX.BufferRegions, VkBuffer, VkDeviceMemory)
stageKtxTexture device pdmp filePath =
  KTX.readKtxFile filePath KTX.skipMetadata $ \header () (fromIntegral -> textureDataSize) readTextureDataInto -> do
    (buffer, bufferMemory) <- lift . lift $ createStagingBuffer device pdmp [] textureDataSize
    bufferRegions <- with (mappedMemory device bufferMemory 0 textureDataSize) $ readTextureDataInto . castPtr
    return (header, bufferRegions, buffer, bufferMemory)

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
  (h@KTX.Header{..}, stagingBufferRegions, stagingBuffer, stagingBufferMemory) <- stageKtxTexture device pdmp filePath

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
    createBoundImage device pdmp (
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

  executeCommands device commandPool queue $ \commandBuffer -> liftIO $ do
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
    lift . allocateAcquireVk_ (imageViewResource device) $
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
    lift . allocateAcquireVk_ (samplerResource device) $
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

type VkaGetter vk r = Ptr vk -> IO r

getVkWithResult :: (MonadIO io, Storable vk) => VkaGetter vk r -> io (r, vk)
getVkWithResult get =
  liftIO $
  alloca $ \ptr -> do
    result <- get ptr
    value <- peek ptr
    return (result, value)

getVk :: (MonadIO io, Storable vk) => VkaGetter vk r -> io vk
getVk = fmap snd . getVkWithResult

onGetterFailureThrow :: String -> [VkResult] -> VkaGetter vk VkResult -> VkaGetter vk VkResult
onGetterFailureThrow functionName successResults get ptr = get ptr & onVkFailureThrow functionName successResults

vkaGetPhysicalDeviceSurfaceSupportKHR :: VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> VkaGetter VkBool32 VkResult
vkaGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi surface =
  vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi surface &
    onGetterFailureThrow "vkGetPhysicalDeviceSurfaceSupportKHR" [VK_SUCCESS]

vkaGetPhysicalDeviceSurfaceCapabilitiesKHR :: VkPhysicalDevice -> VkSurfaceKHR -> VkaGetter VkSurfaceCapabilitiesKHR VkResult
vkaGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface =
  vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface &
  onGetterFailureThrow "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" [VK_SUCCESS]

vkaAcquireNextImageKHR :: VkDevice -> VkSwapchainKHR -> Word64 -> VkSemaphore -> VkFence -> VkaGetter Word32 VkResult
vkaAcquireNextImageKHR device swapchain timeout semaphore fence =
  vkAcquireNextImageKHR device swapchain timeout semaphore fence &
  onGetterFailureThrow "vkAcquireNextImageKHR" [VK_SUCCESS, VK_TIMEOUT, VK_NOT_READY, VK_SUBOPTIMAL_KHR]

newtype VkaIArray vk = VkaIArray { unVkaIArray :: StorableArray Word32 vk }

vkaNumElements :: Storable vk => VkaIArray vk -> Word32
vkaNumElements = fromIntegral . unsafePerformIO . getNumElements . unVkaIArray

vkaElems :: Storable vk => VkaIArray vk -> [vk]
vkaElems = unsafePerformIO . getElems . unVkaIArray

vkaAssocs :: Storable vk => VkaIArray vk -> [(Word32, vk)]
vkaAssocs = unsafePerformIO . getAssocs . unVkaIArray

type VkaArrayFiller vk r = Ptr Word32 -> Ptr vk -> IO r

getVkArrayWithResult :: (MonadIO io, Storable vk) => VkaArrayFiller vk r -> io (r, VkaIArray vk)
getVkArrayWithResult fillArray =
  liftIO $
  alloca $ \countPtr -> do
    let fillArray' = fillArray countPtr
    getCountResult <- fillArray' VK_NULL
    count <- peek countPtr
    array <- newArray_ (0, count-1)
    if count > 0 then do
      fillArrayResult <- withStorableArray array fillArray'
      return (fillArrayResult, VkaIArray array)
    else
      return (getCountResult, VkaIArray array)

getVkArray :: (MonadIO io, Storable vk) => VkaArrayFiller vk r -> io (VkaIArray vk)
getVkArray = fmap snd . getVkArrayWithResult

-- When a VkaArrayFiller is used with getVkArray[WithResult], VK_INCOMPLETE should never be returned, since
-- getVkArray[WithResult] is checking for available count first. Thus, don't provide it as a success result.
onArrayFillerFailureThrow :: String -> [VkResult] -> VkaArrayFiller vk VkResult -> VkaArrayFiller vk VkResult
onArrayFillerFailureThrow functionName successResults fillArray countPtr arrayPtr = fillArray countPtr arrayPtr & onVkFailureThrow functionName successResults

vkaEnumeratePhysicalDevices :: VkInstance -> VkaArrayFiller VkPhysicalDevice VkResult
vkaEnumeratePhysicalDevices = onArrayFillerFailureThrow "vkEnumeratePhysicalDevices" [VK_SUCCESS] . vkEnumeratePhysicalDevices

vkaGetPhysicalDeviceSurfaceFormatsKHR :: VkPhysicalDevice -> VkSurfaceKHR -> VkaArrayFiller VkSurfaceFormatKHR VkResult
vkaGetPhysicalDeviceSurfaceFormatsKHR = onArrayFillerFailureThrow "vkGetPhysicalDeviceSurfaceFormatsKHR" [VK_SUCCESS] .: vkGetPhysicalDeviceSurfaceFormatsKHR

vkaGetPhysicalDeviceSurfacePresentModesKHR :: VkPhysicalDevice -> VkSurfaceKHR -> VkaArrayFiller VkPresentModeKHR VkResult
vkaGetPhysicalDeviceSurfacePresentModesKHR = onArrayFillerFailureThrow "vkGetPhysicalDeviceSurfacePresentModesKHR" [VK_SUCCESS] .: vkGetPhysicalDeviceSurfacePresentModesKHR

vkaGetSwapchainImagesKHR :: VkDevice -> VkSwapchainKHR -> VkaArrayFiller VkImage VkResult
vkaGetSwapchainImagesKHR = onArrayFillerFailureThrow "vkGetSwapchainImagesKHR" [VK_SUCCESS] .: vkGetSwapchainImagesKHR

pdmpDeviceLocalMemorySize :: VkPhysicalDeviceMemoryProperties -> VkDeviceSize
pdmpDeviceLocalMemorySize memoryProperties =
  iwfoldr @VkMemoryHeap @'[_] @'[] (
    \(Idx i :* U) (S x) s ->
      if i < memoryHeapCount && isDeviceLocal x then s + getField @"size" x else s
  ) 0 .
  getVec @"memoryHeaps" $
  memoryProperties
  where
    isDeviceLocal = (zeroBits /=) . (VK_MEMORY_HEAP_DEVICE_LOCAL_BIT .&.) . getField @"flags"
    memoryHeapCount = fromIntegral $ getField @"memoryHeapCount" memoryProperties

getFieldArrayAssocs :: forall fname a. CanReadFieldArray fname a => a -> [(Int, FieldType fname a)]
getFieldArrayAssocs a = [0 .. fieldArrayLength @fname @a] <&> \i -> (i, getFieldArrayUnsafe @fname i a)

getFieldArrayElems :: forall fname a. CanReadFieldArray fname a => a -> [(FieldType fname a)]
getFieldArrayElems = fmap snd . getFieldArrayAssocs @fname @a
-- Vulkan helpers<
