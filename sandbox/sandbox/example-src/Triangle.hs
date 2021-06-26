{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude.Local

import Paths_sandbox

import ApplicationException
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource.Local
import Data.Acquire.Local
import Data.Bits.Local
import Data.Foldable
import Data.Function
import Data.Functor
import Data.IORef
import Data.List
import Data.Maybe
import Data.Reflection
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

import GHC.Generics (Generic)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFWAux

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
#ifndef NDEBUG
import Graphics.Vulkan.Ext.VK_EXT_debug_report
#endif
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create.Local
import Graphics.Vulkan.Marshal.Create.DataFrame
import Graphics.VulkanAux

import Numeric.DataFrame hiding (sortBy)

import System.Clock
import UnliftIO.Exception

main :: IO ()
main =
  runResourceT resourceMain
  `catch` (
    \(e :: ApplicationException) ->
      putStrLn $ displayException e
  )
  `catch` (
    \(e :: VkaResultException) ->
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

  allocateAcquire_ acquireInitializedGLFW
  ioPutStrLn "GLFW initialized."

  vulkanInstance <- do
    glfwExtensions <- liftIO GLFW.getRequiredInstanceExtensions

    vkaAllocateResource_ vkaInstanceResource $
      createVk $
      initStandardInstanceCreateInfo &*
      setVkRef @"pApplicationInfo" (
        createVk $
        initStandardApplicationInfo &*
        setStrRef @"pApplicationName" "Example - Triangle" &*
        set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0) &*
        setStrRef @"pEngineName" "" &*
        set @"engineVersion" 0 &*
        set @"apiVersion" VK_API_VERSION_1_1
      ) &*
      setStrListCountAndRef @"enabledLayerCount" @"ppEnabledLayerNames" (
#ifndef NDEBUG
        ["VK_LAYER_KHRONOS_validation"]
#else
        []
#endif
      ) &*
      setListCountAndRef @"enabledExtensionCount" @"ppEnabledExtensionNames" (
        glfwExtensions
#ifndef NDEBUG
        ++
        [
          VK_EXT_DEBUG_REPORT_EXTENSION_NAME
        ]
#endif
      )
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

  physicalDeviceArray <- vkaGetArray_ (vkaEnumeratePhysicalDevices vulkanInstance)

  physicalDeviceInfo@(PhysicalDeviceInfo physicalDevice physicalDeviceProperties physicalDeviceMemoryProperties physicalDeviceFeatures) <-
    liftIO $
    forM (vkaElems physicalDeviceArray) (\pd ->
      liftM3 (PhysicalDeviceInfo pd)
        (vkaGet_ . vkGetPhysicalDeviceProperties $ pd)
        (vkaGet_ . vkGetPhysicalDeviceMemoryProperties $ pd)
        (vkaGet_ . vkGetPhysicalDeviceFeatures $ pd)
    ) <&>
    sortBy (
      mconcat [
        prefer [VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU, VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU] `on` getField @"deviceType" . physicalDeviceInfo'properties,
        compare `on` vkaGetPhysicalDeviceLocalMemorySize . physicalDeviceInfo'memoryProperties
      ]
    ) <&>
    headOr (throwAppEx "No physical device found")
  ioPutStrLn "Physical device selected."

  window <- allocateAcquire_ $ acquireVulkanGLFWWindow 800 600 "Vulkan Sandbox"
  ioPutStrLn "Window created."

  lastWindowResizeTimeRef <- liftIO $ newIORef Nothing

  liftIO $ GLFW.setFramebufferSizeCallback window $ Just $ \_ _ _ -> do
    time <- getTime Monotonic
    writeIORef lastWindowResizeTimeRef $ Just time
  ioPutStrLn "Window framebuffer size callback registered."

  windowSurface <- allocateAcquire_ $ newVulkanGLFWWindowSurface vulkanInstance window
  ioPutStrLn "Window surface created."

  physicalDeviceQueueFamilyPropertiesArray <- vkaGetArray_ (vkGetPhysicalDeviceQueueFamilyProperties physicalDevice)

  qfis@[graphicsQfi, computeQfi, transferQfi, presentQfi] <-
    minimumBy (compare `on` length . nub) . fmap (fmap fst) <$> selectionsFromM (vkaAssocs physicalDeviceQueueFamilyPropertiesArray) [
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
        \(qfi, _) -> (VK_TRUE ==) <$> vkaGet_ (vkaGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi windowSurface),
        \_ _ -> return EQ
      )
    ]
  ioPutStrLn "Queue family indices selected."

  device <-
    vkaAllocateResource_ (vkaDeviceResource physicalDevice) $
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
    setListCountAndRef @"enabledExtensionCount" @"ppEnabledExtensionNames"
      [
        VK_KHR_SWAPCHAIN_EXTENSION_NAME
      ] &*
    setVkRef @"pEnabledFeatures" (
      createVk $
      copyField @"fillModeNonSolid" physicalDeviceFeatures &*
      copyField @"wideLines" physicalDeviceFeatures
    )
  ioPutStrLn "Vulkan device created."

  give device $ rest qfis graphicsQfi presentQfi lastWindowResizeTimeRef physicalDeviceInfo window windowSurface

rest :: Given VkDevice => [Word32] -> Word32 -> Word32 -> IORef (Maybe TimeSpec) -> PhysicalDeviceInfo -> GLFW.Window -> VkSurfaceKHR -> ResourceT IO ()
rest qfis graphicsQfi presentQfi lastWindowResizeTimeRef (PhysicalDeviceInfo physicalDevice physicalDeviceProperties physicalDeviceMemoryProperties physicalDeviceFeatures) window windowSurface = do
  [(graphicsQueue, graphicsCommandPool), (computeQueue, computeCommandPool), (transferQueue, transferCommandPool), (presentQueue, presentCommandPool)] <-
    forM qfis $ \qfi -> liftM2 (,)
      (vkaGetDeviceQueue qfi 0)
      (
        vkaAllocateResource_ vkaCommandPoolResource $
        createVk $
        initStandardCommandPoolCreateInfo &*
        set @"flags" zeroBits &*
        set @"queueFamilyIndex" qfi
      )
  ioPutStrLn "Device queues obtained, and corresponding command pools created."

  descriptorSetLayout <-
    vkaAllocateResource_ vkaDescriptorSetLayoutResource $
    createVk $
    initStandardDescriptorSetLayoutCreateInfo &*
    set @"flags" zeroBits &*
    setListCountAndRef @"bindingCount" @"pBindings" (
      fmap createVk [
        set @"binding" 0 &*
        set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER &*
        set @"descriptorCount" 1 &*
        set @"stageFlags" VK_SHADER_STAGE_VERTEX_BIT &*
        set @"pImmutableSamplers" VK_NULL
      ]
    )
  ioPutStrLn "Descriptor set layout created."

  pipelineLayout <-
    vkaAllocateResource_ vkaPipelineLayoutResource $
    createVk $
    initStandardPipelineLayoutCreateInfo &*
    setListCountAndRef @"setLayoutCount" @"pSetLayouts" [descriptorSetLayout] &*
    setListCountAndRef @"pushConstantRangeCount" @"pPushConstantRanges" []
  ioPutStrLn "Pipeline layout created."

  (vertexBuffer, vertexBufferMemory) <-
    vkaCreateFilledBufferFromPrimBytes
      physicalDeviceMemoryProperties
      transferCommandPool
      transferQueue
      VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
      [graphicsQfi]
      (
        packDF @Vertex @3 @'[]
          (svertex (vec2 0 (-1)) (vec3 0 0 1))
          (svertex (vec2 (-1) 1) (vec3 0 1 0))
          (svertex (vec2 1 1) (vec3 1 0 0))
      )
  ioPutStrLn "Vertex buffer created."

  (indexBuffer, indexBufferMemory) <-
    vkaCreateFilledBufferFromPrimBytes
      physicalDeviceMemoryProperties
      transferCommandPool
      transferQueue
      VK_BUFFER_USAGE_INDEX_BUFFER_BIT
      [graphicsQfi]
      (packDF @Word32 @3 @'[] 0 1 2)
  ioPutStrLn "Index buffer created."

  frameSyncs <- replicateM maxFramesInFlight $ FrameSync <$> vkaCreateFence True <*> vkaCreateSemaphore <*> vkaCreateSemaphore
  ioPutStrLn "Frame syncs created."

  renderStartTimeRef <- liftIO $ newIORef Nothing

  doWhileM $ runResourceT $ do
    (windowFramebufferWidth, windowFramebufferHeight) <- liftIO $ GLFW.getFramebufferSize window
    ioPutStrLn "Window framebuffer size obtained."

    surfaceCapabilities <- vkaGet_ $ vkaGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice windowSurface
    surfaceFormatArray <- vkaGetArray_ $ vkaGetPhysicalDeviceSurfaceFormatsKHR physicalDevice windowSurface
    surfacePresentModeArray <- vkaGetArray_ $ vkaGetPhysicalDeviceSurfacePresentModesKHR physicalDevice windowSurface
    ioPutStrLn "Surface info obtained."

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

      swapchainImageAspectRatio = fromIntegral (getField @"width" swapchainImageExtent) / fromIntegral (getField @"height" swapchainImageExtent)
    ioPutStrLn "Swapchain parameters determined."

    swapchain <-
      vkaAllocateResource_ vkaSwapchainResource $
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

    swapchainImageArray <- vkaGetArray_ $ vkaGetSwapchainImagesKHR swapchain

    swapchainImageViews <-
      forM (vkaElems swapchainImageArray) $ \image ->
        vkaAllocateResource_ vkaImageViewResource $
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

    let swapchainImageCountWord32@(fromIntegral -> swapchainImageCount) = vkaNumElements swapchainImageArray

    (uniformBuffers, uniformBufferMemories) <-
      fmap unzip . replicateM swapchainImageCount $
      vkaCreateUniformBufferForPrimBytes @UniformBufferObject physicalDeviceMemoryProperties []
    ioPutStrLn "Uniform buffers created."

    descriptorPool <-
      vkaAllocateResource_ vkaDescriptorPoolResource $
      createVk $
      initStandardDescriptorPoolCreateInfo &*
      set @"flags" zeroBits &*
      set @"maxSets" swapchainImageCountWord32 &*
      setListCountAndRef @"poolSizeCount" @"pPoolSizes" (
        createVk . (set @"descriptorCount" swapchainImageCountWord32 &*) <$> [
          set @"type" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
        ]
      )
    ioPutStrLn "Descriptor pool created."

    descriptorSets <-
      liftIO . fmap vkaElems . vkaAllocateDescriptorSets . createVk $
      initStandardDescriptorSetAllocateInfo &*
      set @"descriptorPool" descriptorPool &*
      setListCountAndRef @"descriptorSetCount" @"pSetLayouts" (replicate swapchainImageCount descriptorSetLayout)
    ioPutStrLn "Descriptor sets allocated."

    liftIO $
      vkaUpdateDescriptorSets
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
        vkaGet_ . vkGetPhysicalDeviceFormatProperties physicalDevice
      ) &
      fromMaybeM (throwAppEx "No 24-bit depth formats support being used by a depth attachment on this device.")
    ioPutStrLn $ "Depth format chosen: " ++ show depthFormat ++ "."

    renderPass <-
      vkaAllocateResource_ vkaRenderPassResource $
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
      vertShaderModule <- vkaCreateShaderModuleFromFile =<< liftIO (getDataFileName "shaders/triangle.vert.spv")
      ioPutStrLn "Vertex shader module created."
      fragShaderModule <- vkaCreateShaderModuleFromFile =<< liftIO (getDataFileName "shaders/triangle.frag.spv")
      ioPutStrLn "Fragment shader module created."

      liftIO $
        vkaElems <$> vkaCreateGraphicsPipelines VK_NULL_HANDLE (
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
                  set @"offset" (bFieldOffsetOf @"vertex'pos" @Vertex undefined),

                  set @"location" 1 &*
                  set @"format" VK_FORMAT_R32G32B32_SFLOAT &*
                  set @"offset" (bFieldOffsetOf @"vertex'color" @Vertex undefined)
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
    vkaRegisterGraphicsPipelineForDestruction_ graphicsPipeline
    ioPutStrLn "Graphics pipeline created."

    (depthImage, depthImageMemory) <-
      vkaCreateBoundImage physicalDeviceMemoryProperties (
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

    vkaExecuteCommands graphicsCommandPool graphicsQueue $ \commandBuffer -> liftIO $ do
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
      vkaAllocateResource_ vkaImageViewResource $
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
      vkaAllocateResource_ vkaFramebufferResource $
      createVk $
      initStandardFramebufferCreateInfo &*
      set @"renderPass" renderPass &*
      setListCountAndRef @"attachmentCount" @"pAttachments" [swapchainImageView, depthImageView] &*
      set @"width" (getField @"width" swapchainImageExtent) &*
      set @"height" (getField @"height" swapchainImageExtent) &*
      set @"layers" 1
    ioPutStrLn "Swapchain framebuffers created."

    swapchainCommandBuffers <-
      fmap vkaElems . allocateAcquire_ . vkaAllocatedCommandBuffers . createVk $
      initStandardCommandBufferAllocateInfo &*
      set @"commandPool" graphicsCommandPool &*
      set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY &*
      set @"commandBufferCount" (lengthNum swapchainImageViews)
    ioPutStrLn "Swapchain command buffers created."

    forM_ (zip3 swapchainCommandBuffers swapchainFramebuffers descriptorSets) $ \(commandBuffer, framebuffer, descriptorSet) ->
      with_ (
        vkaRecordingCommandBuffer commandBuffer . createVk $
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
        vkCmdDrawIndexed commandBuffer 3 1 0 0 0 -- 3 is the length of the dataframe used to fill the index buffer earlier.
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
            vkaWaitForFence frameSync'inFlightFence maxBound & void
            vkaResetFence frameSync'inFlightFence

            nextImageIndexWord32@(fromIntegral -> nextImageIndex) <- vkaGet_ $ vkaAcquireNextImageKHR swapchain maxBound frameSync'imageAvailableSemaphore VK_NULL_HANDLE

            secondsOffset <- liftIO $ (0.000000001 *) . fromInteger . toNanoSecs . (subtract renderStartTime) <$> getTime Monotonic

            with (vkaMappedMemory (uniformBufferMemories !! nextImageIndex) 0 (bSizeOf @UniformBufferObject undefined)) $ \ptr ->
              poke (castPtr ptr) . S $
              UniformBufferObject {
                uniformBufferObject'model = rotateZ (0.5 * pi * secondsOffset),
                uniformBufferObject'view = lookAt (vec3 0 (-1) 0) (vec3 0 0 (-2.5)) 0,
                uniformBufferObject'proj = perspective 0.1 256 (pi / 3) swapchainImageAspectRatio %* glToVk
              }

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
    vkaDeviceWaitIdle

    ioPutStrLn "Cleaning up swapchain-related objects."
    return shouldRebuildSwapchain

  ioPutStrLn "Cleaning up the rest."

maxFramesInFlight :: Int
maxFramesInFlight = 2

data Vertex =
  Vertex {
    vertex'pos :: Vec2f,
    vertex'color :: Vec3f
  } deriving (Eq, Show, Generic)

instance PrimBytes Vertex

type SVertex = Scalar Vertex

svertex :: Vec2f -> Vec3f -> SVertex
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

glToVk :: Mat44f
glToVk = mat44
  (vec4 1 0    0   0)
  (vec4 0 (-1) 0   0)
  (vec4 0 0    0.5 0.5)
  (vec4 0 0    0   1)

data PhysicalDeviceInfo =
  PhysicalDeviceInfo {
    physicalDeviceInfo'physicalDevice :: VkPhysicalDevice,
    physicalDeviceInfo'properties :: VkPhysicalDeviceProperties,
    physicalDeviceInfo'memoryProperties :: VkPhysicalDeviceMemoryProperties,
    physicalDeviceInfo'features :: VkPhysicalDeviceFeatures
  }
