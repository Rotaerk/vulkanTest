{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-cse #-} -- needed for cmdargs

module Main where

import Prelude hiding (init)
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Extra (firstJustM, findM, unlessM)
import Control.Monad.IO.Class
import Control.Monad.Loops (whileM_)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Data.Acquire
import Data.Bits
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.GLFW (WindowHint(..), ClientAPI(..))
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_EXT_debug_report
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Proc
import System.Console.CmdArgs.Implicit
import System.FilePath
import System.IO

data CommandLineArguments = CommandLineArguments { claShadersPath :: String } deriving (Show, Data, Typeable)

main :: IO ()
main =
  do
    arguments <-
      cmdArgs $
      CommandLineArguments {
        claShadersPath = def &= explicit &= name "shaderspath" &= typ "PATH" &= help "Path to the SPIR-V shader files."
      }
      &= summary "Vulkan Test"

    let shadersPath = claShadersPath arguments
    putStrLn $ "Shaders path is: '" ++ shadersPath ++ "'."

    runResourceT $ do
      -- Before initializing, probably should setErrorCallback
      allocateAcquire_ initializedGLFW
      ioPutStrLn "GLFW initialized."

      window <- allocateAcquire_ $ newVulkanGLFWWindow width height "Vulkan"
      ioPutStrLn "Window created."

      glfwExtensions <- liftIO $ GLFW.getRequiredInstanceExtensions

      unless (null validationLayers) $ do
        ensureValidationLayersSupported validationLayers
        ioPutStrLn "All required validation layers supported."

      vulkanInstance <-
        allocateAcquire_ $
        newVkInstance $
        configureVkInstance applicationInfo validationLayers (extensions ++ glfwExtensions)
      ioPutStrLn "Vulkan instance created."

-- #ifndef NDEBUG
      registerDebugCallback vulkanInstance
      ioPutStrLn "Debug callback registered."
-- #endif

      surface <- allocateAcquire_ $ newGLFWWindowSurface vulkanInstance window
      ioPutStrLn "Window surface obtained."

      (physicalDevice, qfi, scsd) <-
        mapM (liftIO . peekCString) deviceExtensions >>=
        getFirstSuitablePhysicalDeviceAndProperties vulkanInstance surface
      ioPutStrLn "Suitable physical device found."

      let distinctQfi = qfiDistinct qfi

      device <- allocateAcquire_ $ newVkDevice physicalDevice $ configureVkDevice distinctQfi deviceExtensions
      ioPutStrLn "Vulkan device created."

      graphicsQueue <- getDeviceQueue device (qfiGraphics qfi) 0
      ioPutStrLn "Graphics queue obtained."

      presentQueue <- getDeviceQueue device (qfiPresent qfi) 0
      ioPutStrLn "Present queue obtained."

      let
        surfaceCapabilities = scsdCapabilities scsd
        swapchainSurfaceFormat = chooseSwapchainSurfaceFormat (scsdSurfaceFormats scsd)
        swapchainImageFormat = getField @"format" swapchainSurfaceFormat
        swapchainPresentMode = chooseSwapchainPresentMode (scsdPresentModes scsd)
        swapchainExtent = chooseSwapchainExtent (fromIntegral width) (fromIntegral height) surfaceCapabilities
        swapchainImageCount = chooseSwapchainImageCount surfaceCapabilities

      swapchain <-
        allocateAcquire_ $
        newVkSwapchain device $
        configureVkSwapchain
          surface
          surfaceCapabilities
          swapchainSurfaceFormat
          swapchainPresentMode
          swapchainExtent
          swapchainImageCount
          distinctQfi
      ioPutStrLn "Swapchain created."

      swapchainImages <- listSwapchainImages device swapchain
      ioPutStrLn "Swapchain images created."

      swapchainImageViews <-
        allocateAcquire_ $
        newVkImageViews device $
        configureVkImageView swapchainImageFormat <$> swapchainImages
      ioPutStrLn "Swapchain image views created."

      (vertShaderModuleKey, vertShaderModule) <- createShaderModuleFromFile device (shadersPath </> "shader.vert.spv")
      ioPutStrLn "Vertex shader module created."
      (fragShaderModuleKey, fragShaderModule) <- createShaderModuleFromFile device (shadersPath </> "shader.frag.spv")
      ioPutStrLn "Fragment shader module created."

      renderPass <- allocateAcquire_ $ newRenderPass device $ configureRenderPass swapchainImageFormat
      ioPutStrLn "Render pass created."

      pipelineLayout <- allocateAcquire_ $ newPipelineLayout device $ configurePipelineLayout
      ioPutStrLn "Pipeline layout created."

      graphicsPipeline <-
        allocateAcquire_ $
        newGraphicsPipeline device $
        configureGraphicsPipeline
          [
            configurePipelineShaderStage VK_SHADER_STAGE_VERTEX_BIT vertShaderModule "main",
            configurePipelineShaderStage VK_SHADER_STAGE_FRAGMENT_BIT fragShaderModule "main"
          ]
          configurePipelineVertexInputState
          configurePipelineInputAssemblyState
          (configurePipelineViewportState swapchainExtent)
          configurePipelineRasterizationState
          configurePipelineMultisampleState
          configurePipelineColorBlendState
          renderPass
          pipelineLayout
      ioPutStrLn "Graphics pipeline created."

      release fragShaderModuleKey
      ioPutStrLn "Fragment shader module destroyed."
      release vertShaderModuleKey
      ioPutStrLn "Vertex shader module destroyed."

      swapchainFramebuffers <-
        allocateAcquire_ $
        forM swapchainImageViews $ \imageView ->
          newFramebuffer device $
          configureFramebuffer renderPass [imageView] swapchainExtent
      ioPutStrLn "Framebuffers created."

      commandPool <-
        allocateAcquire_ $
        newCommandPool device $
        configureCommandPool (qfiGraphics qfi) 0
      ioPutStrLn "Command pool created."

      commandBuffers <-
        allocateCommandBuffers device $
        configureCommandBuffers commandPool VK_COMMAND_BUFFER_LEVEL_PRIMARY (fromIntegral $ length swapchainFramebuffers)
      ioPutStrLn "Command buffers created."

      let
        commandBufferBeginInfo =
          createVk $
          set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO &*
          set @"pNext" VK_NULL &*
          set @"flags" VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT &*
          set @"pInheritanceInfo" VK_NULL

        renderPassBeginInfo :: VkFramebuffer -> VkRenderPassBeginInfo
        renderPassBeginInfo framebuffer =
          createVk $
          set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO &*
          set @"pNext" VK_NULL &*
          set @"renderPass" renderPass &*
          set @"framebuffer" framebuffer &*
          setVk @"renderArea" (
            setVk @"offset" (set @"x" 0 &* set @"y" 0) &*
            set @"extent" swapchainExtent
          ) &*
          set @"clearValueCount" 1 &*
          setListRef @"pClearValues" [
            createVk (
              setVk @"color" (
                setAt @"float32" @0 0 &*
                setAt @"float32" @1 0 &*
                setAt @"float32" @2 0 &*
                setAt @"float32" @3 1
              )
            )
          ]

      forM (zip commandBuffers swapchainFramebuffers) $ \(commandBuffer, swapchainFramebuffer) -> do
        beginCommandBuffer commandBuffer commandBufferBeginInfo
        cmdBeginRenderPass commandBuffer (renderPassBeginInfo swapchainFramebuffer) VK_SUBPASS_CONTENTS_INLINE
        liftIO $ vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
        liftIO $ vkCmdDraw commandBuffer 3 1 0 0
        liftIO $ vkCmdEndRenderPass commandBuffer
        endCommandBuffer commandBuffer
      ioPutStrLn "Command buffers filled."

      ioPutStrLn "Main loop starting."
      mainLoop window
      ioPutStrLn "Main loop ended, cleaning up."

  `catch` (
    \(e :: VulkanException) ->
      putStrLn $ displayException e
  )
  `catch` (
    \(e :: ApplicationException) ->
      putStrLn $ displayException e
  )
  where
    (width, height) = (800, 600)

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

    applicationInfo :: VkApplicationInfo
    applicationInfo =
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO &*
      setStrRef @"pApplicationName" "Hello Triangle" &*
      set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0) &*
      setStrRef @"pEngineName" "No Engine" &*
      set @"engineVersion" (_VK_MAKE_VERSION 1 0 0) &*
      set @"apiVersion" VK_API_VERSION_1_0 &*
      set @"pNext" VK_NULL

getFirstSuitablePhysicalDeviceAndProperties :: MonadIO io => VkInstance -> VkSurfaceKHR -> [String] -> io (VkPhysicalDevice, QueueFamilyIndices, SwapchainSupportDetails)
getFirstSuitablePhysicalDeviceAndProperties vulkanInstance surface deviceExtensions =
  listPhysicalDevices vulkanInstance >>=
  firstJustM (\physicalDevice -> runMaybeT $ do
    qfi <- findQueueFamilyIndices physicalDevice surface
    liftIO $ putStrLn "Found queue family indices."

    guardM $ liftIO $ null <$> getUnsupportedDeviceExtensionNames physicalDevice VK_NULL deviceExtensions
    liftIO $ putStrLn "All expected device extensions are supported."

    scsd <- liftIO $ getSwapchainSupportDetails physicalDevice surface
    liftIO $ putStrLn "Finished obtaining swapchain support details."

    guard $ not . null $ scsdSurfaceFormats scsd
    guard $ not . null $ scsdPresentModes scsd

    return (physicalDevice, qfi, scsd)
  ) >>=
  maybe (throwIOAppEx "Failed to find a suitable physical device.") return

findQueueFamilyIndices :: MonadIO io => VkPhysicalDevice -> VkSurfaceKHR -> MaybeT io QueueFamilyIndices
findQueueFamilyIndices physicalDevice surface = do
  indexedFamiliesHavingQueues <- liftIO $
    filter ((0 <) . getField @"queueCount" . snd) . zip [0 ..] <$>
    listPhysicalDeviceQueueFamilyProperties physicalDevice

  let findFamilyIndexWhere condIO = MaybeT $ liftIO $ fmap fst <$> findM condIO indexedFamiliesHavingQueues

  graphics <- findFamilyIndexWhere $ return . (zeroBits /=) . (VK_QUEUE_GRAPHICS_BIT .&.) . getField @"queueFlags" . snd

  present <- findFamilyIndexWhere $ \(qfi, _) ->
    alloca $ \isSupportedPtr -> do
      vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi surface isSupportedPtr &
        onVkFailureThrow "vkGetPhysicalDeviceSurfaceSupportKHR failed."
      peek isSupportedPtr <&> (== VK_TRUE)

  return $ QueueFamilyIndices graphics present

registerDebugCallback :: VkInstance -> ResIO ()
registerDebugCallback vulkanInstance = do
  debugCallbackPtr <- allocateAcquire_ $ newFunPtrFrom $ newVkDebugReportCallbackEXT debugCallback
  void $ allocateAcquire_ $
    registeredVkDebugReportCallbackEXT vulkanInstance $
    configureVkDebugReportCallbackEXT debugReportFlags debugCallbackPtr

  where
    debugReportFlags :: VkDebugReportFlagsEXT
    debugReportFlags = (VK_DEBUG_REPORT_ERROR_BIT_EXT .|. VK_DEBUG_REPORT_WARNING_BIT_EXT)

    debugCallback :: HS_vkDebugReportCallbackEXT
    debugCallback flags objectType object location messageCode layerPrefixPtr messagePtr userDataPtr = do
      message <- peekCString messagePtr
      putStrLn $ "Validation Layer: " ++ message
      return VK_FALSE

configureVkDebugReportCallbackEXT :: VkDebugReportFlagsEXT -> PFN_vkDebugReportCallbackEXT -> VkDebugReportCallbackCreateInfoEXT
configureVkDebugReportCallbackEXT debugReportFlags debugCallbackPtr =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT &*
  set @"flags" debugReportFlags &*
  set @"pfnCallback" debugCallbackPtr &*
  set @"pNext" VK_NULL

configureVkInstance :: VkApplicationInfo -> [String] -> [CString] -> VkInstanceCreateInfo
configureVkInstance applicationInfo validationLayers extensions =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO &*
  setVkRef @"pApplicationInfo" applicationInfo &*
  set @"enabledExtensionCount" (fromIntegral $ length extensions) &*
  setListRef @"ppEnabledExtensionNames" extensions &*
  set @"enabledLayerCount" (fromIntegral $ length validationLayers) &*
  setStrListRef @"ppEnabledLayerNames" validationLayers &*
  set @"pNext" VK_NULL

configureVkDevice :: [Word32] -> [CString] -> VkDeviceCreateInfo
configureVkDevice queueFamilyIndices deviceExtensions =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" 0 &*
  set @"queueCreateInfoCount" (fromIntegral $ length queueFamilyIndices) &*
  setListRef @"pQueueCreateInfos" (
    queueFamilyIndices <&> \qfi ->
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"flags" 0 &*
      set @"queueFamilyIndex" qfi &*
      set @"queueCount" 1 &*
      setListRef @"pQueuePriorities" [1.0]
  ) &*
  set @"enabledLayerCount" 0 &*
  set @"ppEnabledLayerNames" VK_NULL &*
  set @"enabledExtensionCount" (fromIntegral $ length deviceExtensions) &*
  setListRef @"ppEnabledExtensionNames" deviceExtensions &*
  setVkRef @"pEnabledFeatures" (
    createVk $ handleRemFields @_ @'[]
  )

chooseSwapchainSurfaceFormat :: [VkSurfaceFormatKHR] -> VkSurfaceFormatKHR
chooseSwapchainSurfaceFormat = \case
  [f] | getField @"format" f == VK_FORMAT_UNDEFINED -> idealSurfaceFormat
  fs -> find (== idealSurfaceFormat) fs & fromMaybe (throwAppEx "Failed to find an appropriate swap surface format.")
  where
    idealSurfaceFormat =
      createVk $
      set @"format" VK_FORMAT_B8G8R8A8_UNORM &*
      set @"colorSpace" VK_COLOR_SPACE_SRGB_NONLINEAR_KHR

chooseSwapchainPresentMode :: [VkPresentModeKHR] -> VkPresentModeKHR
chooseSwapchainPresentMode presentModes =
  fromMaybe VK_PRESENT_MODE_FIFO_KHR $
  find (elemOf presentModes) [VK_PRESENT_MODE_MAILBOX_KHR, VK_PRESENT_MODE_IMMEDIATE_KHR]

chooseSwapchainExtent :: Word32 -> Word32 -> VkSurfaceCapabilitiesKHR -> VkExtent2D
chooseSwapchainExtent idealWidth idealHeight capabilities =
  if getField @"width" currentExtent /= maxBound then
    currentExtent
  else
    createVk $
    set @"width" (idealWidth & clamp (getField @"width" minImageExtent) (getField @"width" maxImageExtent)) &*
    set @"height" (idealHeight & clamp (getField @"height" minImageExtent) (getField @"height" maxImageExtent))
  where
    currentExtent = getField @"currentExtent" capabilities
    minImageExtent = getField @"minImageExtent" capabilities
    maxImageExtent = getField @"maxImageExtent" capabilities

chooseSwapchainImageCount :: VkSurfaceCapabilitiesKHR -> Word32
chooseSwapchainImageCount capabilities =
  if maxImageCount > 0 then
    min maxImageCount idealImageCount
  else
    idealImageCount
  where
    idealImageCount = getField @"minImageCount" capabilities + 1
    maxImageCount = getField @"maxImageCount" capabilities

configureVkSwapchain :: VkSurfaceKHR -> VkSurfaceCapabilitiesKHR -> VkSurfaceFormatKHR -> VkPresentModeKHR -> VkExtent2D -> Word32 -> [Word32] -> VkSwapchainCreateInfoKHR
configureVkSwapchain surface capabilities surfaceFormat presentMode extent imageCount queueFamilyIndices =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR &*
  set @"pNext" VK_NULL &*
  set @"flags" 0 &*
  set @"surface" surface &*
  set @"minImageCount" imageCount &*
  set @"imageFormat" (getField @"format" surfaceFormat) &*
  set @"imageColorSpace" (getField @"colorSpace" surfaceFormat) &*
  set @"imageExtent" extent &*
  set @"imageArrayLayers" 1 &*
  set @"imageUsage" VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT &*
  (
    case length queueFamilyIndices of
      count | count >= 2 ->
        set @"imageSharingMode" VK_SHARING_MODE_CONCURRENT &*
        set @"queueFamilyIndexCount" (fromIntegral count) &*
        setListRef @"pQueueFamilyIndices" queueFamilyIndices
      _ ->
        set @"imageSharingMode" VK_SHARING_MODE_EXCLUSIVE &*
        set @"queueFamilyIndexCount" 0 &*
        set @"pQueueFamilyIndices" VK_NULL
  ) &*
  set @"preTransform" (getField @"currentTransform" capabilities) &*
  set @"compositeAlpha" VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR &*
  set @"presentMode" presentMode &*
  set @"clipped" VK_TRUE &*
  set @"oldSwapchain" VK_NULL

configureVkImageView :: VkFormat -> VkImage -> VkImageViewCreateInfo
configureVkImageView format image =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"image" image &*
  set @"viewType" VK_IMAGE_VIEW_TYPE_2D &*
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
    set @"levelCount" 1 &*
    set @"baseArrayLayer" 0 &*
    set @"layerCount" 1
  )

configureShaderModule :: CSize -> Ptr Word8 -> VkShaderModuleCreateInfo
configureShaderModule codeSize codePtr =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" 0 &*
  set @"codeSize" codeSize &*
  set @"pCode" (castPtr codePtr)

configureRenderPass :: VkFormat -> VkRenderPassCreateInfo
configureRenderPass colorAttachmentFormat =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"attachmentCount" 1 &*
  setListRef @"pAttachments" [
    createVk (
      set @"format" colorAttachmentFormat &*
      set @"samples" VK_SAMPLE_COUNT_1_BIT &*
      set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR &*
      set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE &*
      set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE &*
      set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE &*
      set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED &*
      set @"finalLayout" VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
    )
  ] &*
  set @"subpassCount" 1 &*
  setListRef @"pSubpasses" [
    createVk (
      set @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS &*
      set @"colorAttachmentCount" 1 &*
      setListRef @"pColorAttachments" [
        createVk (
          set @"attachment" 0 &*
          set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        )
      ] &*
      set @"pInputAttachments" VK_NULL &*
      set @"pPreserveAttachments" VK_NULL
    )
  ] &*
  set @"dependencyCount" 0 &*
  set @"pDependencies" VK_NULL

configurePipelineShaderStage :: VkShaderStageFlagBits -> VkShaderModule -> String -> VkPipelineShaderStageCreateInfo
configurePipelineShaderStage stage shaderModule entryPointName =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" 0 &*
  set @"stage" stage &*
  set @"module" shaderModule &*
  setStrRef @"pName" entryPointName &*
  set @"pSpecializationInfo" VK_NULL

configurePipelineVertexInputState :: VkPipelineVertexInputStateCreateInfo
configurePipelineVertexInputState =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"vertexBindingDescriptionCount" 0 &*
  set @"pVertexBindingDescriptions" VK_NULL &*
  set @"vertexAttributeDescriptionCount" 0 &*
  set @"pVertexAttributeDescriptions" VK_NULL

configurePipelineInputAssemblyState :: VkPipelineInputAssemblyStateCreateInfo
configurePipelineInputAssemblyState =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST &*
  set @"primitiveRestartEnable" VK_FALSE

configurePipelineViewportState :: VkExtent2D -> VkPipelineViewportStateCreateInfo
configurePipelineViewportState extent =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"viewportCount" 1 &*
  setListRef @"pViewports" [
    createVk (
      set @"x" 0 &*
      set @"y" 0 &*
      set @"width" (fromIntegral . getField @"width" $ extent) &*
      set @"height" (fromIntegral . getField @"height" $ extent) &*
      set @"minDepth" 0 &*
      set @"maxDepth" 0
    )
  ] &*
  set @"scissorCount" 1 &*
  setListRef @"pScissors" [
    createVk (
      setVk @"offset" (
        set @"x" 0 &*
        set @"y" 0
      ) &*
      set @"extent" extent
    )
  ]

configurePipelineRasterizationState :: VkPipelineRasterizationStateCreateInfo
configurePipelineRasterizationState =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"depthClampEnable" VK_FALSE &*
  set @"rasterizerDiscardEnable" VK_FALSE &*
  set @"polygonMode" VK_POLYGON_MODE_FILL &*
  set @"lineWidth" 1 &*
  set @"cullMode" VK_CULL_MODE_BACK_BIT &*
  set @"frontFace" VK_FRONT_FACE_CLOCKWISE &*
  set @"depthBiasEnable" VK_FALSE &*
  set @"depthBiasConstantFactor" 0 &*
  set @"depthBiasClamp" 0 &*
  set @"depthBiasSlopeFactor" 0

configurePipelineMultisampleState :: VkPipelineMultisampleStateCreateInfo
configurePipelineMultisampleState =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"sampleShadingEnable" VK_FALSE &*
  set @"rasterizationSamples" VK_SAMPLE_COUNT_1_BIT &*
  set @"minSampleShading" 1 &*
  set @"pSampleMask" VK_NULL &*
  set @"alphaToCoverageEnable" VK_FALSE &*
  set @"alphaToOneEnable" VK_FALSE

configurePipelineColorBlendState :: VkPipelineColorBlendStateCreateInfo
configurePipelineColorBlendState =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"logicOpEnable" VK_FALSE &*
  set @"logicOp" VK_LOGIC_OP_COPY &*
  set @"attachmentCount" 1 &*
  setListRef @"pAttachments" [
    createVk (
      set @"colorWriteMask" (VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT) &*
      set @"blendEnable" VK_FALSE &*
      set @"srcColorBlendFactor" VK_BLEND_FACTOR_ONE &*
      set @"dstColorBlendFactor" VK_BLEND_FACTOR_ZERO &*
      set @"colorBlendOp" VK_BLEND_OP_ADD &*
      set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE &*
      set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO &*
      set @"alphaBlendOp" VK_BLEND_OP_ADD
    )
  ] &*
  setAt @"blendConstants" @0 0 &*
  setAt @"blendConstants" @1 0 &*
  setAt @"blendConstants" @2 0 &*
  setAt @"blendConstants" @3 0

configurePipelineDynamicState :: VkPipelineDynamicStateCreateInfo
configurePipelineDynamicState =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"dynamicStateCount" 2 &*
  setListRef @"pDynamicStates" [
    VK_DYNAMIC_STATE_VIEWPORT,
    VK_DYNAMIC_STATE_LINE_WIDTH
  ]

configurePipelineLayout :: VkPipelineLayoutCreateInfo
configurePipelineLayout =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"setLayoutCount" 0 &*
  set @"pSetLayouts" VK_NULL &*
  set @"pushConstantRangeCount" 0 &*
  set @"pPushConstantRanges" VK_NULL

configureGraphicsPipeline ::
  [VkPipelineShaderStageCreateInfo] ->
  VkPipelineVertexInputStateCreateInfo ->
  VkPipelineInputAssemblyStateCreateInfo ->
  VkPipelineViewportStateCreateInfo ->
  VkPipelineRasterizationStateCreateInfo ->
  VkPipelineMultisampleStateCreateInfo ->
  VkPipelineColorBlendStateCreateInfo ->
  VkRenderPass ->
  VkPipelineLayout ->
  VkGraphicsPipelineCreateInfo
configureGraphicsPipeline
  shaderStageCreateInfos
  vertexInputStateCreateInfo
  inputAssemblyStateCreateInfo
  viewportStateCreateInfo
  rasterizationStateCreateInfo
  multisampleStateCreateInfo
  colorBlendStateCreateInfo
  renderPass
  pipelineLayout
  =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"stageCount" (fromIntegral $ length $ shaderStageCreateInfos) &*
  setListRef @"pStages" shaderStageCreateInfos &*
  setVkRef @"pVertexInputState" vertexInputStateCreateInfo &*
  setVkRef @"pInputAssemblyState" inputAssemblyStateCreateInfo &*
  setVkRef @"pViewportState" viewportStateCreateInfo &*
  setVkRef @"pRasterizationState" rasterizationStateCreateInfo &*
  setVkRef @"pMultisampleState" multisampleStateCreateInfo &*
  set @"pDepthStencilState" VK_NULL &*
  setVkRef @"pColorBlendState" colorBlendStateCreateInfo &*
  set @"pDynamicState" VK_NULL &*
  set @"renderPass" renderPass &*
  set @"subpass" 0 &*
  set @"layout" pipelineLayout &*
  set @"basePipelineHandle" VK_NULL_HANDLE &*
  set @"basePipelineIndex" (-1)

configureFramebuffer :: VkRenderPass -> [VkImageView] -> VkExtent2D -> VkFramebufferCreateInfo
configureFramebuffer renderPass attachments extent =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"renderPass" renderPass &*
  set @"attachmentCount" (fromIntegral $ length $ attachments) &*
  setListRef @"pAttachments" attachments &*
  set @"width" (getField @"width" extent) &*
  set @"height" (getField @"height" extent) &*
  set @"layers" 1

configureCommandPool :: Word32 -> VkCommandPoolCreateFlags -> VkCommandPoolCreateInfo
configureCommandPool queueFamilyIndex flags =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"queueFamilyIndex" queueFamilyIndex &*
  set @"flags" flags

configureCommandBuffers :: VkCommandPool -> VkCommandBufferLevel -> Word32 -> VkCommandBufferAllocateInfo
configureCommandBuffers commandPool level count =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"commandPool" commandPool &*
  set @"level" level &*
  set @"commandBufferCount" count

data QueueFamilyIndices =
  QueueFamilyIndices {
    qfiGraphics :: Word32,
    qfiPresent :: Word32
  }

qfiAll :: QueueFamilyIndices -> [Word32]
qfiAll qfi = [qfiGraphics, qfiPresent] <&> ($ qfi)

qfiDistinct :: QueueFamilyIndices -> [Word32]
qfiDistinct = distinct . qfiAll

data SwapchainSupportDetails =
  SwapchainSupportDetails {
    scsdCapabilities :: VkSurfaceCapabilitiesKHR,
    scsdSurfaceFormats :: [VkSurfaceFormatKHR],
    scsdPresentModes :: [VkPresentModeKHR]
  }

getSwapchainSupportDetails :: VkPhysicalDevice -> VkSurfaceKHR -> IO SwapchainSupportDetails
getSwapchainSupportDetails physicalDevice surface = do
  capabilities <- liftIO $ getPhysicalDeviceSurfaceCapabilities physicalDevice surface
  putStrLn "Obtained physical device surface capabilities."
  formats <- liftIO $ listPhysicalDeviceSurfaceFormats physicalDevice surface
  putStrLn "Obtained physical device surface formats."
  presentModes <- liftIO $ listPhysicalDeviceSurfacePresentModes physicalDevice surface
  putStrLn "Obtained physical device surface present modes."
  return $ SwapchainSupportDetails capabilities formats presentModes

data VulkanException = VulkanException VkResult String deriving (Eq, Show, Read)

instance Exception VulkanException where
  displayException (VulkanException code message) =
    "Vulkan error (" ++ show code ++ "): " ++ message

onVkFailureThrow :: String -> IO VkResult -> IO ()
onVkFailureThrow message vkAction = do
  result <- vkAction
  when (result /= VK_SUCCESS) $ throwIO $ VulkanException result message

data ApplicationException = ApplicationException String deriving (Eq, Show, Read)

instance Exception ApplicationException where
  displayException (ApplicationException message) =
    "Application error: " ++ message

throwAppEx :: String -> a
throwAppEx message = throw $ ApplicationException message

throwIOAppEx :: MonadIO io => String -> io a
throwIOAppEx message = liftIO . throwIO $ ApplicationException message

initializedGLFW :: Acquire ()
initializedGLFW = unlessM GLFW.init (throwIOAppEx "Failed to initialize GLFW.") `mkAcquire` const GLFW.terminate

newVulkanGLFWWindow :: Int -> Int -> String -> Acquire GLFW.Window
newVulkanGLFWWindow width height title =
  do
    GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
    GLFW.windowHint $ WindowHint'Resizable False
    GLFW.createWindow width height title Nothing Nothing >>=
      maybe (throwIOAppEx "Failed to initialize the GLFW window.") return
  `mkAcquire`
  GLFW.destroyWindow

newGLFWWindowSurface :: VkInstance -> GLFW.Window -> Acquire VkSurfaceKHR
newGLFWWindowSurface vulkanInstance window =
  (
    alloca $ \surfacePtr -> do
      GLFW.createWindowSurface vulkanInstance window nullPtr surfacePtr &
        onVkFailureThrow "GLFW.createWindowSurface failed."
      peek surfacePtr
  )
  `mkAcquire`
  \surface -> vkDestroySurfaceKHR vulkanInstance surface VK_NULL

newVk ::
  (Storable vk, VulkanMarshal ci) =>
  String ->
  (Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult) ->
  (vk -> Ptr VkAllocationCallbacks -> IO ()) ->
  ci ->
  Acquire vk
newVk createFuncName create destroy createInfo =
  (
    withPtr createInfo $ \createInfoPtr ->
      alloca $ \vkPtr -> do
        create createInfoPtr VK_NULL vkPtr &
          onVkFailureThrow (createFuncName ++ " failed.")
        peek vkPtr
  )
  `mkAcquire`
  \vk -> destroy vk VK_NULL

newDeviceVk ::
  (Storable vk, VulkanMarshal ci) =>
  String ->
  (VkDevice -> Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult) ->
  (VkDevice -> vk -> Ptr VkAllocationCallbacks -> IO ()) ->
  VkDevice ->
  ci ->
  Acquire vk
newDeviceVk createFuncName create destroy device createInfo = newVk createFuncName (create device) (destroy device) createInfo

newVkInstance :: VkInstanceCreateInfo -> Acquire VkInstance
newVkInstance = newVk "vkCreateInstance" vkCreateInstance vkDestroyInstance

newVkDevice :: VkPhysicalDevice -> VkDeviceCreateInfo -> Acquire VkDevice
newVkDevice physicalDevice = newVk "vkCreateDevice" (vkCreateDevice physicalDevice) vkDestroyDevice

newVkSwapchain :: VkDevice -> VkSwapchainCreateInfoKHR -> Acquire VkSwapchainKHR
newVkSwapchain = newDeviceVk "vkCreateSwapchainKHR" vkCreateSwapchainKHR vkDestroySwapchainKHR

newVkImageView :: VkDevice -> VkImageViewCreateInfo -> Acquire VkImageView
newVkImageView = newDeviceVk "vkCreateImageView" vkCreateImageView vkDestroyImageView

newVkImageViews :: VkDevice -> [VkImageViewCreateInfo] -> Acquire [VkImageView]
newVkImageViews = mapM . newVkImageView

fillBufferWithShaderFileContents :: FilePath -> ResIO (ReleaseKey, CSize, Ptr Word8)
fillBufferWithShaderFileContents path = do
  (handleKey, handle) <- allocate (openBinaryFile path ReadMode) hClose
  fileSize <- liftIO $ hFileSize handle

  -- Vulkan requires SPIR-V bytecode to have an alignment of 4 bytes.
  let alignedSize = fromIntegral . (4 *) . (`div` 4) . (3 +) $ fileSize

  (bufferPtrKey, bufferPtr) <- allocate (mallocArray @Word8 alignedSize) free
  bytesRead <- liftIO $ hGetBuf handle bufferPtr alignedSize
  release handleKey

  liftIO $ pokeArray (castPtr @_ @Word8 . plusPtr bufferPtr $ bytesRead) $ replicate (alignedSize - bytesRead) 0

  return (bufferPtrKey, fromIntegral alignedSize, bufferPtr)

newShaderModule :: VkDevice -> VkShaderModuleCreateInfo -> Acquire VkShaderModule
newShaderModule = newDeviceVk "vkCreateShaderModule" vkCreateShaderModule vkDestroyShaderModule

createShaderModuleFromFile :: VkDevice -> FilePath -> ResIO (ReleaseKey, VkShaderModule)
createShaderModuleFromFile device path = do
  (bufferKey, bufferSize, bufferPtr) <- fillBufferWithShaderFileContents path
  shaderModuleWithKey <-
    allocateAcquire $
    newShaderModule device $
    configureShaderModule bufferSize bufferPtr
  release bufferKey
  return shaderModuleWithKey

newRenderPass :: VkDevice -> VkRenderPassCreateInfo -> Acquire VkRenderPass
newRenderPass = newDeviceVk "vkCreateRenderPass" vkCreateRenderPass vkDestroyRenderPass

newPipelineLayout :: VkDevice -> VkPipelineLayoutCreateInfo -> Acquire VkPipelineLayout
newPipelineLayout = newDeviceVk "vkCreatePipelineLayout" vkCreatePipelineLayout vkDestroyPipelineLayout

-- Problem: It seems less than ideal to force all the pipelines to be destroyed together.
-- This can be fixed by making a ResIO [(ReleaseKey, VkPipeline)].
newGraphicsPipelines :: VkDevice -> [VkGraphicsPipelineCreateInfo] -> Acquire [VkPipeline]
newGraphicsPipelines device createInfos =
  (
    let pipelineCount = length createInfos
    in
      withArray createInfos $ \createInfosPtr ->
        allocaArray pipelineCount $ \pipelinesPtr -> do
          vkCreateGraphicsPipelines device VK_NULL_HANDLE (fromIntegral pipelineCount) createInfosPtr VK_NULL pipelinesPtr &
            onVkFailureThrow "vkCreateGraphicsPipelines failed."
          peekArray pipelineCount pipelinesPtr
  )
  `mkAcquire`
  \pipelines ->
    forM_ pipelines $ \pipeline ->
      vkDestroyPipeline device pipeline VK_NULL

newGraphicsPipeline :: VkDevice -> VkGraphicsPipelineCreateInfo -> Acquire VkPipeline
newGraphicsPipeline device createInfo = head <$> newGraphicsPipelines device [createInfo]

newFramebuffer :: VkDevice -> VkFramebufferCreateInfo -> Acquire VkFramebuffer
newFramebuffer = newDeviceVk "vkCreateFramebuffer" vkCreateFramebuffer vkDestroyFramebuffer

newCommandPool :: VkDevice -> VkCommandPoolCreateInfo -> Acquire VkCommandPool
newCommandPool = newDeviceVk "vkCreateCommandPool" vkCreateCommandPool vkDestroyCommandPool

allocateCommandBuffers :: MonadIO io => VkDevice -> VkCommandBufferAllocateInfo -> io [VkCommandBuffer]
allocateCommandBuffers device allocateInfo = liftIO $
  withPtr allocateInfo $ \allocateInfoPtr ->
    allocaArray commandBufferCount $ \commandBuffersPtr -> do
      vkAllocateCommandBuffers device allocateInfoPtr commandBuffersPtr &
        onVkFailureThrow "vkAllocateCommandBuffers failed."
      peekArray commandBufferCount commandBuffersPtr
  where
    commandBufferCount = fromIntegral $ getField @"commandBufferCount" allocateInfo

beginCommandBuffer :: MonadIO io => VkCommandBuffer -> VkCommandBufferBeginInfo -> io ()
beginCommandBuffer commandBuffer beginInfo =
  liftIO $ withPtr beginInfo $ \beginInfoPtr ->
    vkBeginCommandBuffer commandBuffer beginInfoPtr &
      onVkFailureThrow "vkBeginCommandBuffer failed."

endCommandBuffer :: MonadIO io => VkCommandBuffer -> io ()
endCommandBuffer commandBuffer =
  liftIO $ vkEndCommandBuffer commandBuffer &
    onVkFailureThrow "vkEndCommandBuffer failed."

cmdBeginRenderPass :: MonadIO io => VkCommandBuffer -> VkRenderPassBeginInfo -> VkSubpassContents -> io ()
cmdBeginRenderPass commandBuffer beginInfo subpassContents =
  liftIO $ withPtr beginInfo $ \beginInfoPtr ->
    vkCmdBeginRenderPass commandBuffer beginInfoPtr subpassContents

getDeviceQueue :: MonadIO io => VkDevice -> Word32 -> Word32 -> io VkQueue
getDeviceQueue device queueFamilyIndex queueIndex =
  liftIO $ alloca $ \deviceQueuePtr -> do
    vkGetDeviceQueue device queueFamilyIndex 0 deviceQueuePtr
    peek deviceQueuePtr

newFunPtrFrom :: IO (FunPtr f) -> Acquire (FunPtr f)
newFunPtrFrom = flip mkAcquire freeHaskellFunPtr

registeredVkDebugReportCallbackEXT :: VkInstance -> VkDebugReportCallbackCreateInfoEXT -> Acquire VkDebugReportCallbackEXT
registeredVkDebugReportCallbackEXT vulkanInstance createInfo =
    do
      createDebugReportCallbackEXT <- vkGetInstanceProc @VkCreateDebugReportCallbackEXT vulkanInstance
      withPtr createInfo $ \createInfoPtr ->
        alloca $ \vkDebugReportCallbackEXTPtr -> do
          createDebugReportCallbackEXT vulkanInstance createInfoPtr VK_NULL vkDebugReportCallbackEXTPtr &
            onVkFailureThrow "vkCreateDebugReportCallbackEXT failed."
          peek vkDebugReportCallbackEXTPtr
    `mkAcquire`
    \vkDebugReportCallbackEXT -> do
      destroyDebugReportCallbackEXT <- vkGetInstanceProc @VkDestroyDebugReportCallbackEXT vulkanInstance
      destroyDebugReportCallbackEXT vulkanInstance vkDebugReportCallbackEXT VK_NULL

ensureValidationLayersSupported :: MonadIO io => [String] -> io ()
ensureValidationLayersSupported validationLayers = do
  unsupportedLayers <- getUnsupportedValidationLayerNames validationLayers
  unless (null unsupportedLayers) $ throwIOAppEx ("Expected validation layers are not available: " ++ show unsupportedLayers)

getUnsupportedValidationLayerNames :: MonadIO io => [String] -> io [String]
getUnsupportedValidationLayerNames [] = return []
getUnsupportedValidationLayerNames expectedLayerNames =
  listInstanceLayerProperties
  <&> fmap (getStringField @"layerName")
  <&> \case
    [] -> expectedLayerNames
    availableLayerNames -> filter (not . elemOf availableLayerNames) expectedLayerNames

getUnsupportedDeviceExtensionNames :: MonadIO io => VkPhysicalDevice -> CString -> [String] -> io [String]
getUnsupportedDeviceExtensionNames _ _ [] = return []
getUnsupportedDeviceExtensionNames physicalDevice layerName expectedExtensionNames = do
  listPhysicalDeviceExtensionProperties physicalDevice layerName
  <&> fmap (getStringField @"extensionName")
  <&> \case
    [] -> expectedExtensionNames
    availableExtensionNames -> filter (not . elemOf availableExtensionNames) expectedExtensionNames

getVkList :: (MonadIO io, Storable a) => (Ptr Word32 -> Ptr a -> IO ()) -> io [a]
getVkList getArray =
  liftIO $ alloca $ \countPtr -> do
    getArray countPtr VK_NULL
    count <- fromIntegral <$> peek countPtr
    if count > 0 then
      allocaArray count $ \arrayPtr -> do
        getArray countPtr arrayPtr
        peekArray count arrayPtr
    else
      return []

listInstanceLayerProperties :: MonadIO io => io [VkLayerProperties]
listInstanceLayerProperties =
  getVkList $ \layerCountPtr layersPtr ->
    vkEnumerateInstanceLayerProperties layerCountPtr layersPtr &
      onVkFailureThrow "vkEnumerateInstanceLayerProperties failed."

listPhysicalDevices :: MonadIO io => VkInstance -> io [VkPhysicalDevice]
listPhysicalDevices vulkanInstance =
  getVkList $ \deviceCountPtr devicesPtr ->
    vkEnumeratePhysicalDevices vulkanInstance deviceCountPtr devicesPtr &
      onVkFailureThrow "vkEnumeratePhysicalDevices failed."

listPhysicalDeviceQueueFamilyProperties :: MonadIO io => VkPhysicalDevice -> io [VkQueueFamilyProperties]
listPhysicalDeviceQueueFamilyProperties physicalDevice =
  getVkList $ \queueCountPtr queuesPtr ->
    vkGetPhysicalDeviceQueueFamilyProperties physicalDevice queueCountPtr queuesPtr

listPhysicalDeviceExtensionProperties :: MonadIO io => VkPhysicalDevice -> CString -> io [VkExtensionProperties]
listPhysicalDeviceExtensionProperties physicalDevice layerName =
  getVkList $ \extCountPtr extsPtr ->
    vkEnumerateDeviceExtensionProperties physicalDevice layerName extCountPtr extsPtr &
      onVkFailureThrow "vkEnumerateDeviceExtensionProperties failed."

getPhysicalDeviceSurfaceCapabilities :: MonadIO io => VkPhysicalDevice -> VkSurfaceKHR -> io VkSurfaceCapabilitiesKHR
getPhysicalDeviceSurfaceCapabilities physicalDevice surface =
  liftIO $ alloca $ \capsPtr -> do
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface capsPtr
    peek capsPtr

listPhysicalDeviceSurfaceFormats :: MonadIO io => VkPhysicalDevice -> VkSurfaceKHR -> io [VkSurfaceFormatKHR]
listPhysicalDeviceSurfaceFormats physicalDevice surface =
  getVkList $ \formatCountPtr formatsPtr ->
    vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice surface formatCountPtr formatsPtr &
      onVkFailureThrow "vkGetPhysicalDeviceSurfaceFormatsKHR failed."

listPhysicalDeviceSurfacePresentModes :: MonadIO io => VkPhysicalDevice -> VkSurfaceKHR -> io [VkPresentModeKHR]
listPhysicalDeviceSurfacePresentModes physicalDevice surface =
  getVkList $ \modeCountPtr modesPtr ->
    vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice surface modeCountPtr modesPtr &
      onVkFailureThrow "vkGetPhysicalDeviceSurfacePresentModesKHR failed."

listSwapchainImages :: MonadIO io => VkDevice -> VkSwapchainKHR -> io [VkImage]
listSwapchainImages device swapchain = do
  getVkList $ \imageCountPtr imagesPtr ->
    vkGetSwapchainImagesKHR device swapchain imageCountPtr imagesPtr &
      onVkFailureThrow "vkGetSwapchainImagesKHR failed."

mainLoop :: MonadIO io => GLFW.Window -> io ()
mainLoop window = whileM_ (not <$> liftIO (GLFW.windowShouldClose window)) (liftIO GLFW.pollEvents)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 1 <&>

elemOf :: (Foldable t, Eq a) => t a -> a -> Bool
elemOf = flip elem

distinct :: Ord a => [a] -> [a]
distinct = Set.toList . Set.fromList

guardM :: (Alternative m, Monad m) => m Bool -> m ()
guardM = (guard =<<)

clamp :: Ord a => a -> a -> a -> a
clamp a b =
  case compare a b of
    LT -> min b . max a
    GT -> min a . max b
    EQ -> const a

with_ :: MonadUnliftIO m => Acquire a -> m b -> m b
with_ a m = with a $ \_ -> m

allocate_ :: MonadResource m => IO a -> (a -> IO ()) -> m a
allocate_ create free = snd <$> allocate create free

allocateAcquire_ :: MonadResource m => Acquire a -> m a
allocateAcquire_ a = snd <$> allocateAcquire a

ioPutStrLn = liftIO . putStrLn
