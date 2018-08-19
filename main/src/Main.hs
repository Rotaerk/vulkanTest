{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (init)
import qualified Codec.Picture as JP
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Extra (firstJustM, findM, unlessM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State.Lazy
import Data.Acquire
import Data.Bits
import Data.Bool
import Data.Foldable
import Data.Function
import Data.Functor
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils hiding (with)
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import Graphics.UI.GLFW (WindowHint(..), ClientAPI(..))
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_EXT_debug_report
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame
import Graphics.Vulkan.Marshal.Proc
import Numeric.DataFrame
import Numeric.Dim
import Numeric.PrimBytes
import System.Clock
import System.Console.CmdArgs.Implicit
import System.FilePath
import System.IO

data CommandLineArguments =
  CommandLineArguments {
    claShadersPath :: String,
    claTexturesPath :: String
  } deriving (Show, Data, Typeable)

(initialWindowWidth, initialWindowHeight) = (800, 600)

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

xVertices :: DataFrame Vertex '[XN 3]
xVertices =
  fromJust $
  fromList (D @3) $ scalar <$> [
    Vertex (vec2 (-0.5) (-0.5)) (vec3 1 0 0),
    Vertex (vec2 0.5 (-0.5)) (vec3 0 1 0),
    Vertex (vec2 0.5 0.5) (vec3 0 0 1),
    Vertex (vec2 (-0.5) 0.5) (vec3 1 1 1)
  ]

xIndices :: DataFrame Word16 '[XN 3]
xIndices =
  fromJust $
  fromList (D @3) [
    0, 1, 2, 2, 3, 0
  ]

main :: IO ()
main =
  do
    arguments <-
      cmdArgs $
      CommandLineArguments {
        claShadersPath = def &= explicit &= name "shaderspath" &= typ "PATH" &= help "Path to the SPIR-V shader files.",
        claTexturesPath = def &= explicit &= name "texturespath" &= typ "PATH" &= help "Path to the texture files."
      }
      &= summary "Vulkan Test"

    let
      shadersPath = claShadersPath arguments
      texturesPath = claTexturesPath arguments
    putStrLn $ "Shaders path is: '" ++ shadersPath ++ "'."
    putStrLn $ "Textures path is: '" ++ texturesPath ++ "'."

    case (xVertices, xIndices) of
      (XFrame vertices, XFrame indices) ->
        runResourceT $ do
          -- Before initializing, probably should setErrorCallback
          initializeGLFW
          ioPutStrLn "GLFW initialized."

          window <- createWindow
          ioPutStrLn "Window created."

          lastResizeTimeRef <- liftIO $ newIORef Nothing

          liftIO $ GLFW.setFramebufferSizeCallback window $ Just $ \_ _ _ -> do
            time <- getTime Monotonic
            writeIORef lastResizeTimeRef $ Just time
          ioPutStrLn "Window framebuffer size callback registered."

          glfwExtensions <- liftIO $ GLFW.getRequiredInstanceExtensions

          unless (null validationLayers) $ do
            ensureValidationLayersSupported validationLayers
            ioPutStrLn "All required validation layers supported."

          vulkanInstance <- createVulkanInstance (extensions ++ glfwExtensions) validationLayers
          ioPutStrLn "Vulkan instance created."

#ifndef NDEBUG
          registerDebugCallback vulkanInstance
          ioPutStrLn "Debug callback registered."
#endif

          surface <- createSurface vulkanInstance window
          ioPutStrLn "Window surface obtained."

          (physicalDevice, graphicsQfi, presentQfi) <-
            mapM (liftIO . peekCString) deviceExtensions >>=
            getFirstSuitablePhysicalDeviceAndProperties vulkanInstance surface
          ioPutStrLn "Suitable physical device found."

          let distinctQfis = distinct [graphicsQfi, presentQfi]

          device <- createDevice physicalDevice distinctQfis
          ioPutStrLn "Vulkan device created."

          graphicsQueue <- getDeviceQueue device graphicsQfi 0
          ioPutStrLn "Graphics queue obtained."

          presentQueue <- getDeviceQueue device presentQfi 0
          ioPutStrLn "Present queue obtained."

          descriptorSetLayout <- createDescriptorSetLayout device
          ioPutStrLn "Descriptor set layout created."

          pipelineLayout <- createPipelineLayout device descriptorSetLayout
          ioPutStrLn "Pipeline layout created."

          commandPool <- createCommandPool device graphicsQfi
          ioPutStrLn "Command pool created."

          physicalDeviceMemProperties <- getPhysicalDeviceMemoryProperties physicalDevice
          ioPutStrLn "Physical device memory properties obtained."

          (textureImage, textureImageMemory) <- createTextureImage device commandPool graphicsQueue physicalDeviceMemProperties texturesPath
          ioPutStrLn "Texture image created."

          (vertexBuffer, vertexBufferMemory) <- prepareBuffer device commandPool graphicsQueue physicalDeviceMemProperties vertices VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
          ioPutStrLn "Vertex buffer prepared."

          (indexBuffer, indexBufferMemory) <- prepareBuffer device commandPool graphicsQueue physicalDeviceMemProperties indices VK_BUFFER_USAGE_INDEX_BUFFER_BIT
          ioPutStrLn "Index buffer prepared."

          imageAvailableSemaphores <- replicateM maxFramesInFlight $ createSemaphore device
          ioPutStrLn "Image-available semaphores created."

          renderFinishedSemaphores <- replicateM maxFramesInFlight $ createSemaphore device
          ioPutStrLn "Render-finished semaphores created."

          inFlightFences <- replicateM maxFramesInFlight $ createFence device
          ioPutStrLn "In-flight fences created."

          renderStartTimeRef <- liftIO $ newIORef Nothing

          doWhileM $ runResourceT $ do
            (windowFramebufferWidth, windowFramebufferHeight) <- liftIO $ GLFW.getFramebufferSize window

            surfaceCapabilities <- liftIO $ getPhysicalDeviceSurfaceCapabilities physicalDevice surface
            ioPutStrLn "Obtained physical device surface capabilities."

            surfaceFormats <- liftIO $ listPhysicalDeviceSurfaceFormats physicalDevice surface
            ioPutStrLn "Obtained physical device surface formats."

            surfacePresentModes <- liftIO $ listPhysicalDeviceSurfacePresentModes physicalDevice surface
            ioPutStrLn "Obtained physical device surface present modes."

            let
              swapchainSurfaceFormat = chooseSwapchainSurfaceFormat surfaceFormats
              swapchainImageFormat = getField @"format" swapchainSurfaceFormat
              swapchainSurfacePresentMode = chooseSwapchainSurfacePresentMode surfacePresentModes
              swapchainExtent = chooseSwapchainSurfaceExtent surfaceCapabilities (fromIntegral windowFramebufferWidth) (fromIntegral windowFramebufferHeight)
              idealSwapchainImageCount = chooseSwapchainImageCount surfaceCapabilities
              aspectRatio = fromIntegral (getField @"width" swapchainExtent) / fromIntegral (getField @"height" swapchainExtent)

            swapchain <- createSwapchain device surface surfaceCapabilities swapchainSurfaceFormat swapchainSurfacePresentMode swapchainExtent idealSwapchainImageCount distinctQfis
            ioPutStrLn "Swapchain created."

            swapchainImages <- listSwapchainImages device swapchain
            ioPutStrLn "Swapchain images obtained."

            swapchainImageViews <- createSwapchainImageViews device swapchainImageFormat swapchainImages
            ioPutStrLn "Swapchain image views created."

            let swapchainImageCount = length swapchainImages

            uniformBuffersWithMemories <- replicateM swapchainImageCount $ createUniformBuffer device physicalDeviceMemProperties
            ioPutStrLn "Uniform buffers created and allocated."

            let
              uniformBuffers = fst <$> uniformBuffersWithMemories
              uniformBufferMemories = snd <$> uniformBuffersWithMemories

            descriptorPool <- createDescriptorPool device (fromIntegral swapchainImageCount)
            ioPutStrLn "Descriptor pool created."

            descriptorSets <- allocateDescriptorSets device descriptorPool False (replicate swapchainImageCount descriptorSetLayout)
            ioPutStrLn "Descriptor sets allocated."

            writeDescriptorSets device $ zip uniformBuffers descriptorSets
            ioPutStrLn "Descriptor sets written to."

            renderPass <- createRenderPass device swapchainImageFormat
            ioPutStrLn "Render pass created."

            graphicsPipeline <- createGraphicsPipeline device shadersPath pipelineLayout renderPass swapchainExtent
            ioPutStrLn "Graphics pipeline created."

            swapchainFramebuffers <- createSwapchainFramebuffers device renderPass swapchainExtent swapchainImageViews
            ioPutStrLn "Framebuffers created."

            commandBuffers <- allocateCommandBuffers device commandPool (lengthNum swapchainFramebuffers)
            ioPutStrLn "Command buffers allocated."

            fillCommandBuffers renderPass graphicsPipeline pipelineLayout swapchainExtent vertexBuffer indexBuffer (fromIntegral $ dimVal $ dim1 indices) (zip3 commandBuffers swapchainFramebuffers descriptorSets)
            ioPutStrLn "Command buffers filled."

            renderStartTime <-
              liftIO $ readIORef renderStartTimeRef >>= \case
                Just t -> return t
                Nothing -> do
                  time <- getTime Monotonic
                  writeIORef renderStartTimeRef $ Just time
                  return time

            let drawFrame' = drawFrame device swapchain graphicsQueue presentQueue commandBuffers uniformBufferMemories aspectRatio renderStartTime

            ioPutStrLn "Window event loop starting."
            shouldRebuildSwapchain <- evalStateTWith 0 $ windowEventLoop window lastResizeTimeRef $ do
              currentFrame <- get
              put $ mod (currentFrame + 1) maxFramesInFlight
              drawFrame' (inFlightFences !! currentFrame) (imageAvailableSemaphores !! currentFrame) (renderFinishedSemaphores !! currentFrame)

            ioPutStrLn "Window event loop ended.  Waiting for device to idle."
            liftIO $ vkDeviceWaitIdle device

            ioPutStrLn "Cleaning up swapchain-related objects."
            return shouldRebuildSwapchain

          ioPutStrLn "Cleaning up the rest."
  `catch` (
    \(e :: VulkanException) ->
      putStrLn $ displayException e
  )
  `catch` (
    \(e :: ApplicationException) ->
      putStrLn $ displayException e
  )
  where
    initializeGLFW :: MonadIO io => ResourceT io ()
    initializeGLFW = allocateAcquire_ initializedGLFW

    createWindow :: MonadIO io => ResourceT io GLFW.Window
    createWindow = allocateAcquire_ $ newVulkanGLFWWindow initialWindowWidth initialWindowHeight "Vulkan"

    createVulkanInstance :: MonadIO io => [CString] -> [String] -> ResourceT io VkInstance
    createVulkanInstance extensions validationLayers =
      allocateAcquire_ $
      newVkInstance $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      setVkRef @"pApplicationInfo" (
        createVk $
        set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO &*
        setStrRef @"pApplicationName" "Hello Triangle" &*
        set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0) &*
        setStrRef @"pEngineName" "No Engine" &*
        set @"engineVersion" (_VK_MAKE_VERSION 1 0 0) &*
        set @"apiVersion" VK_API_VERSION_1_0 &*
        set @"pNext" VK_NULL
      ) &*
      set @"enabledExtensionCount" (lengthNum extensions) &*
      setListRef @"ppEnabledExtensionNames" extensions &*
      set @"enabledLayerCount" (lengthNum validationLayers) &*
      setStrListRef @"ppEnabledLayerNames" validationLayers

    createSurface :: MonadIO io => VkInstance -> GLFW.Window -> ResourceT io VkSurfaceKHR
    createSurface vulkanInstance window = allocateAcquire_ $ newGLFWWindowSurface vulkanInstance window

    getFirstSuitablePhysicalDeviceAndProperties :: MonadIO io => VkInstance -> VkSurfaceKHR -> [String] -> io (VkPhysicalDevice, Word32, Word32)
    getFirstSuitablePhysicalDeviceAndProperties vulkanInstance surface deviceExtensions =
      listPhysicalDevices vulkanInstance >>=
      firstJustM (\physicalDevice -> runMaybeT $ do
        (graphicsQfi, presentQfi) <- findQueueFamilyIndices physicalDevice surface
        liftIO $ putStrLn "Found queue family indices."

        guardM $ liftIO $ null <$> getUnsupportedDeviceExtensionNames physicalDevice VK_NULL deviceExtensions
        liftIO $ putStrLn "All expected device extensions are supported."

        surfaceFormats <- liftIO $ listPhysicalDeviceSurfaceFormats physicalDevice surface
        surfacePresentModes <- liftIO $ listPhysicalDeviceSurfacePresentModes physicalDevice surface

        guard $ not . null $ surfaceFormats
        guard $ not . null $ surfacePresentModes

        return (physicalDevice, graphicsQfi, presentQfi)
      ) >>=
      maybe (throwIOAppEx "Failed to find a suitable physical device.") return

    findQueueFamilyIndices :: MonadIO io => VkPhysicalDevice -> VkSurfaceKHR -> MaybeT io (Word32, Word32)
    findQueueFamilyIndices physicalDevice surface = do
      indexedFamiliesHavingQueues <- liftIO $
        filter ((0 <) . getField @"queueCount" . snd) . zip [0 ..] <$>
        listPhysicalDeviceQueueFamilyProperties physicalDevice

      let findFamilyIndexWhere condIO = MaybeT $ liftIO $ fmap fst <$> findM condIO indexedFamiliesHavingQueues

      graphicsQfi <- findFamilyIndexWhere $ return . (zeroBits /=) . (VK_QUEUE_GRAPHICS_BIT .&.) . getField @"queueFlags" . snd

      presentQfi <- findFamilyIndexWhere $ \(qfi, _) ->
        alloca $ \isSupportedPtr -> do
          vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi surface isSupportedPtr &
            onVkFailureThrow "vkGetPhysicalDeviceSurfaceSupportKHR failed."
          peek isSupportedPtr <&> (== VK_TRUE)

      return (graphicsQfi, presentQfi)

    createDevice :: MonadIO io => VkPhysicalDevice -> [Word32] -> ResourceT io VkDevice
    createDevice physicalDevice qfis =
      allocateAcquire_ $
      newVkDevice physicalDevice $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"flags" 0 &*
      set @"queueCreateInfoCount" (lengthNum qfis) &*
      setListRef @"pQueueCreateInfos" (
        qfis <&> \qfi ->
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
      set @"enabledExtensionCount" (lengthNum deviceExtensions) &*
      setListRef @"ppEnabledExtensionNames" deviceExtensions &*
      setVkRef @"pEnabledFeatures" (
        createVk $ handleRemFields @_ @'[]
      )

    createPipelineLayout :: MonadIO io => VkDevice -> VkDescriptorSetLayout -> ResourceT io VkPipelineLayout
    createPipelineLayout device descriptorSetLayout =
      allocateAcquire_ $
      newPipelineLayout device $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"setLayoutCount" 1 &*
      setListRef @"pSetLayouts" [descriptorSetLayout] &*
      set @"pushConstantRangeCount" 0 &*
      set @"pPushConstantRanges" VK_NULL

    createCommandPool :: MonadIO io => VkDevice -> Word32 -> ResourceT io VkCommandPool
    createCommandPool device qfi =
      allocateAcquire_ $
      newCommandPool device $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"queueFamilyIndex" qfi &*
      set @"flags" 0

    createTextureImage :: MonadUnliftIO io => VkDevice -> VkCommandPool -> VkQueue -> VkPhysicalDeviceMemoryProperties -> FilePath -> ResourceT io (VkImage, VkDeviceMemory)
    createTextureImage device commandPool submissionQueue physDevMemProps texturesPath = runResourceT $ do
      (stagingBuffer, stagingBufferMemory, texWidth, texHeight) <- do
        jpImage <-
          liftIO $ JP.readImage (texturesPath </> textureFileName) >>= \case
            Right (JP.ImageRGBA8 jpImage) -> return jpImage
            Right di | jpImage <- JP.convertRGBA8 di -> return jpImage
            Left errorMsg -> throwIOAppEx ("Failed to read '" ++ textureFileName ++ "': " ++ errorMsg)

        let
          texWidth = JP.imageWidth jpImage
          texHeight = JP.imageHeight jpImage
          imageDataSize = 4 * texWidth * texHeight

        (stagingBuffer, stagingBufferMemory) <-
          createAllocatedBuffer
            device
            (fromIntegral imageDataSize)
            VK_BUFFER_USAGE_TRANSFER_SRC_BIT
            VK_SHARING_MODE_EXCLUSIVE
            (findMemoryType' $ VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)

        liftIO $
          with (mappedMemory device stagingBufferMemory 0 (fromIntegral imageDataSize)) $ \stagingBufferPtr ->
          V.unsafeWith (JP.imageData jpImage) $ \imageDataPtr ->
            copyBytes stagingBufferPtr (castPtr imageDataPtr) imageDataSize

        return (stagingBuffer, stagingBufferMemory, texWidth, texHeight)

      (textureImage, textureImageMemory) <-
        lift $
        createAllocatedImage
          device
          (fromIntegral texWidth)
          (fromIntegral texHeight)
          VK_FORMAT_R8G8B8A8_UNORM
          VK_IMAGE_TILING_OPTIMAL
          (VK_IMAGE_USAGE_TRANSFER_DST_BIT .|. VK_IMAGE_USAGE_SAMPLED_BIT)
          VK_SHARING_MODE_EXCLUSIVE
          (findMemoryType' $ VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)

      let transitionImageLayout' = transitionImageLayout device commandPool submissionQueue textureImage VK_FORMAT_R8G8B8A8_UNORM

      transitionImageLayout' VK_IMAGE_LAYOUT_UNDEFINED VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      transitionImageLayout' VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

      return (textureImage, textureImageMemory)

      where
        textureFileName = "statue.jpg"
        findMemoryType' = findMemoryType physDevMemProps

    createSemaphore :: MonadIO io => VkDevice -> ResourceT io VkSemaphore
    createSemaphore device =
      allocateAcquire_ $
      newSemaphore device $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO &*
      set @"pNext" VK_NULL

    createFence :: MonadIO io => VkDevice -> ResourceT io VkFence
    createFence device =
      allocateAcquire_ $
      newFence device $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"flags" VK_FENCE_CREATE_SIGNALED_BIT

    chooseSwapchainSurfaceFormat :: [VkSurfaceFormatKHR] -> VkSurfaceFormatKHR
    chooseSwapchainSurfaceFormat surfaceFormats =
      case surfaceFormats of
        [f] | getField @"format" f == VK_FORMAT_UNDEFINED -> idealSurfaceFormat
        fs -> find (== idealSurfaceFormat) fs & fromMaybe (throwAppEx "Failed to find an appropriate swap surface format.")
        where
          idealSurfaceFormat =
            createVk $
            set @"format" VK_FORMAT_B8G8R8A8_UNORM &*
            set @"colorSpace" VK_COLOR_SPACE_SRGB_NONLINEAR_KHR

    chooseSwapchainSurfacePresentMode :: [VkPresentModeKHR] -> VkPresentModeKHR
    chooseSwapchainSurfacePresentMode surfacePresentModes =
      fromMaybe VK_PRESENT_MODE_FIFO_KHR $
      find (elemOf surfacePresentModes) [VK_PRESENT_MODE_MAILBOX_KHR, VK_PRESENT_MODE_IMMEDIATE_KHR]

    chooseSwapchainSurfaceExtent :: VkSurfaceCapabilitiesKHR -> Word32 -> Word32 -> VkExtent2D
    chooseSwapchainSurfaceExtent surfaceCapabilities idealWidth idealHeight =
      if getField @"width" currentExtent /= maxBound then
        currentExtent
      else
        createVk $
        set @"width" (idealWidth & clamp (getField @"width" minImageExtent) (getField @"width" maxImageExtent)) &*
        set @"height" (idealHeight & clamp (getField @"height" minImageExtent) (getField @"height" maxImageExtent))
      where
        currentExtent = getField @"currentExtent" surfaceCapabilities
        minImageExtent = getField @"minImageExtent" surfaceCapabilities
        maxImageExtent = getField @"maxImageExtent" surfaceCapabilities

    chooseSwapchainImageCount :: VkSurfaceCapabilitiesKHR -> Word32
    chooseSwapchainImageCount surfaceCapabilities =
      if maxImageCount > 0 then
        min maxImageCount idealImageCount
      else
        idealImageCount
      where
        idealImageCount = getField @"minImageCount" surfaceCapabilities + 1
        maxImageCount = getField @"maxImageCount" surfaceCapabilities

    createSwapchain :: MonadIO io => VkDevice -> VkSurfaceKHR -> VkSurfaceCapabilitiesKHR -> VkSurfaceFormatKHR -> VkPresentModeKHR -> VkExtent2D -> Word32 -> [Word32] -> ResourceT io VkSwapchainKHR
    createSwapchain device surface surfaceCapabilities surfaceFormat surfacePresentMode extent imageCount qfis =
      allocateAcquire_ $
      newVkSwapchain device $
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
        case length qfis of
          count | count >= 2 ->
            set @"imageSharingMode" VK_SHARING_MODE_CONCURRENT &*
            set @"queueFamilyIndexCount" (fromIntegral count) &*
            setListRef @"pQueueFamilyIndices" qfis
          _ ->
            set @"imageSharingMode" VK_SHARING_MODE_EXCLUSIVE &*
            set @"queueFamilyIndexCount" 0 &*
            set @"pQueueFamilyIndices" VK_NULL
      ) &*
      set @"preTransform" (getField @"currentTransform" surfaceCapabilities) &*
      set @"compositeAlpha" VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR &*
      set @"presentMode" surfacePresentMode &*
      set @"clipped" VK_TRUE &*
      set @"oldSwapchain" VK_NULL

    createSwapchainImageViews :: MonadIO io => VkDevice -> VkFormat -> [VkImage] -> ResourceT io [VkImageView]
    createSwapchainImageViews device imageFormat images =
      allocateAcquire_ $
      forM images $ \image ->
      newVkImageView device $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"image" image &*
      set @"viewType" VK_IMAGE_VIEW_TYPE_2D &*
      set @"format" imageFormat &*
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

    createRenderPass :: MonadIO io => VkDevice -> VkFormat -> ResourceT io VkRenderPass
    createRenderPass device imageFormat =
      allocateAcquire_ $
      newRenderPass device $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"attachmentCount" 1 &*
      setListRef @"pAttachments" [
        createVk (
          set @"format" imageFormat &*
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
      set @"dependencyCount" 1 &*
      setListRef @"pDependencies" [
        createVk (
          set @"srcSubpass" VK_SUBPASS_EXTERNAL &*
          set @"dstSubpass" 0 &*
          set @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT &*
          set @"srcAccessMask" 0 &*
          set @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT &*
          set @"dstAccessMask" (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
        )
      ]

    createDescriptorSetLayout :: MonadIO io => VkDevice -> ResourceT io VkDescriptorSetLayout
    createDescriptorSetLayout device =
      allocateAcquire_ $
      newDescriptorSetLayout device $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"bindingCount" 1 &*
      setListRef @"pBindings" [
        createVk (
          set @"binding" 0 &*
          set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER &*
          set @"descriptorCount" 1 &*
          set @"stageFlags" VK_SHADER_STAGE_VERTEX_BIT &*
          set @"pImmutableSamplers" VK_NULL
        )
      ]

    createDescriptorPool :: MonadIO io => VkDevice -> Word32 -> ResourceT io VkDescriptorPool
    createDescriptorPool device descriptorCount =
      allocateAcquire_ $
      newDescriptorPool device $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"poolSizeCount" 1 &*
      setListRef @"pPoolSizes" [
        createVk (
          set @"type" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER &*
          set @"descriptorCount" descriptorCount
        )
      ] &*
      set @"maxSets" descriptorCount

    allocateDescriptorSets :: MonadIO io => VkDevice -> VkDescriptorPool -> Bool -> [VkDescriptorSetLayout] -> ResourceT io [VkDescriptorSet]
    allocateDescriptorSets device descriptorPool canFree layouts =
      allocateAcquire_ $
      allocatedDescriptorSets device canFree $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"descriptorPool" descriptorPool &*
      set @"descriptorSetCount" (lengthNum layouts) &*
      setListRef @"pSetLayouts" layouts

    writeDescriptorSets :: MonadIO io => VkDevice -> [(VkBuffer, VkDescriptorSet)] -> io ()
    writeDescriptorSets device ubdss = updateDescriptorSets device writes []
      where
        writes =
          ffor ubdss $ \(uniformBuffer, descriptorSet) ->
            createVk @VkWriteDescriptorSet $
            set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET &*
            set @"pNext" VK_NULL &*
            set @"dstSet" descriptorSet &*
            set @"dstBinding" 0 &*
            set @"dstArrayElement" 0 &*
            set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER &*
            set @"descriptorCount" 1 &*
            setListRef @"pBufferInfo" [
              createVk $
              set @"buffer" uniformBuffer &*
              set @"offset" 0 &*
              set @"range" (fromIntegral $ bSizeOf @UniformBufferObject undefined)
            ] &*
            set @"pImageInfo" VK_NULL &*
            set @"pTexelBufferView" VK_NULL

    createGraphicsPipeline :: MonadUnliftIO io => VkDevice -> FilePath -> VkPipelineLayout -> VkRenderPass -> VkExtent2D -> ResourceT io VkPipeline
    createGraphicsPipeline device shadersPath pipelineLayout renderPass swapchainExtent = runResourceT $ do
      vertShaderModule <- createShaderModuleFromFile device (shadersPath </> "shader.vert.spv")
      ioPutStrLn "Vertex shader module created."
      fragShaderModule <- createShaderModuleFromFile device (shadersPath </> "shader.frag.spv")
      ioPutStrLn "Fragment shader module created."

      let
        shaderStageCreateInfos =
          [
            configurePipelineShaderStage VK_SHADER_STAGE_VERTEX_BIT vertShaderModule "main",
            configurePipelineShaderStage VK_SHADER_STAGE_FRAGMENT_BIT fragShaderModule "main"
          ]
          where
            configurePipelineShaderStage stage shaderModule entryPointName =
              createVk @VkPipelineShaderStageCreateInfo $
              set @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO &*
              set @"pNext" VK_NULL &*
              set @"flags" 0 &*
              set @"stage" stage &*
              set @"module" shaderModule &*
              setStrRef @"pName" entryPointName &*
              set @"pSpecializationInfo" VK_NULL

      lift $
        allocateAcquire_ $
        newGraphicsPipeline device $
        createVk $
        set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO &*
        set @"pNext" VK_NULL &*
        set @"stageCount" (lengthNum shaderStageCreateInfos) &*
        setListRef @"pStages" shaderStageCreateInfos &*
        setVkRef @"pVertexInputState" (
          let
            vertexAttributeDescriptions = vertexAttributeDescriptionsAt 0
          in
            createVk $
            set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO &*
            set @"pNext" VK_NULL &*
            set @"vertexBindingDescriptionCount" 1 &*
            setVkRef @"pVertexBindingDescriptions" (vertexBindingDescriptionAt 0 VK_VERTEX_INPUT_RATE_VERTEX) &*
            set @"vertexAttributeDescriptionCount" (lengthNum vertexAttributeDescriptions) &*
            setListRef @"pVertexAttributeDescriptions" vertexAttributeDescriptions
        ) &*
        setVkRef @"pInputAssemblyState" (
          createVk $
          set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO &*
          set @"pNext" VK_NULL &*
          set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST &*
          set @"primitiveRestartEnable" VK_FALSE
        ) &*
        setVkRef @"pViewportState" (
          createVk $
          set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO &*
          set @"pNext" VK_NULL &*
          set @"viewportCount" 1 &*
          setListRef @"pViewports" [
            createVk (
              set @"x" 0 &*
              set @"y" 0 &*
              set @"width" (fromIntegral . getField @"width" $ swapchainExtent) &*
              set @"height" (fromIntegral . getField @"height" $ swapchainExtent) &*
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
              set @"extent" swapchainExtent
            )
          ]
        ) &*
        setVkRef @"pRasterizationState" (
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
        ) &*
        setVkRef @"pMultisampleState" (
          createVk $
          set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO &*
          set @"pNext" VK_NULL &*
          set @"sampleShadingEnable" VK_FALSE &*
          set @"rasterizationSamples" VK_SAMPLE_COUNT_1_BIT &*
          set @"minSampleShading" 1 &*
          set @"pSampleMask" VK_NULL &*
          set @"alphaToCoverageEnable" VK_FALSE &*
          set @"alphaToOneEnable" VK_FALSE
        ) &*
        set @"pDepthStencilState" VK_NULL &*
        setVkRef @"pColorBlendState" (
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
          setVec @"blendConstants" (vec4 0 0 0 0)
        ) &*
        set @"pDynamicState" VK_NULL &*
        set @"renderPass" renderPass &*
        set @"subpass" 0 &*
        set @"layout" pipelineLayout &*
        set @"basePipelineHandle" VK_NULL_HANDLE &*
        set @"basePipelineIndex" (-1)

    createSwapchainFramebuffers :: MonadIO io => VkDevice -> VkRenderPass -> VkExtent2D -> [VkImageView] -> ResourceT io [VkFramebuffer]
    createSwapchainFramebuffers device renderPass swapchainExtent swapchainImageViews =
      allocateAcquire_ $
      forM swapchainImageViews $ \imageView ->
      newFramebuffer device $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"renderPass" renderPass &*
      set @"attachmentCount" 1 &*
      setListRef @"pAttachments" [imageView] &*
      set @"width" (getField @"width" swapchainExtent) &*
      set @"height" (getField @"height" swapchainExtent) &*
      set @"layers" 1

    prepareBuffer ::
      (MonadUnliftIO io, Storable vs, PrimBytes vs) =>
      VkDevice ->
      VkCommandPool ->
      VkQueue ->
      VkPhysicalDeviceMemoryProperties ->
      vs ->
      VkBufferUsageFlags ->
      ResourceT io (VkBuffer, VkDeviceMemory)
    prepareBuffer device commandPool copySubmissionQueue physDevMemProps vertices usageFlags =
      runResourceT $ do
        (stagingBuffer, stagingBufferMemory) <-
          createAllocatedBuffer
            device
            bufferSize
            VK_BUFFER_USAGE_TRANSFER_SRC_BIT
            VK_SHARING_MODE_EXCLUSIVE
            (findMemoryType' $ VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)

        liftIO $ with (mappedMemory device stagingBufferMemory 0 bufferSize) $ \ptr ->
          poke (castPtr ptr) vertices

        (vertexBuffer, vertexBufferMemory) <-
          lift $
          createAllocatedBuffer
            device
            bufferSize
            (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. usageFlags)
            VK_SHARING_MODE_EXCLUSIVE
            (findMemoryType' VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)

        copyBuffer device commandPool copySubmissionQueue stagingBuffer vertexBuffer bufferSize

        return (vertexBuffer, vertexBufferMemory)

      where
        bufferSize = fromIntegral $ bSizeOf vertices
        findMemoryType' = findMemoryType physDevMemProps

    createUniformBuffer :: MonadIO io => VkDevice -> VkPhysicalDeviceMemoryProperties -> ResourceT io (VkBuffer, VkDeviceMemory)
    createUniformBuffer device physDevMemProps =
      createAllocatedBuffer
        device
        bufferSize
        VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
        VK_SHARING_MODE_EXCLUSIVE
        (findMemoryType' $ VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
      where
        bufferSize = fromIntegral $ bSizeOf @UniformBufferObject undefined
        findMemoryType' = findMemoryType physDevMemProps

    createAllocatedBuffer :: MonadIO io => VkDevice -> VkDeviceSize -> VkBufferUsageFlags -> VkSharingMode -> (Word32 -> Word32) -> ResourceT io (VkBuffer, VkDeviceMemory)
    createAllocatedBuffer device size usageFlags sharingMode memoryTypeIndexFromMemoryTypeBits = do
      buffer <-
        allocateAcquire_ $
        newBuffer device $
        createVk $
        set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO &*
        set @"pNext" VK_NULL &*
        set @"size" size &*
        set @"usage" usageFlags &*
        set @"sharingMode" sharingMode &*
        set @"pQueueFamilyIndices" VK_NULL

      memReqs <- getBufferMemoryRequirements device buffer

      bufferMemory <-
        allocateAcquire_ $
        allocatedMemory device $
        createVk $
        set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO &*
        set @"pNext" VK_NULL &*
        set @"allocationSize" (getField @"size" memReqs) &*
        set @"memoryTypeIndex" (memoryTypeIndexFromMemoryTypeBits $ getField @"memoryTypeBits" memReqs)

      liftIO $ vkBindBufferMemory device buffer bufferMemory 0

      return (buffer, bufferMemory)

    createAllocatedImage :: MonadIO io => VkDevice -> Word32 -> Word32 -> VkFormat -> VkImageTiling -> VkImageUsageFlags -> VkSharingMode -> (Word32 -> Word32) -> ResourceT io (VkImage, VkDeviceMemory)
    createAllocatedImage device width height format tiling usageFlags sharingMode memoryTypeIndexFromMemoryTypeBits = do
      image <-
        allocateAcquire_ $
        newImage device $
        createVk $
        set @"sType" VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO &*
        set @"pNext" VK_NULL &*
        set @"imageType" VK_IMAGE_TYPE_2D &*
        setVk @"extent" (
          set @"width" width &*
          set @"height" height &*
          set @"depth" 1
        ) &*
        set @"mipLevels" 1 &*
        set @"arrayLayers" 1 &*
        set @"format" format &*
        set @"tiling" tiling &*
        set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED &*
        set @"usage" usageFlags &*
        set @"samples" VK_SAMPLE_COUNT_1_BIT &*
        set @"sharingMode" sharingMode &*
        set @"pQueueFamilyIndices" VK_NULL

      memReqs <- getImageMemoryRequirements device image

      imageMemory <-
        allocateAcquire_ $
        allocatedMemory device $
        createVk $
        set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO &*
        set @"pNext" VK_NULL &*
        set @"allocationSize" (getField @"size" memReqs) &*
        set @"memoryTypeIndex" (memoryTypeIndexFromMemoryTypeBits $ getField @"memoryTypeBits" memReqs)

      liftIO $ vkBindImageMemory device image imageMemory 0

      return (image, imageMemory)

    transitionImageLayout :: MonadUnliftIO io => VkDevice -> VkCommandPool -> VkQueue -> VkImage -> VkFormat -> VkImageLayout -> VkImageLayout -> io ()
    transitionImageLayout device commandPool submissionQueue image format oldLayout newLayout =
      submitCommands device commandPool submissionQueue $ \commandBuffer ->
        cmdPipelineBarrier commandBuffer srcStage dstStage 0 [] [] [imageBarrier]

      where
        imageBarrier =
          createVk @VkImageMemoryBarrier $
          set @"sType" VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER &*
          set @"pNext" VK_NULL &*
          set @"oldLayout" oldLayout &*
          set @"newLayout" newLayout &*
          set @"srcQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED &*
          set @"dstQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED &*
          set @"image" image &*
          setVk @"subresourceRange" (
            set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT &*
            set @"baseMipLevel" 0 &*
            set @"levelCount" 1 &*
            set @"baseArrayLayer" 0 &*
            set @"layerCount" 1
          ) &*
          set @"srcAccessMask" srcAccessMask &*
          set @"dstAccessMask" dstAccessMask

        (srcAccessMask :: VkAccessFlags, srcStage :: VkPipelineStageFlags, dstAccessMask :: VkAccessFlags, dstStage :: VkPipelineStageFlags) =
          case (oldLayout, newLayout) of
            (VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) ->
              (
                0, VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                VK_ACCESS_TRANSFER_WRITE_BIT, VK_PIPELINE_STAGE_TRANSFER_BIT
              )
            (VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) ->
              (
                VK_ACCESS_TRANSFER_WRITE_BIT, VK_PIPELINE_STAGE_TRANSFER_BIT,
                VK_ACCESS_SHADER_READ_BIT, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
              )
            _ ->
              throwAppEx "Unimplemented layout transition."

    copyBufferToImage :: MonadUnliftIO io => VkDevice -> VkCommandPool -> VkQueue -> VkBuffer -> VkImage -> Word32 -> Word32 -> io ()
    copyBufferToImage device commandPool submissionQueue buffer image width height =
      submitCommands device commandPool submissionQueue $ \commandBuffer ->
        cmdCopyBufferToImage commandBuffer buffer image VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL [copyRegion]

      where
        copyRegion =
          createVk @VkBufferImageCopy $
          set @"bufferOffset" 0 &*
          set @"bufferRowLength" 0 &*
          set @"bufferImageHeight" 0 &*
          setVk @"imageSubresource" (
            set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT &*
            set @"mipLevel" 0 &*
            set @"baseArrayLayer" 0 &*
            set @"layerCount" 1
          ) &*
          setVk @"imageOffset" (set @"x" 0 &* set @"y" 0 &* set @"z" 0) &*
          setVk @"imageExtent" (set @"width" width &* set @"height" height &* set @"depth" 1)

    findMemoryType :: VkPhysicalDeviceMemoryProperties -> VkMemoryPropertyFlags -> Word32 -> Word32
    findMemoryType memProperties propertyFlags typeFilter =
      fromMaybe (throwAppEx "Failed to find a suitable memory type.") $
      listToMaybe $
      filter (isMatch . fromIntegral) $
      [0 .. getField @"memoryTypeCount" memProperties - 1]
      where
        memoryTypes = getVec @"memoryTypes" memProperties
        isMatch i = testBit typeFilter i && propertyFlags == (propertyFlags .&. getField @"propertyFlags" (ixOff i memoryTypes))

    submitCommands :: MonadUnliftIO io => VkDevice -> VkCommandPool -> VkQueue -> (forall m. MonadIO m => VkCommandBuffer -> m a) -> io a
    submitCommands device commandPool submissionQueue fillCommandBuffer = runResourceT $ do
      [commandBuffer] <- allocateCommandBuffers device commandPool 1

      result <- with_ (recordingCommandBuffer commandBuffer commandBufferBeginInfo) (fillCommandBuffer commandBuffer)

      queueSubmit
        submissionQueue
        [
          createVk $
          set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO &*
          set @"pNext" VK_NULL &*
          set @"waitSemaphoreCount" 0 &*
          set @"pWaitSemaphores" VK_NULL &*
          set @"pWaitDstStageMask" VK_NULL &*
          set @"commandBufferCount" 1 &*
          setListRef @"pCommandBuffers" [commandBuffer] &*
          set @"signalSemaphoreCount" 0 &*
          set @"pSignalSemaphores" VK_NULL
        ]
        VK_NULL_HANDLE

      liftIO $ vkQueueWaitIdle submissionQueue & onVkFailureThrow "vkQueueWaitIdle failed."

      return result

      where
        commandBufferBeginInfo =
          createVk $
          set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO &*
          set @"pNext" VK_NULL &*
          set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT &*
          set @"pInheritanceInfo" VK_NULL

    copyBuffer :: MonadUnliftIO io => VkDevice -> VkCommandPool -> VkQueue -> VkBuffer -> VkBuffer -> VkDeviceSize -> io ()
    copyBuffer device commandPool submissionQueue srcBuffer dstBuffer bufferSize =
      submitCommands device commandPool submissionQueue $ \commandBuffer ->
        cmdCopyBuffer
          commandBuffer
          srcBuffer
          dstBuffer
          [
            createVk @VkBufferCopy $
            set @"size" bufferSize &*
            set @"srcOffset" 0 &*
            set @"dstOffset" 0
          ]

    allocateCommandBuffers :: MonadIO io => VkDevice -> VkCommandPool -> Word32 -> ResourceT io [VkCommandBuffer]
    allocateCommandBuffers device commandPool count =
      allocateAcquire_ $
      allocatedCommandBuffers device $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"commandPool" commandPool &*
      set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY &*
      set @"commandBufferCount" count

    fillCommandBuffers :: MonadUnliftIO io => VkRenderPass -> VkPipeline -> VkPipelineLayout -> VkExtent2D -> VkBuffer -> VkBuffer -> Word32 -> [(VkCommandBuffer, VkFramebuffer, VkDescriptorSet)] -> io ()
    fillCommandBuffers renderPass graphicsPipeline pipelineLayout swapchainExtent vertexBuffer indexBuffer indexCount commandBuffersWithInfo =
      forM_ commandBuffersWithInfo $ \(commandBuffer, swapchainFramebuffer, descriptorSet) ->
        with_ (recordingCommandBuffer commandBuffer commandBufferBeginInfo) $ do
          cmdBeginRenderPass commandBuffer (renderPassBeginInfo swapchainFramebuffer) VK_SUBPASS_CONTENTS_INLINE
          liftIO $ vkCmdBindPipeline commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
          cmdBindVertexBuffers commandBuffer 0 [(vertexBuffer, 0)]
          liftIO $ vkCmdBindIndexBuffer commandBuffer indexBuffer 0 VK_INDEX_TYPE_UINT16
          cmdBindDescriptorSets commandBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 [descriptorSet] []
          liftIO $ vkCmdDrawIndexed commandBuffer indexCount 1 0 0 0
          liftIO $ vkCmdEndRenderPass commandBuffer

      where
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
              setVk @"color" (setVec @"float32" $ vec4 0 0 0 1)
            )
          ]

    updateUniformBufferMemory :: MonadIO io => VkDevice -> VkDeviceMemory -> Float -> TimeSpec -> io ()
    updateUniformBufferMemory device uniformBufferMemory aspectRatio timeOffset = do
      let
        secondsOffset = (0.000000001 *) . fromInteger . toNanoSecs $ timeOffset
        ubo =
          UniformBufferObject {
            uboModel = rotateZ (0.5 * secondsOffset * pi),
            uboView = lookAt (vec3 0 0 1) (vec3 2 2 2) (vec3 0 0 0),
            uboProj = perspective 0.1 10 (pi / 4) aspectRatio
          }

      liftIO $ with (mappedMemory device uniformBufferMemory 0 bufferSize) $ \ptr ->
        poke (castPtr ptr) (scalar ubo)

      where
        bufferSize = fromIntegral $ bSizeOf @UniformBufferObject undefined

    drawFrame :: MonadIO io => VkDevice -> VkSwapchainKHR -> VkQueue -> VkQueue -> [VkCommandBuffer] -> [VkDeviceMemory] -> Float -> TimeSpec -> VkFence -> VkSemaphore -> VkSemaphore -> io Bool
    drawFrame device swapchain graphicsQueue presentQueue commandBuffers uniformBufferMemories aspectRatio renderStartTime inFlightFence imageAvailableSemaphore renderFinishedSemaphore = do
      waitForFences device [inFlightFence] VK_TRUE maxBound
      resetFences device [inFlightFence]

      (acquireResult, nextImageIndex) <- acquireNextImageIndex device swapchain maxBound imageAvailableSemaphore VK_NULL_HANDLE

      case acquireResult of
        VK_ERROR_OUT_OF_DATE_KHR -> return True
        r | r `notElem` [VK_SUCCESS, VK_SUBOPTIMAL_KHR] -> liftIO $ throwIO $ VulkanException r "vkAcquireNextImageKHR failed."
        _ -> do
          currentTime <- liftIO $ getTime Monotonic
          updateUniformBufferMemory device (uniformBufferMemories !! fromIntegral nextImageIndex) aspectRatio (currentTime - renderStartTime)

          queueSubmit
            graphicsQueue
            [
              createVk (
                set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO &*
                set @"pNext" VK_NULL &*
                set @"waitSemaphoreCount" 1 &*
                setListRef @"pWaitSemaphores" [imageAvailableSemaphore] &*
                setListRef @"pWaitDstStageMask" [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT] &*
                set @"commandBufferCount" 1 &*
                setListRef @"pCommandBuffers" [commandBuffers !! fromIntegral nextImageIndex] &*
                set @"signalSemaphoreCount" 1 &*
                setListRef @"pSignalSemaphores" [renderFinishedSemaphore]
              )
            ]
            inFlightFence

          queuePresentResult <-
            queuePresent presentQueue $
              createVk $
              set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR &*
              set @"pNext" VK_NULL &*
              set @"waitSemaphoreCount" 1 &*
              setListRef @"pWaitSemaphores" [renderFinishedSemaphore] &*
              set @"swapchainCount" 1 &*
              setListRef @"pSwapchains" [swapchain] &*
              setListRef @"pImageIndices" [nextImageIndex] &*
              set @"pResults" VK_NULL

          case queuePresentResult of
            r | r `elem` [VK_ERROR_OUT_OF_DATE_KHR, VK_SUBOPTIMAL_KHR] -> return True
            r | r /= VK_SUCCESS -> liftIO $ throwIO $ VulkanException r "vkQueuePresentKHR failed."
            _ -> return False

data Vertex =
  Vertex {
    vtxPos :: Vec2f,
    vtxColor :: Vec3f
  } deriving (Eq, Show, Generic)

instance PrimBytes Vertex

vertexBindingDescriptionAt :: Word32 -> VkVertexInputRate -> VkVertexInputBindingDescription
vertexBindingDescriptionAt binding inputRate =
  createVk @VkVertexInputBindingDescription $
  set @"binding" binding &*
  set @"stride" (fromIntegral $ sizeOf @(Scalar Vertex) undefined) &*
  set @"inputRate" inputRate

vertexAttributeDescriptionsAt :: Word32 -> [VkVertexInputAttributeDescription]
vertexAttributeDescriptionsAt binding =
  createVk @VkVertexInputAttributeDescription <$> [
    set @"binding" binding &*
    set @"location" 0 &*
    set @"format" VK_FORMAT_R32G32_SFLOAT &*
    set @"offset" 0,

    set @"binding" binding &*
    set @"location" 1 &*
    set @"format" VK_FORMAT_R32G32B32_SFLOAT &*
    set @"offset" (fromIntegral $ sizeOf @Vec2f undefined)
  ]

data UniformBufferObject =
  UniformBufferObject {
    uboModel :: Mat44f,
    uboView :: Mat44f,
    uboProj :: Mat44f
  } deriving (Eq, Show, Generic)

instance PrimBytes UniformBufferObject

registerDebugCallback :: MonadUnliftIO io => VkInstance -> ResourceT io ()
registerDebugCallback vulkanInstance = do
  debugCallbackPtr <- allocateAcquire_ $ newFunPtrFrom $ newVkDebugReportCallbackEXT debugCallback
  void $ allocateAcquire_ $
    registeredVkDebugReportCallbackEXT vulkanInstance $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT &*
      set @"pNext" VK_NULL &*
      set @"flags" (
        VK_DEBUG_REPORT_ERROR_BIT_EXT .|.
        VK_DEBUG_REPORT_WARNING_BIT_EXT .|.
        VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT .|.
        VK_DEBUG_REPORT_INFORMATION_BIT_EXT .|.
        VK_DEBUG_REPORT_DEBUG_BIT_EXT
      ) &*
      set @"pfnCallback" debugCallbackPtr

  where
    debugCallback :: HS_vkDebugReportCallbackEXT
    debugCallback flags objectType object location messageCode layerPrefixPtr messagePtr userDataPtr = do
      message <- peekCString messagePtr
      putStrLn $ "Validation Layer: " ++ message
      return VK_FALSE

data VulkanException =
  VulkanException {
    vkexResult :: VkResult,
    vkexMessage :: String
  } deriving (Eq, Show, Read)

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

fillBufferWithShaderFileContents :: MonadUnliftIO io => FilePath -> ResourceT io (CSize, Ptr Word8)
fillBufferWithShaderFileContents path = do
  (bufferPtr, alignedSize, bytesRead) <- runResourceT $ do
    handle <- allocate_ (openBinaryFile path ReadMode) hClose
    fileSize <- liftIO $ hFileSize handle

    -- Vulkan requires SPIR-V bytecode to have an alignment of 4 bytes.
    let alignedSize = fromIntegral . (4 *) . (`div` 4) . (3 +) $ fileSize

    bufferPtr <- lift $ allocate_ (mallocArray @Word8 alignedSize) free
    bytesRead <- liftIO $ hGetBuf handle bufferPtr alignedSize

    return (bufferPtr, alignedSize, bytesRead)

  liftIO $ pokeArray (castPtr @_ @Word8 . plusPtr bufferPtr $ bytesRead) $ replicate (alignedSize - bytesRead) 0

  return (fromIntegral alignedSize, bufferPtr)

newShaderModule :: VkDevice -> VkShaderModuleCreateInfo -> Acquire VkShaderModule
newShaderModule = newDeviceVk "vkCreateShaderModule" vkCreateShaderModule vkDestroyShaderModule

createShaderModuleFromFile :: MonadUnliftIO io => VkDevice -> FilePath -> ResourceT io VkShaderModule
createShaderModuleFromFile device path = do
  runResourceT $ do
    (bufferSize, bufferPtr) <- fillBufferWithShaderFileContents path
    lift $
      allocateAcquire_ $
      newShaderModule device $
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"flags" 0 &*
      set @"codeSize" bufferSize &*
      set @"pCode" (castPtr bufferPtr)

newRenderPass :: VkDevice -> VkRenderPassCreateInfo -> Acquire VkRenderPass
newRenderPass = newDeviceVk "vkCreateRenderPass" vkCreateRenderPass vkDestroyRenderPass

newPipelineLayout :: VkDevice -> VkPipelineLayoutCreateInfo -> Acquire VkPipelineLayout
newPipelineLayout = newDeviceVk "vkCreatePipelineLayout" vkCreatePipelineLayout vkDestroyPipelineLayout

newDescriptorSetLayout :: VkDevice -> VkDescriptorSetLayoutCreateInfo -> Acquire VkDescriptorSetLayout
newDescriptorSetLayout = newDeviceVk "vkCreateDescriptorSetLayout" vkCreateDescriptorSetLayout vkDestroyDescriptorSetLayout

newDescriptorPool :: VkDevice -> VkDescriptorPoolCreateInfo -> Acquire VkDescriptorPool
newDescriptorPool = newDeviceVk "vkCreateDescriptorPool" vkCreateDescriptorPool vkDestroyDescriptorPool

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

newSemaphore :: VkDevice -> VkSemaphoreCreateInfo -> Acquire VkSemaphore
newSemaphore = newDeviceVk "vkCreateSemaphore" vkCreateSemaphore vkDestroySemaphore

newFence :: VkDevice -> VkFenceCreateInfo -> Acquire VkFence
newFence = newDeviceVk "vkCreateFence" vkCreateFence vkDestroyFence

newBuffer :: VkDevice -> VkBufferCreateInfo -> Acquire VkBuffer
newBuffer = newDeviceVk "vkCreateBuffer" vkCreateBuffer vkDestroyBuffer

newImage :: VkDevice -> VkImageCreateInfo -> Acquire VkImage
newImage = newDeviceVk "vkCreateImage" vkCreateImage vkDestroyImage

allocatedMemory :: VkDevice -> VkMemoryAllocateInfo -> Acquire VkDeviceMemory
allocatedMemory device allocateInfo =
  (
    withPtr allocateInfo $ \allocateInfoPtr ->
      alloca $ \deviceMemoryPtr -> do
        vkAllocateMemory device allocateInfoPtr VK_NULL deviceMemoryPtr &
          onVkFailureThrow "vkAllocateMemory failed."
        peek deviceMemoryPtr
  )
  `mkAcquire`
  \deviceMemory ->
    vkFreeMemory device deviceMemory VK_NULL

mappedMemory :: VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> Acquire (Ptr Void)
mappedMemory device deviceMemory offset size =
  (
    alloca $ \ptrPtr -> do
      vkMapMemory device deviceMemory offset size 0 ptrPtr
      peek ptrPtr
  )
  `mkAcquire`
  const (vkUnmapMemory device deviceMemory)

allocatedCommandBuffers :: VkDevice -> VkCommandBufferAllocateInfo -> Acquire [VkCommandBuffer]
allocatedCommandBuffers device allocateInfo =
  (
    withPtr allocateInfo $ \allocateInfoPtr ->
      allocaArray commandBufferCount $ \commandBuffersPtr -> do
        vkAllocateCommandBuffers device allocateInfoPtr commandBuffersPtr &
          onVkFailureThrow "vkAllocateCommandBuffers failed."
        peekArray commandBufferCount commandBuffersPtr
  )
  `mkAcquire`
  \commandBuffers ->
    withArray commandBuffers $ vkFreeCommandBuffers device commandPool (fromIntegral commandBufferCount)
  where
    commandBufferCount = fromIntegral $ getField @"commandBufferCount" allocateInfo
    commandPool = getField @"commandPool" allocateInfo

allocatedDescriptorSets :: VkDevice -> Bool -> VkDescriptorSetAllocateInfo -> Acquire [VkDescriptorSet]
allocatedDescriptorSets device canFree allocateInfo =
  (
    withPtr allocateInfo $ \allocateInfoPtr ->
      allocaArray descriptorSetCount $ \descriptorSetsPtr -> do
        vkAllocateDescriptorSets device allocateInfoPtr descriptorSetsPtr &
          onVkFailureThrow "vkAllocateDescriptorSets failed."
        peekArray descriptorSetCount descriptorSetsPtr
  )
  `mkAcquire`
  (
    if canFree then
      \descriptorSets ->
        withArray descriptorSets $ \descriptorSetsPtr ->
          vkFreeDescriptorSets device descriptorPool (fromIntegral descriptorSetCount) descriptorSetsPtr &
            onVkFailureThrow "vkFreeDescriptorSets failed."
    else
      const $ return ()
  )
  where
    descriptorSetCount = fromIntegral $ getField @"descriptorSetCount" allocateInfo
    descriptorPool = getField @"descriptorPool" allocateInfo

recordingCommandBuffer :: VkCommandBuffer -> VkCommandBufferBeginInfo -> Acquire ()
recordingCommandBuffer commandBuffer beginInfo =
  (
    withPtr beginInfo $ \beginInfoPtr ->
      vkBeginCommandBuffer commandBuffer beginInfoPtr &
        onVkFailureThrow "vkBeginCommandBuffer failed."
  )
  `mkAcquire`
  const (
    vkEndCommandBuffer commandBuffer &
      onVkFailureThrow "vkEndCommandBuffer failed."
  )

updateDescriptorSets :: MonadIO io => VkDevice -> [VkWriteDescriptorSet] -> [VkCopyDescriptorSet] -> io ()
updateDescriptorSets device writes copies =
  liftIO $
  withArray writes $ \writesPtr ->
  withArray copies $ \copiesPtr ->
    vkUpdateDescriptorSets device (lengthNum writes) writesPtr (lengthNum copies) copiesPtr

cmdBeginRenderPass :: MonadIO io => VkCommandBuffer -> VkRenderPassBeginInfo -> VkSubpassContents -> io ()
cmdBeginRenderPass commandBuffer beginInfo subpassContents =
  liftIO $ withPtr beginInfo $ \beginInfoPtr ->
    vkCmdBeginRenderPass commandBuffer beginInfoPtr subpassContents

cmdBindVertexBuffers :: MonadIO io => VkCommandBuffer -> Word32 -> [(VkBuffer, VkDeviceSize)] -> io ()
cmdBindVertexBuffers commandBuffer firstBinding bindings =
  liftIO $
  withArray (fst <$> bindings) $ \buffersPtr ->
  withArray (snd <$> bindings) $ \offsetsPtr ->
  vkCmdBindVertexBuffers commandBuffer firstBinding (lengthNum bindings) buffersPtr offsetsPtr

cmdBindDescriptorSets :: MonadIO io => VkCommandBuffer -> VkPipelineBindPoint -> VkPipelineLayout -> Word32 -> [VkDescriptorSet] -> [Word32] -> io ()
cmdBindDescriptorSets commandBuffer bindPoint pipelineLayout firstSet descriptorSets dynamicOffsets =
  liftIO $
  withArray descriptorSets $ \descriptorSetsPtr ->
  withArray dynamicOffsets $ \dynamicOffsetsPtr ->
  vkCmdBindDescriptorSets commandBuffer bindPoint pipelineLayout firstSet (lengthNum descriptorSets) descriptorSetsPtr (lengthNum dynamicOffsets) dynamicOffsetsPtr

cmdCopyBuffer :: MonadIO io => VkCommandBuffer -> VkBuffer -> VkBuffer -> [VkBufferCopy] -> io ()
cmdCopyBuffer commandBuffer srcBuffer dstBuffer copyRegions =
  liftIO $
  withArray copyRegions $ \copyRegionsPtr ->
  vkCmdCopyBuffer commandBuffer srcBuffer dstBuffer (lengthNum copyRegions) copyRegionsPtr

cmdCopyBufferToImage :: MonadIO io => VkCommandBuffer -> VkBuffer -> VkImage -> VkImageLayout -> [VkBufferImageCopy] -> io ()
cmdCopyBufferToImage commandBuffer srcBuffer dstImage dstImageLayout copyRegions =
  liftIO $
  withArray copyRegions $ \copyRegionsPtr ->
  vkCmdCopyBufferToImage commandBuffer srcBuffer dstImage dstImageLayout (lengthNum copyRegions) copyRegionsPtr

cmdPipelineBarrier ::
  MonadIO io =>
  VkCommandBuffer ->
  VkPipelineStageFlags ->
  VkPipelineStageFlags ->
  VkDependencyFlags ->
  [VkMemoryBarrier] ->
  [VkBufferMemoryBarrier] ->
  [VkImageMemoryBarrier] ->
  io ()
cmdPipelineBarrier
  commandBuffer
  srcStageMask
  dstStageMask
  depFlags
  memoryBarriers
  bufferMemoryBarriers
  imageMemoryBarriers
  =
  liftIO $
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

queueSubmit :: MonadIO io => VkQueue -> [VkSubmitInfo] -> VkFence -> io ()
queueSubmit queue submitInfos fence =
  liftIO $ withArray submitInfos $ \submitInfosPtr ->
    vkQueueSubmit queue (lengthNum submitInfos) submitInfosPtr fence &
      onVkFailureThrow "vkQueueSubmit failed."

queuePresent :: MonadIO io => VkQueue -> VkPresentInfoKHR -> io VkResult
queuePresent queue presentInfo =
  liftIO $ withPtr presentInfo $ \presentInfoPtr ->
    vkQueuePresentKHR queue presentInfoPtr

waitForFences :: MonadIO io => VkDevice -> [VkFence] -> VkBool32 -> Word64 -> io ()
waitForFences device fences shouldWaitAll timeout =
  liftIO $ withArray fences $ \fencesPtr ->
    vkWaitForFences device (lengthNum fences) fencesPtr shouldWaitAll timeout & void

resetFences :: MonadIO io => VkDevice -> [VkFence] -> io ()
resetFences device fences =
  liftIO $ withArray fences $ \fencesPtr ->
    vkResetFences device (lengthNum fences) fencesPtr & void

getDeviceQueue :: MonadIO io => VkDevice -> Word32 -> Word32 -> io VkQueue
getDeviceQueue device queueFamilyIndex queueIndex =
  liftIO $ alloca $ \deviceQueuePtr -> do
    vkGetDeviceQueue device queueFamilyIndex 0 deviceQueuePtr
    peek deviceQueuePtr

newFunPtrFrom :: IO (FunPtr f) -> Acquire (FunPtr f)
newFunPtrFrom = flip mkAcquire freeHaskellFunPtr

registeredVkDebugReportCallbackEXT :: VkInstance -> VkDebugReportCallbackCreateInfoEXT -> Acquire VkDebugReportCallbackEXT
registeredVkDebugReportCallbackEXT vulkanInstance createInfo = do
  createDebugReportCallbackEXT <- liftIO $ vkGetInstanceProc @VkCreateDebugReportCallbackEXT vulkanInstance
  destroyDebugReportCallbackEXT <- liftIO $ vkGetInstanceProc @VkDestroyDebugReportCallbackEXT vulkanInstance
  newVk "vkCreateDebugReportCallbackEXT" (createDebugReportCallbackEXT vulkanInstance) (destroyDebugReportCallbackEXT vulkanInstance) createInfo

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

acquireNextImageIndex :: MonadIO io => VkDevice -> VkSwapchainKHR -> Word64 -> VkSemaphore -> VkFence -> io (VkResult, Word32)
acquireNextImageIndex device swapchain timeout semaphore fence =
  liftIO $ alloca $ \imageIndexPtr -> do
    result <- vkAcquireNextImageKHR device swapchain timeout semaphore fence imageIndexPtr
    (result,) <$> peek imageIndexPtr

getBufferMemoryRequirements :: MonadIO io => VkDevice -> VkBuffer -> io VkMemoryRequirements
getBufferMemoryRequirements device buffer =
  liftIO $ alloca $ \memReqsPtr -> do
    vkGetBufferMemoryRequirements device buffer memReqsPtr
    peek memReqsPtr

getImageMemoryRequirements :: MonadIO io => VkDevice -> VkImage -> io VkMemoryRequirements
getImageMemoryRequirements device buffer =
  liftIO $ alloca $ \memReqsPtr -> do
    vkGetImageMemoryRequirements device buffer memReqsPtr
    peek memReqsPtr

getPhysicalDeviceMemoryProperties :: MonadIO io => VkPhysicalDevice -> io VkPhysicalDeviceMemoryProperties
getPhysicalDeviceMemoryProperties physicalDevice =
  liftIO $ alloca $ \memPropsPtr -> do
    vkGetPhysicalDeviceMemoryProperties physicalDevice memPropsPtr
    peek memPropsPtr

windowEventLoop :: MonadIO io => GLFW.Window -> IORef (Maybe TimeSpec) -> io Bool -> io Bool
windowEventLoop window lastResizeTimeRef body = fix $ \loop ->
  liftIO (GLFW.windowShouldClose window) >>= \case
    True -> return False
    False -> do
      liftIO GLFW.pollEvents
      shouldResize <- liftIO $ do
        currentTime <- getTime Monotonic
        atomicModifyIORef lastResizeTimeRef $ \case
          Just lastResizeTime | currentTime - lastResizeTime >= resizeDelay -> (Nothing, True)
          v -> (v, False)
      if shouldResize then
        return True
      else
        body >>= bool loop (return True)
  where
    resizeDelay = fromNanoSecs (100 * 1000 * 1000) -- 100 milliseconds

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 1 <&>

ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap

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

ioPutStrLn :: MonadIO io => String -> io ()
ioPutStrLn = liftIO . putStrLn

evalStateTWith :: Monad m => s -> StateT s m a -> m a
evalStateTWith = flip evalStateT

doWhileM :: Monad m => m Bool -> m ()
doWhileM a = fix $ \loop -> a >>= bool (return ()) loop

lengthNum :: (Foldable t, Num n) => t a -> n
lengthNum = fromIntegral . length
