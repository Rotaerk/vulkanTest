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
      ioPutStrLn "Obtained the window surface."

      (physicalDevice, qfi, scsd) <-
        mapM (liftIO . peekCString) deviceExtensions >>=
        getFirstSuitablePhysicalDeviceAndProperties vulkanInstance surface
      ioPutStrLn "Found a suitable physical device."

      let distinctQfi = qfiDistinct qfi

      device <- allocateAcquire_ $ newVkDevice physicalDevice $ configureVkDevice distinctQfi deviceExtensions
      ioPutStrLn "Vulkan device created."

      graphicsQueue <- getDeviceQueue device (qfiGraphics qfi) 0
      ioPutStrLn "Obtained the graphics queue."

      presentQueue <- getDeviceQueue device (qfiPresent qfi) 0
      ioPutStrLn "Obtained the present queue."

      let
        surfaceCapabilities = scsdCapabilities scsd
        swapchainSurfaceFormat = chooseSwapchainSurfaceFormat (scsdSurfaceFormats scsd)
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
      ioPutStrLn "Obtained the swapchain images."

      swapchainImageViews <-
        allocateAcquire_ $
        newVkImageViews device $
        configureVkImageView (getField @"format" swapchainSurfaceFormat) <$> swapchainImages
      ioPutStrLn "Swapchain image views created."

      do
        (vertShaderModuleKey, vertShaderModule) <- createShaderModuleFromFile device (shadersPath </> "shader.vert.spv")
        ioPutStrLn "Created vertex shader module."
        (fragShaderModuleKey, fragShaderModule) <- createShaderModuleFromFile device (shadersPath </> "shader.frag.spv")
        ioPutStrLn "Created fragment shader module."

        let
          shaderStages =
            [
              configurePipelineShaderStage VK_SHADER_STAGE_VERTEX_BIT vertShaderModule "main",
              configurePipelineShaderStage VK_SHADER_STAGE_FRAGMENT_BIT fragShaderModule "main"
            ]

        release fragShaderModuleKey
        ioPutStrLn "Destroyed fragment shader module."
        release vertShaderModuleKey
        ioPutStrLn "Destroyed vertex shader module."

      ioPutStrLn "Entering main loop."
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
      createVk @VkApplicationInfo $
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
  createVk @VkDebugReportCallbackCreateInfoEXT $
  set @"sType" VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT &*
  set @"flags" debugReportFlags &*
  set @"pfnCallback" debugCallbackPtr &*
  set @"pNext" VK_NULL

configureVkInstance :: VkApplicationInfo -> [String] -> [CString] -> VkInstanceCreateInfo
configureVkInstance applicationInfo validationLayers extensions =
  createVk @VkInstanceCreateInfo $
  set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO &*
  setVkRef @"pApplicationInfo" applicationInfo &*
  set @"enabledExtensionCount" (fromIntegral $ length extensions) &*
  setListRef @"ppEnabledExtensionNames" extensions &*
  set @"enabledLayerCount" (fromIntegral $ length validationLayers) &*
  setStrListRef @"ppEnabledLayerNames" validationLayers &*
  set @"pNext" VK_NULL

configureVkDevice :: [Word32] -> [CString] -> VkDeviceCreateInfo
configureVkDevice queueFamilyIndices deviceExtensions =
  createVk @VkDeviceCreateInfo $
  set @"sType" VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" 0 &*
  set @"queueCreateInfoCount" (fromIntegral $ length queueFamilyIndices) &*
  setListRef @"pQueueCreateInfos" (
    queueFamilyIndices <&> \qfi ->
      createVk @VkDeviceQueueCreateInfo $
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
    createVk @VkPhysicalDeviceFeatures $ handleRemFields @_ @'[]
  )

chooseSwapchainSurfaceFormat :: [VkSurfaceFormatKHR] -> VkSurfaceFormatKHR
chooseSwapchainSurfaceFormat = \case
  [f] | getField @"format" f == VK_FORMAT_UNDEFINED -> idealSurfaceFormat
  fs -> find (== idealSurfaceFormat) fs & fromMaybe (throwAppEx "Failed to find an appropriate swap surface format.")
  where
    idealSurfaceFormat =
      createVk @VkSurfaceFormatKHR $
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
    createVk @VkExtent2D $
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
  createVk @VkSwapchainCreateInfoKHR $
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
  createVk @VkImageViewCreateInfo $
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
  createVk @VkShaderModuleCreateInfo $
  set @"sType" VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" 0 &*
  set @"codeSize" codeSize &*
  set @"pCode" (castPtr codePtr)

configurePipelineShaderStage :: VkShaderStageFlagBits -> VkShaderModule -> String -> VkPipelineShaderStageCreateInfo
configurePipelineShaderStage stage shaderModule entryPointName =
  createVk @VkPipelineShaderStageCreateInfo $
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" 0 &*
  set @"stage" stage &*
  set @"module" shaderModule &*
  setStrRef @"pName" entryPointName &*
  set @"pSpecializationInfo" VK_NULL

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

newVkInstance :: VkInstanceCreateInfo -> Acquire VkInstance
newVkInstance createInfo =
  (
    withPtr createInfo $ \createInfoPtr ->
      alloca $ \vulkanInstancePtr -> do
        vkCreateInstance createInfoPtr VK_NULL vulkanInstancePtr &
          onVkFailureThrow "vkCreateInstance failed."
        peek vulkanInstancePtr
  )
  `mkAcquire`
  \vulkanInstance -> vkDestroyInstance vulkanInstance VK_NULL

newVkDevice :: VkPhysicalDevice -> VkDeviceCreateInfo -> Acquire VkDevice
newVkDevice physicalDevice createInfo =
  (
    withPtr createInfo $ \createInfoPtr ->
      alloca $ \devicePtr -> do
        vkCreateDevice physicalDevice createInfoPtr VK_NULL devicePtr &
          onVkFailureThrow "vkCreateDevice failed."
        peek devicePtr
  )
  `mkAcquire`
  \device -> vkDestroyDevice device VK_NULL

newVkSwapchain :: VkDevice -> VkSwapchainCreateInfoKHR -> Acquire VkSwapchainKHR
newVkSwapchain device createInfo =
  (
    withPtr createInfo $ \createInfoPtr ->
      alloca $ \swapchainPtr -> do
        vkCreateSwapchainKHR device createInfoPtr VK_NULL swapchainPtr &
          onVkFailureThrow "vkCreateSwapchainKHR failed."
        peek swapchainPtr
  )
  `mkAcquire`
  \swapchain -> vkDestroySwapchainKHR device swapchain VK_NULL

newVkImageViews :: VkDevice -> [VkImageViewCreateInfo] -> Acquire [VkImageView]
newVkImageViews device createInfos =
  (
    forM (zip [0..] createInfos) $ \(idx, createInfo) ->
      withPtr createInfo $ \createInfoPtr ->
        alloca $ \imageViewPtr -> do
          vkCreateImageView device createInfoPtr VK_NULL imageViewPtr &
            onVkFailureThrow ("vkCreateImageView failed for item " ++ show idx ++ ".")
          peek imageViewPtr
  )
  `mkAcquire`
  \imageViews ->
    forM_ imageViews $ \imageView ->
      vkDestroyImageView device imageView VK_NULL

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
newShaderModule device createInfo =
  (
    withPtr createInfo $ \createInfoPtr ->
      alloca $ \shaderModulePtr -> do
        vkCreateShaderModule device createInfoPtr VK_NULL shaderModulePtr &
          onVkFailureThrow "vkCreateShaderModule failed."
        peek shaderModulePtr
  )
  `mkAcquire`
  \shaderModule -> vkDestroyShaderModule device shaderModule VK_NULL

createShaderModuleFromFile :: VkDevice -> FilePath -> ResIO (ReleaseKey, VkShaderModule)
createShaderModuleFromFile device path = do
  (bufferKey, bufferSize, bufferPtr) <- fillBufferWithShaderFileContents path
  shaderModuleWithKey <-
    allocateAcquire $
    newShaderModule device $
    configureShaderModule bufferSize bufferPtr
  release bufferKey
  return shaderModuleWithKey

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
