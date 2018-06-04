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
import Control.Monad.Extra (firstJustM, findM)
import Control.Monad.IO.Class
import Control.Monad.Loops (whileM_)
import Control.Monad.Trans.Maybe
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

data Arguments = Arguments { shadersPath :: String } deriving (Show, Data, Typeable)

main :: IO ()
main =
  do
    arguments <-
      cmdArgs $
      Arguments {
        shadersPath = def &= typ "PATH" &= help "Path to the SPIR-V shader files."
      }
      &= summary "Vulkan Test"
    putStrLn $ "Shaders path is: '" ++ shadersPath arguments ++ "'."
    withGLFW $
      withVulkanGLFWWindow width height "Vulkan" $ \window -> do
        putStrLn "Window created."

        glfwExtensions <- GLFW.getRequiredInstanceExtensions

        unless (null validationLayers) $ do
          ensureValidationLayersSupported validationLayers
          putStrLn "All required validation layers supported."

        withVkInstance (configureVkInstance applicationInfo validationLayers (extensions ++ glfwExtensions)) $ \vulkanInstance -> do
          putStrLn "Vulkan instance created."

          maybeWithDebugCallback vulkanInstance $ do
            withGLFWWindowSurface vulkanInstance window $ \surface -> do
              putStrLn "Obtained the window surface."

              (physicalDevice, qfi, scsd) <-
                getFirstSuitablePhysicalDeviceAndProperties vulkanInstance surface =<< mapM peekCString deviceExtensions
              putStrLn "Found a suitable physical device."

              let distinctQfi = qfiDistinct qfi

              withVkDevice physicalDevice (configureVkDevice distinctQfi deviceExtensions) $ \device -> do
                putStrLn "Vulkan device created."

                graphicsQueue <- getDeviceQueue device (qfiGraphics qfi) 0
                putStrLn "Obtained the graphics queue."

                presentQueue <- getDeviceQueue device (qfiPresent qfi) 0
                putStrLn "Obtained the present queue."

                let surfaceCapabilities = scsdCapabilities scsd
                let swapchainSurfaceFormat = chooseSwapchainSurfaceFormat (scsdSurfaceFormats scsd)
                let swapchainPresentMode = chooseSwapchainPresentMode (scsdPresentModes scsd)
                let swapchainExtent = chooseSwapchainExtent (fromIntegral width) (fromIntegral height) surfaceCapabilities
                let swapchainImageCount = chooseSwapchainImageCount surfaceCapabilities

                withVkSwapchain device (configureVkSwapchain surface surfaceCapabilities swapchainSurfaceFormat swapchainPresentMode swapchainExtent swapchainImageCount distinctQfi) $ \swapchain -> do
                  putStrLn "Swapchain created."

                  swapchainImages <- listSwapchainImages device swapchain
                  putStrLn "Obtained the swapchain images."

                  withVkImageViews device (configureVkImageView (getField @"format" swapchainSurfaceFormat) <$> swapchainImages) $ \swapchainImageViews -> do
                    putStrLn "Swapchain image views created."

                    putStrLn "Entering main loop."
                    mainLoop window
                    putStrLn "Main loop ended, cleaning up."
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

getFirstSuitablePhysicalDeviceAndProperties :: VkInstance -> VkSurfaceKHR -> [String] -> IO (VkPhysicalDevice, QueueFamilyIndices, SwapchainSupportDetails)
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

findQueueFamilyIndices :: VkPhysicalDevice -> VkSurfaceKHR -> MaybeT IO QueueFamilyIndices
findQueueFamilyIndices physicalDevice surface = do
  indexedFamiliesHavingQueues <- liftIO $
    filter ((0 <) . getField @"queueCount" . snd) . zip [0 ..] <$>
    listPhysicalDeviceQueueFamilyProperties physicalDevice

  let findFamilyIndexWhere condIO = MaybeT $ fmap fst <$> findM condIO indexedFamiliesHavingQueues

  graphics <- findFamilyIndexWhere $ return . (zeroBits /=) . (VK_QUEUE_GRAPHICS_BIT .&.) . getField @"queueFlags" . snd

  present <- findFamilyIndexWhere $ \(qfi, _) ->
    alloca $ \isSupportedPtr -> do
      vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi surface isSupportedPtr &
        onVkFailureThrow "vkGetPhysicalDeviceSurfaceSupportKHR failed."
      peek isSupportedPtr <&> (== VK_TRUE)

  return $ QueueFamilyIndices graphics present

maybeWithDebugCallback :: VkInstance -> IO a -> IO a
#ifndef NDEBUG
maybeWithDebugCallback vulkanInstance action =
  withFunPtr (newVkDebugReportCallbackEXT debugCallback) $ \debugCallbackPtr ->
    withVkDebugReportCallbackEXT vulkanInstance (configureVkDebugReportCallbackEXT debugReportFlags debugCallbackPtr) $ \vkDebugReportCallback -> do
      putStrLn "Debug callback setup."
      action
  where
    debugReportFlags :: VkDebugReportFlagsEXT
    debugReportFlags = (VK_DEBUG_REPORT_ERROR_BIT_EXT .|. VK_DEBUG_REPORT_WARNING_BIT_EXT)

    debugCallback :: HS_vkDebugReportCallbackEXT
    debugCallback flags objectType object location messageCode layerPrefixPtr messagePtr userDataPtr = do
      message <- peekCString messagePtr
      putStrLn $ "Validation Layer: " ++ message
      return VK_FALSE
#else
maybeWithDebugCallback _ = id
#endif

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

throwIOAppEx :: String -> IO a
throwIOAppEx message = throwIO $ ApplicationException message

withGLFW :: IO a -> IO a
withGLFW = GLFW.init `bracket_` GLFW.terminate

withVulkanGLFWWindow :: Int -> Int -> String -> (GLFW.Window -> IO a) -> IO a
withVulkanGLFWWindow width height title =
  do
    GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
    GLFW.windowHint $ WindowHint'Resizable False
    GLFW.createWindow width height title Nothing Nothing >>=
      maybe (throwIOAppEx "Failed to initialize the GLFW window.") return
  `bracket`
  GLFW.destroyWindow

withGLFWWindowSurface :: VkInstance -> GLFW.Window -> (VkSurfaceKHR -> IO a) -> IO a
withGLFWWindowSurface vulkanInstance window =
  (
    alloca $ \surfacePtr -> do
      GLFW.createWindowSurface vulkanInstance window nullPtr surfacePtr &
        onVkFailureThrow "GLFW.createWindowSurface failed."
      peek surfacePtr
  )
  `bracket`
  \surface -> vkDestroySurfaceKHR vulkanInstance surface VK_NULL

withVkInstance :: VkInstanceCreateInfo -> (VkInstance -> IO a) -> IO a
withVkInstance createInfo =
  (
    withPtr createInfo $ \createInfoPtr ->
      alloca $ \vulkanInstancePtr -> do
        vkCreateInstance createInfoPtr VK_NULL vulkanInstancePtr &
          onVkFailureThrow "vkCreateInstance failed."
        peek vulkanInstancePtr
  )
  `bracket`
  \vulkanInstance -> vkDestroyInstance vulkanInstance VK_NULL

withVkDevice :: VkPhysicalDevice -> VkDeviceCreateInfo -> (VkDevice -> IO a) -> IO a
withVkDevice physicalDevice createInfo =
  (
    withPtr createInfo $ \createInfoPtr ->
      alloca $ \devicePtr -> do
        vkCreateDevice physicalDevice createInfoPtr VK_NULL devicePtr &
          onVkFailureThrow "vkCreateDevice failed."
        peek devicePtr
  )
  `bracket`
  \device -> vkDestroyDevice device VK_NULL

withVkSwapchain :: VkDevice -> VkSwapchainCreateInfoKHR -> (VkSwapchainKHR -> IO a) -> IO a
withVkSwapchain device createInfo =
  (
    withPtr createInfo $ \createInfoPtr ->
      alloca $ \swapchainPtr -> do
        vkCreateSwapchainKHR device createInfoPtr VK_NULL swapchainPtr &
          onVkFailureThrow "vkCreateSwapchainKHR failed."
        peek swapchainPtr
  )
  `bracket`
  \swapchain -> vkDestroySwapchainKHR device swapchain VK_NULL

withVkImageViews :: VkDevice -> [VkImageViewCreateInfo] -> ([VkImageView] -> IO a) -> IO a
withVkImageViews device createInfos =
  (
    forM (zip [0..] createInfos) $ \(idx, createInfo) ->
      withPtr createInfo $ \createInfoPtr ->
        alloca $ \imageViewPtr -> do
          vkCreateImageView device createInfoPtr VK_NULL imageViewPtr &
            onVkFailureThrow ("vkCreateImageView failed for item " ++ show idx ++ ".")
          peek imageViewPtr
  )
  `bracket`
  \imageViews ->
    forM_ imageViews $ \imageView ->
      vkDestroyImageView device imageView VK_NULL

getDeviceQueue :: VkDevice -> Word32 -> Word32 -> IO VkQueue
getDeviceQueue device queueFamilyIndex queueIndex =
  alloca $ \deviceQueuePtr -> do
    vkGetDeviceQueue device queueFamilyIndex 0 deviceQueuePtr
    peek deviceQueuePtr

withFunPtr :: IO (FunPtr f) -> (FunPtr f -> IO a) -> IO a
withFunPtr createFunPtr = bracket createFunPtr freeHaskellFunPtr

withVkDebugReportCallbackEXT :: VkInstance -> VkDebugReportCallbackCreateInfoEXT -> (VkDebugReportCallbackEXT -> IO a) -> IO a
withVkDebugReportCallbackEXT vulkanInstance createInfo =
    do
      createDebugReportCallbackEXT <- vkGetInstanceProc @VkCreateDebugReportCallbackEXT vulkanInstance
      withPtr createInfo $ \createInfoPtr ->
        alloca $ \vkDebugReportCallbackEXTPtr -> do
          createDebugReportCallbackEXT vulkanInstance createInfoPtr VK_NULL vkDebugReportCallbackEXTPtr &
            onVkFailureThrow "vkCreateDebugReportCallbackEXT failed."
          peek vkDebugReportCallbackEXTPtr
    `bracket`
    \vkDebugReportCallbackEXT -> do
      destroyDebugReportCallbackEXT <- vkGetInstanceProc @VkDestroyDebugReportCallbackEXT vulkanInstance
      destroyDebugReportCallbackEXT vulkanInstance vkDebugReportCallbackEXT VK_NULL

ensureValidationLayersSupported :: [String] -> IO ()
ensureValidationLayersSupported validationLayers = do
  unsupportedLayers <- getUnsupportedValidationLayerNames validationLayers
  unless (null unsupportedLayers) $ throwIOAppEx ("Expected validation layers are not available: " ++ show unsupportedLayers)

getUnsupportedValidationLayerNames :: [String] -> IO [String]
getUnsupportedValidationLayerNames [] = return []
getUnsupportedValidationLayerNames expectedLayerNames =
  listInstanceLayerProperties
  <&> fmap (getStringField @"layerName")
  <&> \case
    [] -> expectedLayerNames
    availableLayerNames -> filter (not . elemOf availableLayerNames) expectedLayerNames

getUnsupportedDeviceExtensionNames :: VkPhysicalDevice -> CString -> [String] -> IO [String]
getUnsupportedDeviceExtensionNames _ _ [] = return []
getUnsupportedDeviceExtensionNames physicalDevice layerName expectedExtensionNames = do
  listPhysicalDeviceExtensionProperties physicalDevice layerName
  <&> fmap (getStringField @"extensionName")
  <&> \case
    [] -> expectedExtensionNames
    availableExtensionNames -> filter (not . elemOf availableExtensionNames) expectedExtensionNames

getVkList :: Storable a => (Ptr Word32 -> Ptr a -> IO ()) -> IO [a]
getVkList getArray =
  alloca $ \countPtr -> do
    getArray countPtr VK_NULL
    count <- fromIntegral <$> peek countPtr
    if count > 0 then
      allocaArray count $ \arrayPtr -> do
        getArray countPtr arrayPtr
        peekArray count arrayPtr
    else
      return []

listInstanceLayerProperties :: IO [VkLayerProperties]
listInstanceLayerProperties =
  getVkList $ \layerCountPtr layersPtr ->
    vkEnumerateInstanceLayerProperties layerCountPtr layersPtr &
      onVkFailureThrow "vkEnumerateInstanceLayerProperties failed."

listPhysicalDevices :: VkInstance -> IO [VkPhysicalDevice]
listPhysicalDevices vulkanInstance =
  getVkList $ \deviceCountPtr devicesPtr ->
    vkEnumeratePhysicalDevices vulkanInstance deviceCountPtr devicesPtr &
      onVkFailureThrow "vkEnumeratePhysicalDevices failed."

listPhysicalDeviceQueueFamilyProperties :: VkPhysicalDevice -> IO [VkQueueFamilyProperties]
listPhysicalDeviceQueueFamilyProperties physicalDevice = getVkList $ vkGetPhysicalDeviceQueueFamilyProperties physicalDevice

listPhysicalDeviceExtensionProperties :: VkPhysicalDevice -> CString -> IO [VkExtensionProperties]
listPhysicalDeviceExtensionProperties physicalDevice layerName =
  getVkList $ \extCountPtr extsPtr ->
    vkEnumerateDeviceExtensionProperties physicalDevice layerName extCountPtr extsPtr &
      onVkFailureThrow "vkEnumerateDeviceExtensionProperties failed."

getPhysicalDeviceSurfaceCapabilities :: VkPhysicalDevice -> VkSurfaceKHR -> IO VkSurfaceCapabilitiesKHR
getPhysicalDeviceSurfaceCapabilities physicalDevice surface =
  alloca $ \capsPtr -> do
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface capsPtr
    peek capsPtr

listPhysicalDeviceSurfaceFormats :: VkPhysicalDevice -> VkSurfaceKHR -> IO [VkSurfaceFormatKHR]
listPhysicalDeviceSurfaceFormats physicalDevice surface =
  getVkList $ \formatCountPtr formatsPtr ->
    vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice surface formatCountPtr formatsPtr &
      onVkFailureThrow "vkGetPhysicalDeviceSurfaceFormatsKHR failed."

listPhysicalDeviceSurfacePresentModes :: VkPhysicalDevice -> VkSurfaceKHR -> IO [VkPresentModeKHR]
listPhysicalDeviceSurfacePresentModes physicalDevice surface =
  getVkList $ \modeCountPtr modesPtr ->
    vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice surface modeCountPtr modesPtr &
      onVkFailureThrow "vkGetPhysicalDeviceSurfacePresentModesKHR failed."

listSwapchainImages :: VkDevice -> VkSwapchainKHR -> IO [VkImage]
listSwapchainImages device swapchain = do
  getVkList $ \imageCountPtr imagesPtr ->
    vkGetSwapchainImagesKHR device swapchain imageCountPtr imagesPtr &
      onVkFailureThrow "vkGetSwapchainImagesKHR failed."

mainLoop :: GLFW.Window -> IO ()
mainLoop window = whileM_ (not <$> GLFW.windowShouldClose window) GLFW.pollEvents

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
