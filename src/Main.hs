{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (init)
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Bits
import Data.Function
import Data.Functor
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
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Proc

main :: IO ()
main =
  (
    withGLFW $
    withVulkanGLFWWindow width height "Vulkan" $ \window -> do
      putStrLn "Window created."

      glfwExtensions <- getGLFWRequiredInstanceExtensions

      unless (null validationLayers) $ do
        ensureValidationLayersSupported validationLayers
        putStrLn "All required validation layers supported."

      withVkInstance applicationInfo validationLayers (extensions ++ glfwExtensions) $ \vkInstance -> do
        putStrLn "Instance created."

        maybeWithDebugCallback vkInstance $ do
          device <- getFirstSuitableDevice isDeviceSuitable vkInstance
          putStrLn "Found a suitable device."
          putStrLn "Entering main loop."
          mainLoop window
          putStrLn "Main loop ended, cleaning up."
  )
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

    maybeWithDebugCallback :: VkInstance -> IO a -> IO a
#ifndef NDEBUG
    maybeWithDebugCallback vkInstance action =
      withFunPtr (newVkDebugReportCallbackEXT debugCallback) $ \debugCallbackPtr ->
        withVkDebugReportCallbackEXT vkInstance debugReportFlags debugCallbackPtr $ \vkDebugReportCallback -> do
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
    maybeWithDebugCallback = id
#endif

    isDeviceSuitable :: VkPhysicalDevice -> IO Bool
    isDeviceSuitable device =
      any isQueueFamilySuitable <$> listPhysicalDeviceQueueFamilyProperties device

      where
        isQueueFamilySuitable :: VkQueueFamilyProperties -> Bool
        isQueueFamilySuitable qf =
          getField @"queueCount" qf > 0 &&
          getField @"queueFlags" qf .&. VK_QUEUE_GRAPHICS_BIT /= zeroBits

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

throwAppEx :: String -> IO a
throwAppEx message = throwIO $ ApplicationException message

withGLFW :: IO a -> IO a
withGLFW = bracket_ GLFW.init GLFW.terminate

withVulkanGLFWWindow :: Int -> Int -> String -> (GLFW.Window -> IO a) -> IO a
withVulkanGLFWWindow width height title = bracket create GLFW.destroyWindow
  where
    create = do
      GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
      GLFW.windowHint $ WindowHint'Resizable False
      GLFW.createWindow width height title Nothing Nothing >>=
        maybe (throwAppEx "Failed to initialize the GLFW window.") return

getGLFWRequiredInstanceExtensions :: IO [CString]
getGLFWRequiredInstanceExtensions = do
  (count, glfwExtensionsArray) <- GLFW.getRequiredInstanceExtensions
  peekArray (fromIntegral count) glfwExtensionsArray

getFirstSuitableDevice :: (VkPhysicalDevice -> IO Bool) -> VkInstance -> IO VkPhysicalDevice
getFirstSuitableDevice isDeviceSuitable vkInstance =
  listPhysicalDevices vkInstance >>=
  firstM isDeviceSuitable >>=
  maybe (throwAppEx "Failed to find a suitable device.") return

withVkInstance :: VkApplicationInfo -> [String] -> [CString] -> (VkInstance -> IO a) -> IO a
withVkInstance applicationInfo validationLayers extensions = bracket create destroy
  where
    createInfo =
      createVk @VkInstanceCreateInfo $
      set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO &*
      setVkRef @"pApplicationInfo" applicationInfo &*
      set @"enabledExtensionCount" (fromIntegral $ length extensions) &*
      setListRef @"ppEnabledExtensionNames" extensions &*
      set @"enabledLayerCount" (fromIntegral $ length validationLayers) &*
      setStrListRef @"ppEnabledLayerNames" validationLayers &*
      set @"pNext" VK_NULL
    create =
      withPtr createInfo $ \createInfoPtr ->
        alloca $ \vkInstancePtr -> do
          onVkFailureThrow "vkCreateInstance failed." $
            vkCreateInstance createInfoPtr VK_NULL vkInstancePtr
          peek vkInstancePtr
    destroy vkInstance = vkDestroyInstance vkInstance VK_NULL

withFunPtr :: IO (FunPtr f) -> (FunPtr f -> IO a) -> IO a
withFunPtr createFunPtr = bracket createFunPtr freeHaskellFunPtr

withVkDebugReportCallbackEXT :: VkInstance -> VkDebugReportFlagsEXT -> PFN_vkDebugReportCallbackEXT -> (VkDebugReportCallbackEXT -> IO a) -> IO a
withVkDebugReportCallbackEXT vkInstance debugReportFlags debugCallbackPtr = bracket create destroy
  where
    createInfo =
      createVk @VkDebugReportCallbackCreateInfoEXT $
      set @"sType" VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT &*
      set @"flags" debugReportFlags &*
      set @"pfnCallback" debugCallbackPtr &*
      set @"pNext" VK_NULL
    create = do
      createDebugReportCallbackEXT <- vkGetInstanceProc @VkCreateDebugReportCallbackEXT vkInstance
      withPtr createInfo $ \createInfoPtr ->
        alloca $ \vkDebugReportCallbackEXTPtr -> do
          onVkFailureThrow "vkCreateDebugReportCallbackEXT failed." $
            createDebugReportCallbackEXT vkInstance createInfoPtr VK_NULL vkDebugReportCallbackEXTPtr
          peek vkDebugReportCallbackEXTPtr
    destroy vkDebugReportCallbackEXT = do
      destroyDebugReportCallbackEXT <- vkGetInstanceProc @VkDestroyDebugReportCallbackEXT vkInstance
      destroyDebugReportCallbackEXT vkInstance vkDebugReportCallbackEXT VK_NULL

ensureValidationLayersSupported :: [String] -> IO ()
ensureValidationLayersSupported validationLayers = do
  unsupportedLayers <- getUnsupportedValidationLayerNames validationLayers
  unless (null unsupportedLayers) $ throwAppEx ("Expected validation layers are not available: " ++ show unsupportedLayers)

getUnsupportedValidationLayerNames :: [String] -> IO [String]
getUnsupportedValidationLayerNames [] = return []
getUnsupportedValidationLayerNames expectedLayerNames =
  listInstanceLayerProperties
  <&> fmap (getStringField @"layerName")
  <&> \case
    [] -> expectedLayerNames
    availableLayerNames -> filter (not . elemOf availableLayerNames) expectedLayerNames
  where
    (<&>) = flip (<$>)
    infixl 1 <&>
    elemOf = flip elem

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
    onVkFailureThrow "vkEnumerateInstanceLayerProperties failed." $
    vkEnumerateInstanceLayerProperties layerCountPtr layersPtr

listPhysicalDevices :: VkInstance -> IO [VkPhysicalDevice]
listPhysicalDevices vkInstance =
  getVkList $ \deviceCountPtr devicesPtr ->
    onVkFailureThrow "vkEnumeratePhysicalDevices failed." $
    vkEnumeratePhysicalDevices vkInstance deviceCountPtr devicesPtr

listPhysicalDeviceQueueFamilyProperties :: VkPhysicalDevice -> IO [VkQueueFamilyProperties]
listPhysicalDeviceQueueFamilyProperties device =
  getVkList $ vkGetPhysicalDeviceQueueFamilyProperties device

mainLoop :: GLFW.Window -> IO ()
mainLoop window = whileM_ (not <$> GLFW.windowShouldClose window) GLFW.pollEvents
