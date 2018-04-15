{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (init)
import Control.Exception
import Control.Monad
import Control.Monad.Extra (firstJustM)
import Control.Monad.Loops (whileM_)
import Data.Bits
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List
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

      glfwExtensions <- GLFW.getRequiredInstanceExtensions

      unless (null validationLayers) $ do
        ensureValidationLayersSupported validationLayers
        putStrLn "All required validation layers supported."

      withVkInstance applicationInfo validationLayers (extensions ++ glfwExtensions) $ \vkInstance -> do
        putStrLn "Instance created."

        maybeWithDebugCallback vkInstance $ do
          (physicalDevice, qfi) <- getFirstSuitablePhysicalDeviceAndQueueFamilyIndices vkInstance
          putStrLn "Found a suitable physical device."
          withVkDevice physicalDevice [qfiGraphicsFamilyIndex qfi] $ \vkDevice -> do
            putStrLn "Vulkan device created."

            graphicsQueue <- getDeviceQueue vkDevice (qfiGraphicsFamilyIndex qfi) 0
            putStrLn "Obtained the graphics queue."

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

    getFirstSuitablePhysicalDeviceAndQueueFamilyIndices :: VkInstance -> IO (VkPhysicalDevice, QueueFamilyIndices)
    getFirstSuitablePhysicalDeviceAndQueueFamilyIndices vkInstance =
      listPhysicalDevices vkInstance >>=
      firstJustM (\device ->
        ((device,) <$>) . findQueueFamilyIndices <$>
        listPhysicalDeviceQueueFamilyProperties device
      ) >>=
      maybe (throwAppEx "Failed to find a suitable physical device.") return

    findQueueFamilyIndices :: [VkQueueFamilyProperties] -> Maybe QueueFamilyIndices
    findQueueFamilyIndices queueFamilies = do
      graphicsFamilyIndex <- findFamilyIndexWhere $ (zeroBits /=) . (VK_QUEUE_GRAPHICS_BIT .&.) . getField @"queueFlags"
      return $ QueueFamilyIndices graphicsFamilyIndex
      where
        indexedFamiliesWithQueues :: [(Word32, VkQueueFamilyProperties)]
        indexedFamiliesWithQueues = filter ((0 <) . getField @"queueCount" . snd) . zip [0 ..] $ queueFamilies

        findFamilyIndexWhere :: (VkQueueFamilyProperties -> Bool) -> Maybe Word32
        findFamilyIndexWhere cond = fst <$> find (cond . snd) indexedFamiliesWithQueues

data QueueFamilyIndices =
  QueueFamilyIndices {
    qfiGraphicsFamilyIndex :: Word32
  } deriving (Eq, Show, Read)

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

withVkInstance :: VkApplicationInfo -> [String] -> [CString] -> (VkInstance -> IO a) -> IO a
withVkInstance applicationInfo validationLayers extensions = bracket create destroy
  where
    create =
      withPtr createInfo $ \createInfoPtr ->
        alloca $ \vkInstancePtr -> do
          onVkFailureThrow "vkCreateInstance failed." $
            vkCreateInstance createInfoPtr VK_NULL vkInstancePtr
          peek vkInstancePtr
    destroy vkInstance = vkDestroyInstance vkInstance VK_NULL
    createInfo =
      createVk @VkInstanceCreateInfo $
      set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO &*
      setVkRef @"pApplicationInfo" applicationInfo &*
      set @"enabledExtensionCount" (fromIntegral $ length extensions) &*
      setListRef @"ppEnabledExtensionNames" extensions &*
      set @"enabledLayerCount" (fromIntegral $ length validationLayers) &*
      setStrListRef @"ppEnabledLayerNames" validationLayers &*
      set @"pNext" VK_NULL

withVkDevice :: VkPhysicalDevice -> [Word32] -> (VkDevice -> IO a) -> IO a
withVkDevice physicalDevice queueFamilyIndices = bracket create destroy
  where
    create =
      withPtr createInfo $ \createInfoPtr ->
        alloca $ \vkDevicePtr -> do
          onVkFailureThrow "vkCreateDevice failed." $
            vkCreateDevice physicalDevice createInfoPtr VK_NULL vkDevicePtr
          peek vkDevicePtr
    destroy vkDevice = vkDestroyDevice vkDevice VK_NULL
    createInfo =
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
      set @"enabledExtensionCount" 0 &*
      set @"ppEnabledExtensionNames" VK_NULL &*
      setVkRef @"pEnabledFeatures" (
        createVk @VkPhysicalDeviceFeatures $ handleRemFields @_ @'[]
      )

getDeviceQueue :: VkDevice -> Word32 -> Word32 -> IO VkQueue
getDeviceQueue vkDevice queueFamilyIndex queueIndex =
  alloca $ \deviceQueuePtr -> do
    vkGetDeviceQueue vkDevice queueFamilyIndex 0 deviceQueuePtr
    peek deviceQueuePtr

withFunPtr :: IO (FunPtr f) -> (FunPtr f -> IO a) -> IO a
withFunPtr createFunPtr = bracket createFunPtr freeHaskellFunPtr

withVkDebugReportCallbackEXT :: VkInstance -> VkDebugReportFlagsEXT -> PFN_vkDebugReportCallbackEXT -> (VkDebugReportCallbackEXT -> IO a) -> IO a
withVkDebugReportCallbackEXT vkInstance debugReportFlags debugCallbackPtr = bracket create destroy
  where
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
    createInfo =
      createVk @VkDebugReportCallbackCreateInfoEXT $
      set @"sType" VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT &*
      set @"flags" debugReportFlags &*
      set @"pfnCallback" debugCallbackPtr &*
      set @"pNext" VK_NULL

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

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 1 <&>

elemOf :: (Foldable t, Eq a) => t a -> a -> Bool
elemOf = flip elem
