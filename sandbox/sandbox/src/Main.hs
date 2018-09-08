{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Main where
  
import Control.Exception
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Acquire
import Data.Array.Base
import Data.Array.MArray
import Data.Array.Storable
import Data.Function
import Data.Proxy
import Data.Reflection
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_EXT_debug_report
--import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame

main :: IO ()
main =
  runResourceT resourceMain
  `catch` (
    \(e :: ApplicationException) ->
      putStrLn $ displayException e
  )
  `catch` (
    \(e :: VulkanException) ->
      putStrLn $ displayException e
  )
  `catch` (
    \(e :: GLFWException) ->
      putStrLn $ displayException e
  )

resourceMain :: ResourceT IO ()
resourceMain = do
  allocateAcquire_ initializedGLFW

  --window <- allocateAcquire_ $ newVulkanGLFWWindow 800 600 "Vulkan Sandbox"

  glfwExtensions <- liftIO $ GLFW.getRequiredInstanceExtensions

  let allExtensions = glfwExtensions ++ extensions

  vulkanInstance <-
    allocateAcquire_ . newVk vulkanInstanceResource . createVk $
    set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO &*
    set @"pNext" VK_NULL &*
    setVkRef @"pApplicationInfo" (
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO &*
      set @"pNext" VK_NULL &*
      setStrRef @"pApplicationName" "Vulkan Sandbox" &*
      set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0) &*
      set @"pEngineName" VK_NULL &*
      set @"engineVersion" 0 &*
      set @"apiVersion" VK_API_VERSION_1_0
    ) &*
    set @"enabledLayerCount" (lengthNum validationLayers) &*
    setStrListRef @"ppEnabledLayerNames" validationLayers &*
    set @"enabledExtensionCount" (lengthNum allExtensions) &*
    setListRef @"ppEnabledExtensionNames" allExtensions

  physicalDevices <- getVkArray (physicalDevicesOfInstance vulkanInstance)

  liftIO $ forAssocsM_ physicalDevices $ \index physicalDevice -> do
    properties <- getVk (propertiesOfPhysicalDevice physicalDevice)
    putStrLn $ "Device #" ++ show index ++ ":"
    let
      apiVersion = getField @"apiVersion" properties
      driverVersion = getField @"driverVersion" properties
      vendorId = getField @"vendorID" properties
      deviceId = getField @"deviceID" properties
      deviceType = getField @"deviceType" properties
      deviceName = getStringField @"deviceName" properties
      pipelineCacheUUID = getVec @"pipelineCacheUUID" properties
      limits = getField @"limits" properties
      sparseProperties = getField @"sparseProperties" properties
    putStrLn $ "API Version: " ++ show (_VK_VERSION_MAJOR apiVersion) ++ "." ++ show (_VK_VERSION_MINOR apiVersion) ++ "." ++ show (_VK_VERSION_PATCH apiVersion)
    putStrLn $ "Driver Version: " ++ show driverVersion
    putStrLn $ "Vendor ID: " ++ show vendorId
    putStrLn $ "Device ID: " ++ show deviceId
    putStrLn $ "Device Type: " ++ show deviceType
    putStrLn $ "Device Name: " ++ deviceName
    putStrLn $ "Pipeline Cache UUID: " ++ show pipelineCacheUUID
    putStrLn $ "Limits: " ++ show limits
    putStrLn $ "Sparse Properties: " ++ show sparseProperties
    putStrLn ""

  return ()

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

data ApplicationException = ApplicationException String deriving (Eq, Show, Read)

instance Exception ApplicationException where
  displayException (ApplicationException message) =
    "Application error: " ++ message

throwAppEx :: String -> a
throwAppEx message = throw $ ApplicationException message

throwIOAppEx :: MonadIO io => String -> io a
throwIOAppEx message = liftIO . throwIO $ ApplicationException message

-- GLFW helpers>
data GLFWException =
  GLFWException {
    glfwexFunctionName :: String
  } deriving (Eq, Show, Read)

instance Exception GLFWException where
  displayException (GLFWException functionName) =
    functionName ++ " failed."

initializedGLFW :: Acquire ()
initializedGLFW =
  unlessM GLFW.init (throwIO $ GLFWException "init")
  `mkAcquire`
  const GLFW.terminate

newVulkanGLFWWindow :: Int -> Int -> String -> Acquire GLFW.Window
newVulkanGLFWWindow width height title =
  do
    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
    whenNothingM (GLFW.createWindow width height title Nothing Nothing) (throwIO $ GLFWException "createWindow")
  `mkAcquire`
  GLFW.destroyWindow
-- GLFW helpers<

-- Vulkan helpers>
data VulkanException =
  VulkanException {
    vkexFunctionName :: String,
    vkexResult :: VkResult
  } deriving (Eq, Show, Read)

instance Exception VulkanException where
  displayException (VulkanException functionName result) =
    functionName ++ " failed with: " ++ show result

onVkFailureThrow :: String -> [VkResult] -> IO VkResult -> IO VkResult
onVkFailureThrow functionName successResults vkAction = do
  result <- vkAction
  unless (result `elem` successResults) $ throwIO (VulkanException functionName result)
  return result

data VulkanResource vk ci =
  VulkanResource {
    vkrCreate :: Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult,
    vkrDestroy :: vk -> Ptr VkAllocationCallbacks -> IO (),
    vkrCreateName :: String,
    vkrSuccessResults :: [VkResult]
  }

newVkWithResult :: (Storable vk, VulkanMarshal ci) => VulkanResource vk ci -> ci -> Acquire (VkResult, vk)
newVkWithResult (VulkanResource create destroy functionName successResults) createInfo =
  (
    withPtr createInfo $ \createInfoPtr ->
    alloca $ \vkPtr -> do
      result <- create createInfoPtr VK_NULL vkPtr & onVkFailureThrow functionName successResults
      (result,) <$> peek vkPtr
  )
  `mkAcquire`
  \(_, vk) -> destroy vk VK_NULL

newVk :: (Storable vk, VulkanMarshal ci) => VulkanResource vk ci -> ci -> Acquire vk
newVk vr ci = snd <$> newVkWithResult vr ci

vulkanInstanceResource :: VulkanResource VkInstance VkInstanceCreateInfo
vulkanInstanceResource = VulkanResource vkCreateInstance vkDestroyInstance "vkCreateInstance" [VK_SUCCESS]

data VulkanGetter vk =
  VulkanGetter {
    vkgGet :: Ptr vk -> IO ()
  }

getVk :: (MonadIO io, Storable vk) => VulkanGetter vk -> io vk
getVk (VulkanGetter get) =
  liftIO $
  alloca $ \ptr -> do
    get ptr
    peek ptr

featuresOfPhysicalDevice :: VkPhysicalDevice -> VulkanGetter VkPhysicalDeviceFeatures
featuresOfPhysicalDevice physicalDevice = VulkanGetter (vkGetPhysicalDeviceFeatures physicalDevice)

propertiesOfPhysicalDevice :: VkPhysicalDevice -> VulkanGetter VkPhysicalDeviceProperties
propertiesOfPhysicalDevice physicalDevice = VulkanGetter (vkGetPhysicalDeviceProperties physicalDevice)

data VulkanArrayFiller vk =
  VulkanArrayFiller {
    vkafFillArray :: Ptr Word32 -> Ptr vk -> IO VkResult,
    vkafName :: String,
    vkafSuccessResults :: [VkResult]
  }

-- VK_INCOMPLETE should never be returned, since we're checking for available count first.
-- Thus, don't treat it as a success result.
getVkArrayWithResult :: (MonadIO io, Storable vk) => VulkanArrayFiller vk -> io (VkResult, StorableArray Word32 vk)
getVkArrayWithResult (VulkanArrayFiller fillArray functionName successResults) =
  liftIO $
  alloca $ \countPtr -> do
    let fillArray' ptr = fillArray countPtr ptr & onVkFailureThrow functionName successResults
    getCountResult <- fillArray' VK_NULL
    count <- peek countPtr
    array <- newArray_ (0, count-1)
    if count > 0 then do
      fillArrayResult <- withStorableArray array fillArray'
      return (fillArrayResult, array)
    else do
      return (getCountResult, array)

getVkArray :: (MonadIO io, Storable vk) => VulkanArrayFiller vk -> io (StorableArray Word32 vk)
getVkArray = fmap snd . getVkArrayWithResult

physicalDevicesOfInstance :: VkInstance -> VulkanArrayFiller VkPhysicalDevice
physicalDevicesOfInstance inst = VulkanArrayFiller (vkEnumeratePhysicalDevices inst) "vkEnumeratePhysicalDevices" [VK_SUCCESS]

-- ResourceT helpers>
allocate_ :: MonadResource m => IO a -> (a -> IO ()) -> m a
allocate_ acquire free = snd <$> allocate acquire free

allocateAcquire_ :: MonadResource m => Acquire a -> m a
allocateAcquire_ = (snd <$>) . allocateAcquire
-- ResourceT helpers<

-- Other helpers>
whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing (Just x) _ = pure x
whenNothing Nothing m = m
{-# INLINE whenNothing #-}

whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM mm a = mm >>= \m -> whenNothing m a
{-# INLINE whenNothingM #-}

lengthNum :: (Foldable t, Num n) => t a -> n
lengthNum = fromIntegral . length
{-# INLINE lengthNum #-}

reflection :: forall t r. Reifies t r => r
reflection = reflect (Proxy :: Proxy t)

mapAssocsM_ :: (Monad m, MArray a e m, Ix i) => (i -> e -> m b) -> a i e -> m ()
mapAssocsM_ f array = do
  bounds <- getBounds array
  let
    index' = index bounds
    unsafeRead' = unsafeRead array
  forM_ (range bounds) $ \i ->
    unsafeRead' (index' i) >>= f i

forAssocsM_ :: (Monad m, MArray a e m, Ix i) => a i e -> (i -> e -> m b) -> m ()
forAssocsM_ = flip mapAssocsM_

mapElemsM_ :: (Monad m, MArray a e m, Ix i) => (e -> m b) -> a i e -> m ()
mapElemsM_ f = mapAssocsM_ (const f)

forElemsM_ :: (Monad m, MArray a e m, Ix i) => a i e -> (e -> m b) -> m ()
forElemsM_ = flip mapElemsM_
-- Other helpers<
