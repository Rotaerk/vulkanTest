{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
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
import Data.Function
import Data.Maybe
--import Data.Reflection
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_EXT_debug_report
--import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
--import Graphics.Vulkan.Marshal.Create.DataFrame

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

  window <- allocateAcquire_ $ newVulkanGLFWWindow 800 600 "Vulkan Sandbox"

  glfwExtensions <- liftIO $ GLFW.getRequiredInstanceExtensions

  let allExtensions = glfwExtensions ++ extensions

  vulkanInstance <-
    allocateAcquire_ $
    newVkInstanceFrom_ $
    createVk $
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

newVkWithResult ::
  (Storable vk, VulkanMarshal ci) =>
  String ->
  (Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult) ->
  (vk -> Ptr VkAllocationCallbacks -> IO()) ->
  [VkResult] ->
  ci ->
  Acquire (VkResult, vk)
newVkWithResult functionName create destroy successResults createInfo =
  (
    withPtr createInfo $ \createInfoPtr ->
    alloca $ \vkPtr -> do
      result <- create createInfoPtr VK_NULL vkPtr & onVkFailureThrow functionName successResults
      (result,) <$> peek vkPtr
  )
  `mkAcquire`
  \(_, vk) -> destroy vk VK_NULL

newVkInstanceFrom :: VkInstanceCreateInfo -> Acquire (VkResult, VkInstance)
newVkInstanceFrom = newVkWithResult "vkCreateInstance" vkCreateInstance vkDestroyInstance [VK_SUCCESS]

newVkInstanceFrom_ :: VkInstanceCreateInfo -> Acquire VkInstance
newVkInstanceFrom_ = fmap snd . newVkInstanceFrom
-- Vulkan helpers<

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

-- Other helpers<
