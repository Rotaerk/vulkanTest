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
import Data.Bits
import Data.Foldable
import Data.Function
import Data.Proxy
import Data.Reflection
import Data.Tuple.Extra
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

import qualified Numeric.DataFrame as DF

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

  (physicalDevice, properties, memoryProperties) <-
    liftIO $
    fmap (
      maximumBy . mconcat $ [
        prefer [VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU, VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU] `on` getField @"deviceType" . snd3,
        compare `on` pdmpDeviceLocalMemorySize . thd3
      ]
    ) $
    mapElemsM (\physicalDevice ->
      liftM2 (physicalDevice,,)
        (getVk . vkGetPhysicalDeviceProperties $ physicalDevice)
        (getVk . vkGetPhysicalDeviceMemoryProperties $ physicalDevice)
    ) =<<
    getVkArray (vktEnumeratePhysicalDevices vulkanInstance)

  liftIO $ putStrLn $ getStringField @"deviceName" properties

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

type VulkanGetter vk = Ptr vk -> IO ()

getVk :: (MonadIO io, Storable vk) => VulkanGetter vk -> io vk
getVk get =
  liftIO $
  alloca $ \ptr -> do
    get ptr
    peek ptr

type VulkanArrayFiller vk r = Ptr Word32 -> Ptr vk -> IO r

getVkArrayWithResult :: (MonadIO io, Storable vk) => VulkanArrayFiller vk r -> io (r, StorableArray Word32 vk)
getVkArrayWithResult fillArray =
  liftIO $
  alloca $ \countPtr -> do
    let fillArray' ptr = fillArray countPtr ptr
    getCountResult <- fillArray' VK_NULL
    count <- peek countPtr
    array <- newArray_ (0, count-1)
    if count > 0 then do
      fillArrayResult <- withStorableArray array fillArray'
      return (fillArrayResult, array)
    else
      return (getCountResult, array)

getVkArray :: (MonadIO io, Storable vk) => VulkanArrayFiller vk r -> io (StorableArray Word32 vk)
getVkArray = fmap snd . getVkArrayWithResult

-- When a VulkanArrayFiller is used with getVkArray[WithResult], VK_INCOMPLETE should never be returned, since
-- getVkArray[WithResult] is checking for available count first. Thus, don't provide it as a success result.
onArrayFillerFailureThrow :: String -> [VkResult] -> VulkanArrayFiller vk VkResult -> VulkanArrayFiller vk VkResult
onArrayFillerFailureThrow functionName successResults fillArray countPtr arrayPtr = fillArray countPtr arrayPtr & onVkFailureThrow functionName successResults

vktEnumeratePhysicalDevices :: VkInstance -> VulkanArrayFiller VkPhysicalDevice VkResult
vktEnumeratePhysicalDevices = onArrayFillerFailureThrow "vkEnumeratePhysicalDevices" [VK_SUCCESS] . vkEnumeratePhysicalDevices

pdmpDeviceLocalMemorySize :: VkPhysicalDeviceMemoryProperties -> VkDeviceSize
pdmpDeviceLocalMemorySize memoryProperties = sum . fmap (getField @"size") . filter isDeviceLocal $ memoryHeaps
  where
    isDeviceLocal = (0 /=) . (VK_MEMORY_HEAP_DEVICE_LOCAL_BIT .&.) . getField @"flags"
    memoryHeapCount = getField @"memoryHeapCount" memoryProperties
    memoryHeaps = take (fromIntegral memoryHeapCount) . fmap DF.unScalar . DF.toList . getVec @"memoryHeaps" $ memoryProperties
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

reflection :: forall t r. Reifies t r => r
reflection = reflect (Proxy :: Proxy t)

mapAssocsM :: (Monad m, MArray a e m, Ix i) => (i -> e -> m b) -> a i e -> m [b]
mapAssocsM f array = do
  bounds <- getBounds array
  let
    index' = index bounds
    unsafeRead' = unsafeRead array
  forM (range bounds) $ \i ->
    unsafeRead' (index' i) >>= f i

forAssocsM :: (Monad m, MArray a e m, Ix i) => a i e -> (i -> e -> m b) -> m [b]
forAssocsM = flip mapAssocsM

mapElemsM :: (Monad m, MArray a e m, Ix i) => (e -> m b) -> a i e -> m [b]
mapElemsM f = mapAssocsM (const f)

forElemsM :: (Monad m, MArray a e m, Ix i) => a i e -> (e -> m b) -> m [b]
forElemsM = flip mapElemsM

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

foldlAssocsM :: forall a i e m b. (Monad m, MArray a e m, Ix i) => (b -> i -> e -> m b) -> b -> a i e -> m b
foldlAssocsM f z array = do
  bounds <- getBounds array
  let
    index' = index bounds
    unsafeRead' = unsafeRead array
    step b i = do
      e <- unsafeRead' (index' i)
      f b i e
  foldlM step z (range bounds)

foldlElemsM :: forall a i e m b. (Monad m, MArray a e m, Ix i) => (b -> e -> m b) -> b -> a i e -> m b
foldlElemsM f = foldlAssocsM (\b _ e -> f b e)

foldrAssocsM :: forall a i e m b. (Monad m, MArray a e m, Ix i) => (i -> e -> b -> m b) -> b -> a i e -> m b
foldrAssocsM f z array = do
  bounds <- getBounds array
  let
    index' = index bounds
    unsafeRead' = unsafeRead array
    step i b = do
      e <- unsafeRead' (index' i)
      f i e b
  foldrM step z (range bounds)

foldrElemsM :: forall a i e m b. (Monad m, MArray a e m, Ix i) => (e -> b -> m b) -> b -> a i e -> m b
foldrElemsM f = foldrAssocsM (\_ e b -> f e b)

prefer :: Eq a => [a] -> a -> a -> Ordering
prefer [] _ _ = EQ
prefer (x:xs) a b
  | a == x = if a == b then EQ else GT
  | b == x = LT
  | otherwise = prefer xs a b
-- Other helpers<
