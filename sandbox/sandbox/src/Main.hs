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
import Data.Bool
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List
import Data.Proxy
import Data.Reflection
import qualified Data.Set as Set
import Data.Tuple.Extra
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_EXT_debug_report
import Graphics.Vulkan.Ext.VK_KHR_surface
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
    \(e :: VkaException) ->
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

  allocateAcquire_ initializedGLFW
  ioPutStrLn "GLFW initialized."

  glfwExtensions <- liftIO GLFW.getRequiredInstanceExtensions

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

  (physicalDevice, physicalDeviceProperties, physicalDeviceMemoryProperties) <-
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
    getVkArray (vkaEnumeratePhysicalDevices vulkanInstance)
  ioPutStrLn "Physical device selected."

  window <- allocateAcquire_ $ newVulkanGLFWWindow 800 600 "Vulkan Sandbox"
  ioPutStrLn "Window created."

  windowSurface <- allocateAcquire_ $ newVulkanGLFWWindowSurface vulkanInstance window
  ioPutStrLn "Window surface created."

  physicalDeviceQueueFamilyPropertiesArray <- getVkArray (vkGetPhysicalDeviceQueueFamilyProperties physicalDevice)

  qfis@[graphicsQfi, computeQfi, transferQfi, presentQfi] <-
    liftIO $
    bestQueueFamilyIndexComboIn physicalDeviceQueueFamilyPropertiesArray
      [
        (
          return . (zeroBits /=) . (VK_QUEUE_GRAPHICS_BIT .&.) . getField @"queueFlags" . snd,
          preferWhereM [
            return . (zeroBits ==) . (VK_QUEUE_COMPUTE_BIT .&.) . getField @"queueFlags" . snd
          ]
        ),
        (
          return . (zeroBits /=) . (VK_QUEUE_COMPUTE_BIT .&.) . getField @"queueFlags" . snd,
          preferWhereM [
            return . (zeroBits ==) . (VK_QUEUE_GRAPHICS_BIT .&.) . getField @"queueFlags" . snd
          ]
        ),
        (
          return . (zeroBits /=) . ((VK_QUEUE_GRAPHICS_BIT .|. VK_QUEUE_COMPUTE_BIT .|. VK_QUEUE_TRANSFER_BIT) .&.) . getField @"queueFlags" . snd,
          preferWhereM [
            return . (zeroBits ==) . ((VK_QUEUE_GRAPHICS_BIT .|. VK_QUEUE_COMPUTE_BIT) .&.) . getField @"queueFlags" . snd
          ]
        ),
        (
          (\(qfi, _) -> (VK_TRUE ==) <$> getVk (vkaGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi windowSurface)),
          \_ _ -> return EQ
        )
      ]
  ioPutStrLn "Queue family indices selected."

  let distinctQfis = distinct qfis

  -- Next up: create the logical device

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

newVulkanGLFWWindowSurface :: VkInstance -> GLFW.Window -> Acquire VkSurfaceKHR
newVulkanGLFWWindowSurface vulkanInstance window =
  (
    alloca $ \surfacePtr -> do
      void $ GLFW.createWindowSurface vulkanInstance window nullPtr surfacePtr & onVkFailureThrow "GLFW.createWindowSurface" [VK_SUCCESS]
      peek surfacePtr
  )
  `mkAcquire`
  \surface -> vkDestroySurfaceKHR vulkanInstance surface VK_NULL
-- GLFW helpers<

-- Vulkan helpers>
data VkaException =
  VkaException {
    vkexFunctionName :: String,
    vkexResult :: VkResult
  } deriving (Eq, Show, Read)

instance Exception VkaException where
  displayException (VkaException functionName result) =
    functionName ++ " failed with: " ++ show result

onVkFailureThrow :: String -> [VkResult] -> IO VkResult -> IO VkResult
onVkFailureThrow functionName successResults vkAction = do
  result <- vkAction
  unless (result `elem` successResults) $ throwIO (VkaException functionName result)
  return result

vkaRegisterDebugCallback :: MonadUnliftIO io => VkInstance -> VkDebugReportFlagsEXT -> HS_vkDebugReportCallbackEXT -> ResourceT io ()
vkaRegisterDebugCallback vulkanInstance flags debugCallback = do
  debugCallbackPtr <- allocateAcquire_ . newFunPtrFrom . newVkDebugReportCallbackEXT $ debugCallback
  void . allocateAcquire_ . newVk (registeredDebugReportCallbackResource vulkanInstance) $
    createVk $
    set @"sType" VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT &*
    set @"pNext" VK_NULL &*
    set @"flags" flags &*
    set @"pfnCallback" debugCallbackPtr

data VkaResource vk ci =
  VkaResource {
    vkrGetCreate :: IO (Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult),
    vkrGetDestroy :: IO (vk -> Ptr VkAllocationCallbacks -> IO ()),
    vkrCreateName :: String,
    vkrSuccessResults :: [VkResult]
  }

newVkWithResult :: (Storable vk, VulkanMarshal ci) => VkaResource vk ci -> ci -> Acquire (VkResult, vk)
newVkWithResult (VkaResource getCreate getDestroy functionName successResults) createInfo =
  liftIO (liftM2 (,) getCreate getDestroy) >>= \(create, destroy) ->
    (
      withPtr createInfo $ \createInfoPtr ->
      alloca $ \vkPtr -> do
        result <- create createInfoPtr VK_NULL vkPtr & onVkFailureThrow functionName successResults
        (result,) <$> peek vkPtr
    )
    `mkAcquire`
    \(_, vk) -> destroy vk VK_NULL

newVk :: (Storable vk, VulkanMarshal ci) => VkaResource vk ci -> ci -> Acquire vk
newVk vr ci = snd <$> newVkWithResult vr ci

vulkanInstanceResource :: VkaResource VkInstance VkInstanceCreateInfo
vulkanInstanceResource = VkaResource (return vkCreateInstance) (return vkDestroyInstance) "vkCreateInstance" [VK_SUCCESS]

registeredDebugReportCallbackResource :: VkInstance -> VkaResource VkDebugReportCallbackEXT VkDebugReportCallbackCreateInfoEXT
registeredDebugReportCallbackResource vulkanInstance =
  VkaResource
    (vkGetInstanceProc @VkCreateDebugReportCallbackEXT vulkanInstance <*> pure vulkanInstance)
    (vkGetInstanceProc @VkDestroyDebugReportCallbackEXT vulkanInstance <*> pure vulkanInstance)
    "vkCreateDebugReportCallbackEXT"
    [VK_SUCCESS]

type VkaGetter vk r = Ptr vk -> IO r

getVkWithResult :: (MonadIO io, Storable vk) => VkaGetter vk r -> io (r, vk)
getVkWithResult get =
  liftIO $
  alloca $ \ptr -> do
    result <- get ptr
    value <- peek ptr
    return (result, value)

getVk :: (MonadIO io, Storable vk) => VkaGetter vk r -> io vk
getVk = fmap snd . getVkWithResult

onGetterFailureThrow :: String -> [VkResult] -> VkaGetter vk VkResult -> VkaGetter vk VkResult
onGetterFailureThrow functionName successResults get ptr = get ptr & onVkFailureThrow functionName successResults

vkaGetPhysicalDeviceSurfaceSupportKHR :: VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> VkaGetter VkBool32 VkResult
vkaGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi surface =
  vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi surface &
    onGetterFailureThrow "vkGetPhysicalDeviceSurfaceSupportKHR" [VK_SUCCESS]

type VkaArrayFiller vk r = Ptr Word32 -> Ptr vk -> IO r

getVkArrayWithResult :: (MonadIO io, Storable vk) => VkaArrayFiller vk r -> io (r, StorableArray Word32 vk)
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

getVkArray :: (MonadIO io, Storable vk) => VkaArrayFiller vk r -> io (StorableArray Word32 vk)
getVkArray = fmap snd . getVkArrayWithResult

-- When a VkaArrayFiller is used with getVkArray[WithResult], VK_INCOMPLETE should never be returned, since
-- getVkArray[WithResult] is checking for available count first. Thus, don't provide it as a success result.
onArrayFillerFailureThrow :: String -> [VkResult] -> VkaArrayFiller vk VkResult -> VkaArrayFiller vk VkResult
onArrayFillerFailureThrow functionName successResults fillArray countPtr arrayPtr = fillArray countPtr arrayPtr & onVkFailureThrow functionName successResults

vkaEnumeratePhysicalDevices :: VkInstance -> VkaArrayFiller VkPhysicalDevice VkResult
vkaEnumeratePhysicalDevices = onArrayFillerFailureThrow "vkEnumeratePhysicalDevices" [VK_SUCCESS] . vkEnumeratePhysicalDevices

pdmpDeviceLocalMemorySize :: VkPhysicalDeviceMemoryProperties -> VkDeviceSize
pdmpDeviceLocalMemorySize memoryProperties = sum . fmap (getField @"size") . filter isDeviceLocal $ memoryHeaps
  where
    isDeviceLocal = (0 /=) . (VK_MEMORY_HEAP_DEVICE_LOCAL_BIT .&.) . getField @"flags"
    memoryHeapCount = getField @"memoryHeapCount" memoryProperties
    memoryHeaps = take (fromIntegral memoryHeapCount) . fmap DF.unScalar . DF.toList . getVec @"memoryHeaps" $ memoryProperties

comboWithFewestDistinct :: Eq a => [[a]] -> [a]
comboWithFewestDistinct = minimumBy (compare `on` length . nub) . sequence

type QF i = (i, VkQueueFamilyProperties)
type QFIPredicateM i = PredicateM IO (QF i)
type QFICompareM i = CompareM IO (QF i)

bestQueueFamilyIndices :: forall i. Ix i => QFIPredicateM i -> QFICompareM i -> StorableArray i VkQueueFamilyProperties -> IO [i]
bestQueueFamilyIndices isCandidate compareQfps = (fmap . fmap) fst . foldrAssocsM step []
  where
    step :: i -> VkQueueFamilyProperties -> [QF i] -> IO [QF i]
    step qfi qfp best@(bqf:_) = do
      let qf = (qfi,qfp)
      c <- isCandidate qf
      if c then
        compareQfps qf bqf <&> \case
          LT -> best
          EQ -> qf:best
          GT -> [qf]
      else
        return best
    step qfi qfp [] = let qf = (qfi,qfp) in bool [] [qf] <$> isCandidate qf

bestQueueFamilyIndicesIn :: forall i. Ix i => StorableArray i VkQueueFamilyProperties -> QFIPredicateM i -> QFICompareM i -> IO [i]
bestQueueFamilyIndicesIn a p c = bestQueueFamilyIndices p c a

bestQueueFamilyIndexCombo :: Ix i => [(QFIPredicateM i, QFICompareM i)] -> StorableArray i VkQueueFamilyProperties -> IO [i]
bestQueueFamilyIndexCombo criteriaList = fmap comboWithFewestDistinct . forM criteriaList . uncurry . bestQueueFamilyIndicesIn

bestQueueFamilyIndexComboIn :: Ix i => StorableArray i VkQueueFamilyProperties -> [(QFIPredicateM i, QFICompareM i)] -> IO [i]
bestQueueFamilyIndexComboIn a cs = bestQueueFamilyIndexCombo cs a
-- Vulkan helpers<

-- ResourceT helpers>
allocate_ :: MonadResource m => IO a -> (a -> IO ()) -> m a
allocate_ acquire free = snd <$> allocate acquire free

allocateAcquire_ :: MonadResource m => Acquire a -> m a
allocateAcquire_ = (snd <$>) . allocateAcquire
-- ResourceT helpers<

-- Other helpers>
newFunPtrFrom :: IO (FunPtr f) -> Acquire (FunPtr f)
newFunPtrFrom = flip mkAcquire freeHaskellFunPtr

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

distinct :: Ord a => [a] -> [a]
distinct = Set.toList . Set.fromList

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

type Predicate a = a -> Bool
type PredicateM m a = a -> m Bool
type Compare a = a -> a -> Ordering
type CompareM m a = a -> a -> m Ordering

preferWhere :: [Predicate a] -> Compare a
preferWhere [] _ _ = EQ
preferWhere (p:ps) a b
  | p a = if p b then EQ else GT
  | p b = LT
  | otherwise = preferWhere ps a b

prefer :: Eq a => [a] -> Compare a
prefer = preferWhere . fmap (==)

preferWhereM :: Monad m => [PredicateM m a] -> CompareM m a
preferWhereM [] _ _ = return EQ
preferWhereM (p:ps) a b = do
  pa <- p a
  pb <- p b
  if pa then
    return $ if pb then EQ else GT
  else if pb then
    return LT
  else
    preferWhereM ps a b

ioPutStrLn :: MonadIO io => String -> io ()
ioPutStrLn = liftIO . putStrLn
-- Other helpers<
