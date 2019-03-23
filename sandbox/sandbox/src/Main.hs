{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude.Local

import qualified Codec.Image.Ktx.Read as KTX
import Control.Exception (throw)
import Control.Monad
import Control.Monad.BufferWriter
import Control.Monad.Extra
import Control.Monad.FileReader
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Acquire.Local
import Data.Array.Base
import Data.Array.MArray.Local
import Data.Array.Storable
import Data.Array.Unsafe
import Data.Bits
import Data.Bool
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra
import Data.Word
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import qualified GHC.Base as GHC
import qualified GHC.ForeignPtr as GHC

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_EXT_debug_report
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame
import Graphics.Vulkan.Marshal.Internal

import qualified Numeric.DataFrame as DF

import Pipes
import qualified Pipes.Prelude.Local as P

import Safe.Foldable
import System.IO.Unsafe
import UnliftIO.Exception
import UnliftIO.IO

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
    allocateAcquireVk_ vulkanInstanceResource $
    defineStandardVulkanInstance "Vulkan Sandbox" (_VK_MAKE_VERSION 1 0 0) "" 0 VK_API_VERSION_1_0 validationLayers allExtensions
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

  physicalDevicesArray <- getVkArray (vkaEnumeratePhysicalDevices vulkanInstance)

  (physicalDevice, physicalDeviceProperties, physicalDeviceMemoryProperties) <-
    liftIO $
    getElems physicalDevicesArray >>=
    mapM (\pd ->
      liftM2 (pd,,)
        (getVk . vkGetPhysicalDeviceProperties $ pd)
        (getVk . vkGetPhysicalDeviceMemoryProperties $ pd)
    ) <&>
    sortBy (
      mconcat [
        prefer [VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU, VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU] `on` getField @"deviceType" . snd3,
        compare `on` pdmpDeviceLocalMemorySize . thd3
      ]
    ) <&>
    headOr (throwAppEx "No physical device found")

  ioPutStrLn "Physical device selected."

  window <- allocateAcquire_ $ newVulkanGLFWWindow 800 600 "Vulkan Sandbox"
  ioPutStrLn "Window created."

  windowSurface <- allocateAcquire_ $ newVulkanGLFWWindowSurface vulkanInstance window
  ioPutStrLn "Window surface created."

  physicalDeviceQueueFamilyPropertiesArray <- getVkArray (vkGetPhysicalDeviceQueueFamilyProperties physicalDevice)

  qfis@[graphicsQfi, computeQfi, transferQfi, presentQfi] <-
    liftIO $
    pickQueueFamilyIndexCombo
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
          \(qfi, _) -> (VK_TRUE ==) <$> getVk (vkaGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi windowSurface),
          \_ _ -> return EQ
        )
      ]
      physicalDeviceQueueFamilyPropertiesArray
  ioPutStrLn "Queue family indices selected."

  device <- allocateAcquireVk_ (deviceResource physicalDevice) $ defineDeviceWithOneQueuePerUsedFamily (Set.fromList qfis) deviceExtensions
  ioPutStrLn "Vulkan device created."

  [(graphicsQueue, graphicsCommandPool), (computeQueue, computeCommandPool), (transferQueue, transferCommandPool), (presentQueue, presentCommandPool)] <-
    let
      createCommandPool = allocateAcquireVk_ (commandPoolResource device) . defineStandardCommandPool 0
    in
      forM qfis $ \qfi -> liftM2 (,) (getVk $ vkGetDeviceQueue device qfi 0) (createCommandPool qfi)
  ioPutStrLn "Device queues obtained, and corresponding command pools created."

  descriptorSetLayout <-
    allocateAcquireVk_ (descriptorSetLayoutResource device) . defineStandardDescriptorSetLayout 0 . fmap (createVk @VkDescriptorSetLayoutBinding) $ [
      set @"binding" 0 &*
      set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER &*
      set @"descriptorCount" 1 &*
      set @"stageFlags" VK_SHADER_STAGE_VERTEX_BIT &*
      set @"pImmutableSamplers" VK_NULL,

      set @"binding" 1 &*
      set @"descriptorType" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER &*
      set @"descriptorCount" 1 &*
      set @"stageFlags" VK_SHADER_STAGE_FRAGMENT_BIT &*
      set @"pImmutableSamplers" VK_NULL
    ]
  ioPutStrLn "Descriptor set layout created."

  pipelineLayout <- allocateAcquireVk_ (pipelineLayoutResource device) $ defineStandardPipelineLayout [descriptorSetLayout] []
  ioPutStrLn "Pipeline layout created."

  -- What information I need about a texture:
  -- - width, height, depth
  -- - pixel size
  -- - # mip levels
  -- - format

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

data VkaResource ci vk =
  VkaResource {
    vkrGetCreate :: IO (Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult),
    vkrGetDestroy :: IO (vk -> Ptr VkAllocationCallbacks -> IO ()),
    vkrCreateName :: String,
    vkrSuccessResults :: [VkResult]
  }

newVkWithResult :: (Storable vk, VulkanMarshal ci) => VkaResource ci vk -> ci -> Acquire (VkResult, vk)
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

newVk :: (Storable vk, VulkanMarshal ci) => VkaResource ci vk -> ci -> Acquire vk
newVk vr ci = snd <$> newVkWithResult vr ci

allocateAcquireVk_ :: (Storable vk, VulkanMarshal ci, MonadResource m) => VkaResource ci vk -> ci -> m vk
allocateAcquireVk_ = (allocateAcquire_ .) . newVk

vulkanInstanceResource :: VkaResource VkInstanceCreateInfo VkInstance
vulkanInstanceResource = VkaResource (return vkCreateInstance) (return vkDestroyInstance) "vkCreateInstance" [VK_SUCCESS]

defineStandardVulkanInstance :: String -> Word32 -> String -> Word32 -> Word32 -> [String] -> [CString] -> VkInstanceCreateInfo
defineStandardVulkanInstance applicationName applicationVersion engineName engineVersion apiVersion validationLayers extensions =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  setVkRef @"pApplicationInfo" (
    createVk $
    set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO &*
    set @"pNext" VK_NULL &*
    setStrRef @"pApplicationName" applicationName &*
    set @"applicationVersion" applicationVersion &*
    setStrRef @"pEngineName" engineName &*
    set @"engineVersion" engineVersion &*
    set @"apiVersion" apiVersion
  ) &*
  setStrListCountAndRef @"enabledLayerCount" @"ppEnabledLayerNames" validationLayers &*
  setListCountAndRef @"enabledExtensionCount" @"ppEnabledExtensionNames" extensions

registeredDebugReportCallbackResource :: VkInstance -> VkaResource VkDebugReportCallbackCreateInfoEXT VkDebugReportCallbackEXT
registeredDebugReportCallbackResource vulkanInstance =
  VkaResource
    (vkGetInstanceProc @VkCreateDebugReportCallbackEXT vulkanInstance <*> pure vulkanInstance)
    (vkGetInstanceProc @VkDestroyDebugReportCallbackEXT vulkanInstance <*> pure vulkanInstance)
    "vkCreateDebugReportCallbackEXT"
    [VK_SUCCESS]

deviceResource :: VkPhysicalDevice -> VkaResource VkDeviceCreateInfo VkDevice
deviceResource physicalDevice = VkaResource (return $ vkCreateDevice physicalDevice) (return vkDestroyDevice) "vkCreateDevice" [VK_SUCCESS]

defineDeviceWithOneQueuePerUsedFamily :: Set Word32 -> [CString] -> VkDeviceCreateInfo
defineDeviceWithOneQueuePerUsedFamily queueFamilyIndices deviceExtensions =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" 0 &*
  setListCountAndRef @"queueCreateInfoCount" @"pQueueCreateInfos" (
    Set.toList queueFamilyIndices <&> \qfi ->
      createVk $
      set @"sType" VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO &*
      set @"pNext" VK_NULL &*
      set @"flags" 0 &*
      set @"queueFamilyIndex" qfi &*
      setListCountAndRef @"queueCount" @"pQueuePriorities" [1.0]
  ) &*
  set @"enabledLayerCount" 0 &*
  set @"ppEnabledLayerNames" VK_NULL &*
  setListCountAndRef @"enabledExtensionCount" @"ppEnabledExtensionNames" deviceExtensions &*
  set @"pEnabledFeatures" VK_NULL

commandPoolResource :: VkDevice -> VkaResource VkCommandPoolCreateInfo VkCommandPool
commandPoolResource device = VkaResource (return $ vkCreateCommandPool device) (return $ vkDestroyCommandPool device) "vkCreateCommandPool" [VK_SUCCESS]

defineStandardCommandPool :: VkCommandPoolCreateFlags -> Word32 -> VkCommandPoolCreateInfo
defineStandardCommandPool flags queueFamilyIndex =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" flags &*
  set @"queueFamilyIndex" queueFamilyIndex

descriptorSetLayoutResource :: VkDevice -> VkaResource VkDescriptorSetLayoutCreateInfo VkDescriptorSetLayout
descriptorSetLayoutResource device = VkaResource (return $ vkCreateDescriptorSetLayout device) (return $ vkDestroyDescriptorSetLayout device) "vkCreateDescriptorSetLayout" [VK_SUCCESS]

defineStandardDescriptorSetLayout :: VkDescriptorSetLayoutCreateFlags -> [VkDescriptorSetLayoutBinding] -> VkDescriptorSetLayoutCreateInfo
defineStandardDescriptorSetLayout flags bindings =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" flags &*
  setListCountAndRef @"bindingCount" @"pBindings" bindings

pipelineLayoutResource :: VkDevice -> VkaResource VkPipelineLayoutCreateInfo VkPipelineLayout
pipelineLayoutResource device = VkaResource (return $ vkCreatePipelineLayout device) (return $ vkDestroyPipelineLayout device) "vkCreatePipelineLayout" [VK_SUCCESS]

defineStandardPipelineLayout :: [VkDescriptorSetLayout] -> [VkPushConstantRange] -> VkPipelineLayoutCreateInfo
defineStandardPipelineLayout descriptorSetLayouts pushConstantRanges =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" 0 &*
  setListCountAndRef @"setLayoutCount" @"pSetLayouts" descriptorSetLayouts &*
  setListCountAndRef @"pushConstantRangeCount" @"pPushConstantRanges" pushConstantRanges

allocatedMemoryResource :: VkDevice -> VkaResource VkMemoryAllocateInfo VkDeviceMemory
allocatedMemoryResource device = VkaResource (return $ vkAllocateMemory device) (return $ vkFreeMemory device) "vkAllocateMemory" [VK_SUCCESS]

defineStandardMemoryAllocation :: VkDeviceSize -> Word32 -> VkMemoryAllocateInfo
defineStandardMemoryAllocation allocationSize memoryTypeIndex =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"allocationSize" allocationSize &*
  set @"memoryTypeIndex" memoryTypeIndex

bufferResource :: VkDevice -> VkaResource VkBufferCreateInfo VkBuffer
bufferResource device = VkaResource (return $ vkCreateBuffer device) (return $ vkDestroyBuffer device) "vkCreateBuffer" [VK_SUCCESS]

defineStandardBuffer :: VkDeviceSize -> VkBufferUsageFlags -> [Word32] -> VkBufferCreateInfo
defineStandardBuffer size usageFlags qfis =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"size" size &*
  set @"usage" usageFlags &*
  set @"sharingMode" (if null qfis' then VK_SHARING_MODE_EXCLUSIVE else VK_SHARING_MODE_CONCURRENT) &*
  setListCountAndRef @"queueFamilyIndexCount" @"pQueueFamilyIndices" qfis'

  where
    -- If just one QFI is provided, it's the same as providing none; both are exclusive mode.
    qfis' = if length qfis > 1 then qfis else []

allocateAndBindBufferMemory ::
  MonadUnliftIO io =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  VkBuffer ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe (VkBuffer, VkDeviceMemory))
allocateAndBindBufferMemory device pdmp buffer qualification =
  getVk (vkGetBufferMemoryRequirements device buffer) >>= \memReqs ->
  lift (
    pickByM qualification .
    filter (testBit (getField @"memoryTypeBits" memReqs) . fst) .
    take (fromIntegral $ getField @"memoryTypeCount" pdmp) $
    getFieldArrayAssocs @"memoryTypes" pdmp
  ) >>=
    mapM (\(chosenMemoryTypeIndex, _) -> do
      bufferMemory <-
        allocateAcquireVk_ (allocatedMemoryResource device) $
        defineStandardMemoryAllocation (getField @"size" memReqs) (fromIntegral chosenMemoryTypeIndex)
      liftIO $ vkaBindBufferMemory device buffer bufferMemory 0
      return (buffer, bufferMemory)
    )

vkaBindBufferMemory :: VkDevice -> VkBuffer -> VkDeviceMemory -> VkDeviceSize -> IO ()
vkaBindBufferMemory device buffer memory memoryOffset =
  void $ vkBindBufferMemory device buffer memory memoryOffset &
    onVkFailureThrow "vkBindBufferMemory" [VK_SUCCESS]

imageResource :: VkDevice -> VkaResource VkImageCreateInfo VkImage
imageResource device = VkaResource (return $ vkCreateImage device) (return $ vkDestroyImage device) "vkCreateImage" [VK_SUCCESS]

defineStandardImage ::
  VkImageCreateFlags ->
  VkImageType ->
  VkFormat ->
  VkExtent3D ->
  Word32 ->
  Word32 ->
  VkSampleCountFlagBits ->
  VkImageTiling ->
  VkImageUsageFlags ->
  [Word32] ->
  VkImageLayout ->
  VkImageCreateInfo
defineStandardImage flags imageType format extent numMipLevels numArrayLayers sampleCountFlagBits tiling usage qfis initialLayout =
  createVk $
  set @"sType" VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" flags &*
  set @"imageType" imageType &*
  set @"format" format &*
  set @"extent" extent &*
  set @"mipLevels" numMipLevels &*
  set @"arrayLayers" numArrayLayers &*
  set @"samples" sampleCountFlagBits &*
  set @"tiling" tiling &*
  set @"usage" usage &*
  set @"sharingMode" (if null qfis' then VK_SHARING_MODE_EXCLUSIVE else VK_SHARING_MODE_CONCURRENT) &*
  setListCountAndRef @"queueFamilyIndexCount" @"pQueueFamilyIndices" qfis' &*
  set @"initialLayout" initialLayout

  where
    -- If just one QFI is provided, it's the same as providing none; both are exclusive mode.
    qfis' = if length qfis > 1 then qfis else []

allocateAndBindImageMemory :: MonadUnliftIO io => VkDevice -> VkImage -> (Word32 -> Word32) -> ResourceT io (VkImage, VkDeviceMemory)
allocateAndBindImageMemory device image memoryTypeIndexFromMemoryTypeBits = do
  memReqs <- getVk $ vkGetImageMemoryRequirements device image
  imageMemory <-
    allocateAcquireVk_ (allocatedMemoryResource device) $
    defineStandardMemoryAllocation (getField @"size" memReqs) (memoryTypeIndexFromMemoryTypeBits $ getField @"memoryTypeBits" memReqs)
  liftIO $ vkaBindImageMemory device image imageMemory 0
  return (image, imageMemory)

vkaBindImageMemory :: VkDevice -> VkImage -> VkDeviceMemory -> VkDeviceSize -> IO ()
vkaBindImageMemory device buffer memory memoryOffset =
  void $ vkBindImageMemory device buffer memory memoryOffset &
    onVkFailureThrow "vkBindImageMemory" [VK_SUCCESS]

loadKtxTexture :: (MonadUnliftIO io, MonadThrow io) => VkDevice -> VkPhysicalDeviceMemoryProperties -> FilePath -> ResourceT io ()
loadKtxTexture device pdmp filePath =
  withBinaryFile filePath ReadMode . runFileReaderT $
  KTX.readAndCheckIdentifier >> KTX.readHeader >>= KTX.runKtxBodyReaderT (do
    textureDataSize <- KTX.getTextureDataSize
    stagingBuffer <-
      lift . lift . allocateAcquireVk_ (bufferResource device) $
      defineStandardBuffer (fromIntegral textureDataSize) VK_BUFFER_USAGE_TRANSFER_SRC_BIT []
--    stagingBufferMemory <- lift . lift $ allocateAndBindBufferMemory device pdmp stagingBuffer undefined
    KTX.skipMetadata
    undefined
  )

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

pickQueueFamilyIndexCombo :: forall i. Ix i => [QualificationM IO (i, VkQueueFamilyProperties)] -> StorableArray i VkQueueFamilyProperties -> IO [i]
pickQueueFamilyIndexCombo qualifications qfpArray =
  fmap (fmap fst . comboWithFewestDistinct) . sequence $
  P.picksByM <$> qualifications <*> pure (produceAssocs qfpArray)

getFieldArrayAssocs :: forall fname a. CanReadFieldArray fname a => a -> [(Int, FieldType fname a)]
getFieldArrayAssocs a = [0 .. fieldArrayLength @fname @a] <&> \i -> (i, getFieldArrayUnsafe @fname i a)

getFieldArrayElems :: forall fname a. CanReadFieldArray fname a => a -> [(FieldType fname a)]
getFieldArrayElems = fmap snd . getFieldArrayAssocs @fname @a
-- Vulkan helpers<

-- Other helpers>
newFunPtrFrom :: IO (FunPtr f) -> Acquire (FunPtr f)
newFunPtrFrom = flip mkAcquire freeHaskellFunPtr

lengthNum :: (Foldable t, Num n) => t a -> n
lengthNum = fromIntegral . length
{-# INLINE lengthNum #-}

comboWithFewestDistinct :: Eq a => [[a]] -> [a]
comboWithFewestDistinct = minimumBy (compare `on` length . nub) . sequence

ioPutStrLn :: MonadIO io => String -> io ()
ioPutStrLn = liftIO . putStrLn
-- Other helpers<
