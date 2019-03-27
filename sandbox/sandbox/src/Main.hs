{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude.Local

import qualified Codec.Image.Ktx.Read as KTX
import qualified Codec.Image.Ktx.VkConstants as KTX
import Control.Applicative
import Control.Exception (throw)
import Control.Monad
import Control.Monad.BufferWriter
import Control.Monad.Extra
import Control.Monad.Fail
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
import Data.Bits.Local
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
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import qualified GHC.Base as GHC
import qualified GHC.ForeignPtr as GHC

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Core_1_1
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
    \(e :: VkResultException) ->
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
    createVk $
    initStandardInstanceCreateInfo &*
    setVkRef @"pApplicationInfo" (
      createVk $
      initStandardApplicationInfo &*
      setStrRef @"pApplicationName" "Vulkan Sandbox" &*
      set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0) &*
      setStrRef @"pEngineName" "" &*
      set @"engineVersion" 0 &*
      set @"apiVersion" VK_API_VERSION_1_1
    ) &*
    setStrListCountAndRef @"enabledLayerCount" @"ppEnabledLayerNames" validationLayers &*
    setListCountAndRef @"enabledExtensionCount" @"ppEnabledExtensionNames" allExtensions
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
    pickQueueFamilyIndexCombo [
      (
        return . someAreSet VK_QUEUE_GRAPHICS_BIT . getField @"queueFlags" . snd,
        preferWhereM [
          return . noneAreSet VK_QUEUE_COMPUTE_BIT . getField @"queueFlags" . snd
        ]
      ),
      (
        return . someAreSet VK_QUEUE_COMPUTE_BIT . getField @"queueFlags" . snd,
        preferWhereM [
          return . noneAreSet VK_QUEUE_GRAPHICS_BIT . getField @"queueFlags" . snd
        ]
      ),
      (
        return . someAreSet (VK_QUEUE_GRAPHICS_BIT .|. VK_QUEUE_COMPUTE_BIT .|. VK_QUEUE_TRANSFER_BIT) . getField @"queueFlags" . snd,
        preferWhereM [
          return . noneAreSet (VK_QUEUE_GRAPHICS_BIT .|. VK_QUEUE_COMPUTE_BIT) . getField @"queueFlags" . snd
        ]
      ),
      (
        \(qfi, _) -> (VK_TRUE ==) <$> getVk (vkaGetPhysicalDeviceSurfaceSupportKHR physicalDevice qfi windowSurface),
        \_ _ -> return EQ
      )
    ]
      physicalDeviceQueueFamilyPropertiesArray
  ioPutStrLn "Queue family indices selected."

  device <-
    allocateAcquireVk_ (deviceResource physicalDevice) $
    createVk $
    initStandardDeviceCreateInfo &*
    setListCountAndRef @"queueCreateInfoCount" @"pQueueCreateInfos" (
      qfis <&> \qfi ->
        createVk $
        initStandardDeviceQueueCreateInfo &*
        set @"flags" 0 &*
        set @"queueFamilyIndex" qfi &*
        setListCountAndRef @"queueCount" @"pQueuePriorities" [1.0]
    ) &*
    setListCountAndRef @"enabledExtensionCount" @"ppEnabledExtensionNames" deviceExtensions &*
    set @"pEnabledFeatures" VK_NULL
  ioPutStrLn "Vulkan device created."

  [(graphicsQueue, graphicsCommandPool), (computeQueue, computeCommandPool), (transferQueue, transferCommandPool), (presentQueue, presentCommandPool)] <-
    let
      createCommandPool qfi =
        allocateAcquireVk_ (commandPoolResource device) $
        createVk $
        initStandardCommandPoolCreateInfo &*
        set @"flags" 0 &*
        set @"queueFamilyIndex" qfi
    in
      forM qfis $ \qfi -> liftM2 (,) (getVk $ vkGetDeviceQueue device qfi 0) (createCommandPool qfi)
  ioPutStrLn "Device queues obtained, and corresponding command pools created."

  descriptorSetLayout <-
    allocateAcquireVk_ (descriptorSetLayoutResource device) $
    createVk $
    initStandardDescriptorSetLayoutCreateInfo &*
    set @"flags" 0 &*
    setListCountAndRef @"bindingCount" @"pBindings" (
      fmap createVk [
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
    )
  ioPutStrLn "Descriptor set layout created."

  pipelineLayout <-
    allocateAcquireVk_ (pipelineLayoutResource device) $
    createVk $
    initStandardPipelineLayoutCreateInfo &*
    setListCountAndRef @"setLayoutCount" @"pSetLayouts" [descriptorSetLayout] &*
    setListCountAndRef @"pushConstantRangeCount" @"pPushConstantRanges" []
  ioPutStrLn "Pipeline layout created."

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

throwAppExM :: MonadThrow m => String -> m a
throwAppExM message = throwM $ ApplicationException message

-- GLFW helpers>
data GLFWException =
  GLFWException {
    glfwexFunctionName :: String
  } deriving (Eq, Show, Read)

instance Exception GLFWException where
  displayException (GLFWException functionName) = "GLFWException: " ++ functionName ++ " failed."

throwGLFWExceptionM :: MonadThrow m => String -> m a
throwGLFWExceptionM = throwM . GLFWException

initializedGLFW :: Acquire ()
initializedGLFW =
  unlessM GLFW.init (throwGLFWExceptionM "init")
  `mkAcquire`
  const GLFW.terminate

newVulkanGLFWWindow :: Int -> Int -> String -> Acquire GLFW.Window
newVulkanGLFWWindow width height title =
  do
    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
    fromMaybeM (throwGLFWExceptionM "createWindow") (GLFW.createWindow width height title Nothing Nothing)
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
data VkResultException =
  VkResultException {
    vkaResultException'functionName :: String,
    vkaResultException'result :: VkResult
  } deriving (Eq, Show, Read)

instance Exception VkResultException where
  displayException (VkResultException functionName result) =
    functionName ++ " failed with: " ++ show result

onVkFailureThrow :: String -> [VkResult] -> IO VkResult -> IO VkResult
onVkFailureThrow functionName successResults vkAction = do
  result <- vkAction
  unless (result `elem` successResults) $ throwIO (VkResultException functionName result)
  return result

data VkaException = VkaException String deriving (Eq, Show, Read)

instance Exception VkaException where
  displayException (VkaException message) = "VkaException: " ++ message

throwVkaException :: String -> a
throwVkaException = throw . VkaException

throwVkaExceptionM :: MonadThrow m => String -> m a
throwVkaExceptionM = throwM . VkaException

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
    vkaResource'getCreate :: IO (Ptr ci -> Ptr VkAllocationCallbacks -> Ptr vk -> IO VkResult),
    vkaResource'getDestroy :: IO (vk -> Ptr VkAllocationCallbacks -> IO ()),
    vkaResource'createName :: String,
    vkaResource'successResults :: [VkResult]
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

initStandardInstanceCreateInfo :: CreateVkStruct VkInstanceCreateInfo '["sType", "pNext"] ()
initStandardInstanceCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardApplicationInfo :: CreateVkStruct VkApplicationInfo '["sType", "pNext"] ()
initStandardApplicationInfo =
  set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO &*
  set @"pNext" VK_NULL

registeredDebugReportCallbackResource :: VkInstance -> VkaResource VkDebugReportCallbackCreateInfoEXT VkDebugReportCallbackEXT
registeredDebugReportCallbackResource vulkanInstance =
  VkaResource
    (vkGetInstanceProc @VkCreateDebugReportCallbackEXT vulkanInstance <*> pure vulkanInstance)
    (vkGetInstanceProc @VkDestroyDebugReportCallbackEXT vulkanInstance <*> pure vulkanInstance)
    "vkCreateDebugReportCallbackEXT"
    [VK_SUCCESS]

deviceResource :: VkPhysicalDevice -> VkaResource VkDeviceCreateInfo VkDevice
deviceResource physicalDevice = VkaResource (return $ vkCreateDevice physicalDevice) (return vkDestroyDevice) "vkCreateDevice" [VK_SUCCESS]

initStandardDeviceCreateInfo :: CreateVkStruct VkDeviceCreateInfo '["sType", "pNext", "flags", "enabledLayerCount", "ppEnabledLayerNames"] ()
initStandardDeviceCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" 0 &*
  set @"enabledLayerCount" 0 &*
  set @"ppEnabledLayerNames" VK_NULL

initStandardDeviceQueueCreateInfo :: CreateVkStruct VkDeviceQueueCreateInfo '["sType", "pNext"] ()
initStandardDeviceQueueCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO &*
  set @"pNext" VK_NULL

commandPoolResource :: VkDevice -> VkaResource VkCommandPoolCreateInfo VkCommandPool
commandPoolResource device = VkaResource (return $ vkCreateCommandPool device) (return $ vkDestroyCommandPool device) "vkCreateCommandPool" [VK_SUCCESS]

initStandardCommandPoolCreateInfo :: CreateVkStruct VkCommandPoolCreateInfo '["sType", "pNext"] ()
initStandardCommandPoolCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO &*
  set @"pNext" VK_NULL

descriptorSetLayoutResource :: VkDevice -> VkaResource VkDescriptorSetLayoutCreateInfo VkDescriptorSetLayout
descriptorSetLayoutResource device = VkaResource (return $ vkCreateDescriptorSetLayout device) (return $ vkDestroyDescriptorSetLayout device) "vkCreateDescriptorSetLayout" [VK_SUCCESS]

initStandardDescriptorSetLayoutCreateInfo :: CreateVkStruct VkDescriptorSetLayoutCreateInfo '["sType", "pNext"] ()
initStandardDescriptorSetLayoutCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO &*
  set @"pNext" VK_NULL

pipelineLayoutResource :: VkDevice -> VkaResource VkPipelineLayoutCreateInfo VkPipelineLayout
pipelineLayoutResource device = VkaResource (return $ vkCreatePipelineLayout device) (return $ vkDestroyPipelineLayout device) "vkCreatePipelineLayout" [VK_SUCCESS]

initStandardPipelineLayoutCreateInfo :: CreateVkStruct VkPipelineLayoutCreateInfo '["sType", "pNext", "flags"] ()
initStandardPipelineLayoutCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO &*
  set @"pNext" VK_NULL &*
  set @"flags" 0

allocatedMemoryResource :: VkDevice -> VkaResource VkMemoryAllocateInfo VkDeviceMemory
allocatedMemoryResource device = VkaResource (return $ vkAllocateMemory device) (return $ vkFreeMemory device) "vkAllocateMemory" [VK_SUCCESS]

initStandardMemoryAllocateInfo :: CreateVkStruct VkMemoryAllocateInfo '["sType", "pNext"] ()
initStandardMemoryAllocateInfo =
  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO &*
  set @"pNext" VK_NULL

bufferResource :: VkDevice -> VkaResource VkBufferCreateInfo VkBuffer
bufferResource device = VkaResource (return $ vkCreateBuffer device) (return $ vkDestroyBuffer device) "vkCreateBuffer" [VK_SUCCESS]

initStandardBufferCreateInfo :: CreateVkStruct VkBufferCreateInfo '["sType", "pNext"] ()
initStandardBufferCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO &*
  set @"pNext" VK_NULL

setSharingQueueFamilyIndices ::
  (
    CanWriteField "sharingMode" a,
    CanWriteField "queueFamilyIndexCount" a,
    CanWriteField "pQueueFamilyIndices" a,
    FieldType "sharingMode" a ~ VkSharingMode,
    FieldType "queueFamilyIndexCount" a ~ Word32,
    FieldType "pQueueFamilyIndices" a ~ Ptr Word32
  ) =>
  [Word32] ->
  CreateVkStruct a '["sharingMode", "queueFamilyIndexCount", "pQueueFamilyIndices"] ()
setSharingQueueFamilyIndices qfis =
  set @"sharingMode" (if null qfis' then VK_SHARING_MODE_EXCLUSIVE else VK_SHARING_MODE_CONCURRENT) &*
  setListCountAndRef @"queueFamilyIndexCount" @"pQueueFamilyIndices" qfis'
  where
    -- If just one QFI is provided, it's the same as providing none; both are exclusive mode,
    -- and the QFI list is ignored in that case.
    qfis' = if length qfis > 1 then qfis else []

imageResource :: VkDevice -> VkaResource VkImageCreateInfo VkImage
imageResource device = VkaResource (return $ vkCreateImage device) (return $ vkDestroyImage device) "vkCreateImage" [VK_SUCCESS]

initStandardImageCreateInfo :: CreateVkStruct VkImageCreateInfo '["sType", "pNext"] ()
initStandardImageCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO &*
  set @"pNext" VK_NULL

allocateAndBindVulkanMemory ::
  MonadUnliftIO io =>
  (VkDevice -> obj -> Ptr VkMemoryRequirements -> IO ()) ->
  (VkDevice -> obj -> VkDeviceMemory -> VkDeviceSize -> IO ()) ->
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  obj ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe VkDeviceMemory)
allocateAndBindVulkanMemory getMemoryRequirements bindMemory device pdmp obj qualification =
  getVk (getMemoryRequirements device obj) >>= \memReqs ->
  lift (
    pickByM qualification .
    filter (testBit (getField @"memoryTypeBits" memReqs) . fst) .
    take (fromIntegral $ getField @"memoryTypeCount" pdmp) $
    getFieldArrayAssocs @"memoryTypes" pdmp
  ) >>=
    mapM (\(chosenMemoryTypeIndex, _) -> do
      memory <-
        allocateAcquireVk_ (allocatedMemoryResource device) $
        createVk $
        initStandardMemoryAllocateInfo &*
        set @"allocationSize" (getField @"size" memReqs) &*
        set @"memoryTypeIndex" (fromIntegral chosenMemoryTypeIndex)
      liftIO $ bindMemory device obj memory 0
      return memory
    )

allocateAndBindBufferMemory ::
  MonadUnliftIO io =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  VkBuffer ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe VkDeviceMemory)
allocateAndBindBufferMemory = allocateAndBindVulkanMemory vkGetBufferMemoryRequirements vkaBindBufferMemory

vkaBindBufferMemory :: VkDevice -> VkBuffer -> VkDeviceMemory -> VkDeviceSize -> IO ()
vkaBindBufferMemory device buffer memory memoryOffset =
  void $ vkBindBufferMemory device buffer memory memoryOffset &
    onVkFailureThrow "vkBindBufferMemory" [VK_SUCCESS]

allocateAndBindImageMemory ::
  MonadUnliftIO io =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  VkImage ->
  QualificationM io (Int, VkMemoryType) ->
  ResourceT io (Maybe VkDeviceMemory)
allocateAndBindImageMemory = allocateAndBindVulkanMemory vkGetImageMemoryRequirements vkaBindImageMemory

vkaBindImageMemory :: VkDevice -> VkImage -> VkDeviceMemory -> VkDeviceSize -> IO ()
vkaBindImageMemory device buffer memory memoryOffset =
  void $ vkBindImageMemory device buffer memory memoryOffset &
    onVkFailureThrow "vkBindImageMemory" [VK_SUCCESS]

vkaMapMemory :: VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> VkMemoryMapFlags -> IO (Ptr Void)
vkaMapMemory device deviceMemory offset size flags =
  alloca $ \ptrPtr -> do
    void $ vkMapMemory device deviceMemory offset size flags ptrPtr &
      onVkFailureThrow "vkMapMemory" [VK_SUCCESS]
    peek ptrPtr

mappedMemory :: VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> Acquire (Ptr Void)
mappedMemory device deviceMemory offset size =
  vkaMapMemory device deviceMemory offset size 0
  `mkAcquire`
  const (vkUnmapMemory device deviceMemory)

allocatedCommandBuffers :: VkDevice -> VkCommandBufferAllocateInfo -> Acquire (StorableArray Word32 VkCommandBuffer)
allocatedCommandBuffers device allocateInfo =
  do
    array <- newArray_ (0, commandBufferCount-1)
    when (commandBufferCount > 0) $
      withPtr allocateInfo $ \allocateInfoPtr ->
        void $ withStorableArray array (vkAllocateCommandBuffers device allocateInfoPtr) &
          onVkFailureThrow "vkAllocateCommandBuffers" [VK_SUCCESS]
    return array
  `mkAcquire`
  if commandBufferCount > 0 then
    \commandBufferArray -> withStorableArray commandBufferArray $ vkFreeCommandBuffers device commandPool commandBufferCount
  else
    const $ return ()

  where
    commandBufferCount = getField @"commandBufferCount" allocateInfo
    commandPool = getField @"commandPool" allocateInfo

initStandardCommandBufferAllocateInfo :: CreateVkStruct VkCommandBufferAllocateInfo '["sType", "pNext"] ()
initStandardCommandBufferAllocateInfo =
  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO &*
  set @"pNext" VK_NULL

recordingCommandBuffer :: VkCommandBuffer -> VkCommandBufferBeginInfo -> Acquire ()
recordingCommandBuffer commandBuffer beginInfo =
  (
    withPtr beginInfo $ \beginInfoPtr ->
      void $ vkBeginCommandBuffer commandBuffer beginInfoPtr &
        onVkFailureThrow "vkBeginCommandBuffer" [VK_SUCCESS]
  )
  `mkAcquire`
  const (
    void $ vkEndCommandBuffer commandBuffer &
      onVkFailureThrow "vkEndCommandBuffer" [VK_SUCCESS]
  )

initStandardCommandBufferBeginInfo :: CreateVkStruct VkCommandBufferBeginInfo '["sType", "pNext"] ()
initStandardCommandBufferBeginInfo =
  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO &*
  set @"pNext" VK_NULL

initPrimaryCommandBufferBeginInfo :: CreateVkStruct VkCommandBufferBeginInfo '["sType", "pNext", "pInheritanceInfo"] ()
initPrimaryCommandBufferBeginInfo =
  initStandardCommandBufferBeginInfo &*
  set @"pInheritanceInfo" VK_NULL

vkaQueueWaitIdle :: VkQueue -> IO ()
vkaQueueWaitIdle queue = void $ vkQueueWaitIdle queue & onVkFailureThrow "vkQueueWaitIdle" [VK_SUCCESS]

vkaQueueSubmit :: VkQueue -> [VkSubmitInfo] -> VkFence -> IO ()
vkaQueueSubmit queue submitInfos fence =
  withArray submitInfos $ \submitInfosPtr ->
  void $ vkQueueSubmit queue (lengthNum submitInfos) submitInfosPtr fence &
    onVkFailureThrow "vkQueueSubmit" [VK_SUCCESS]

initStandardSubmitInfo :: CreateVkStruct VkSubmitInfo '["sType", "pNext"] ()
initStandardSubmitInfo =
  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO &*
  set @"pNext" VK_NULL

setSubmitWaitSemaphoresAndStageFlags ::
  [(VkSemaphore, VkPipelineStageFlags)] ->
  CreateVkStruct VkSubmitInfo '["waitSemaphoreCount", "pWaitSemaphores", "pWaitDstStageMask"] ()
setSubmitWaitSemaphoresAndStageFlags waitSemaphoresAndStageFlags =
  setListCountAndRef @"waitSemaphoreCount" @"pWaitSemaphores" (fst <$> waitSemaphoresAndStageFlags) &*
  setListRef @"pWaitDstStageMask" (snd <$> waitSemaphoresAndStageFlags)

fenceResource :: VkDevice -> VkaResource VkFenceCreateInfo VkFence
fenceResource device = VkaResource (return $ vkCreateFence device) (return $ vkDestroyFence device) "vkCreateFence" [VK_SUCCESS]

initStandardFenceCreateInfo :: CreateVkStruct VkFenceCreateInfo '["sType", "pNext"] ()
initStandardFenceCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO &*
  set @"pNext" VK_NULL

setFenceSignaled :: Bool -> CreateVkStruct VkFenceCreateInfo '["flags"] ()
setFenceSignaled isSignaled = set @"flags" (if isSignaled then VK_FENCE_CREATE_SIGNALED_BIT else 0)

vkaWaitForFences :: VkDevice -> [VkFence] -> VkBool32 -> Word64 -> IO VkResult
vkaWaitForFences device fences waitAll timeout =
  withArray fences $ \fencesPtr ->
  vkWaitForFences device (lengthNum fences) fencesPtr waitAll timeout &
    onVkFailureThrow "vkWaitForFences" [VK_SUCCESS, VK_TIMEOUT]

vkaWaitForFence :: VkDevice -> VkFence -> Word64 -> IO VkResult
vkaWaitForFence device fence timeout = vkaWaitForFences device [fence] VK_TRUE timeout

executeCommands :: (MonadUnliftIO m, MonadFail m) => VkDevice -> VkCommandPool -> VkQueue -> (forall n. MonadIO n => VkCommandBuffer -> n a) -> m a
executeCommands device commandPool submissionQueue fillCommandBuffer = runResourceT $ do
  [commandBuffer] <-
    liftIO . getElems =<< (
      allocateAcquire_ $ allocatedCommandBuffers device $
      createVk $
      initStandardCommandBufferAllocateInfo &*
      set @"commandPool" commandPool &*
      set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY &*
      set @"commandBufferCount" 1
    )

  result <-
    with_ (
      recordingCommandBuffer commandBuffer $
      createVk $
      initPrimaryCommandBufferBeginInfo &*
      set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    )
    (fillCommandBuffer commandBuffer)

  executionCompleteFence <-
    allocateAcquireVk_ (fenceResource device) $
    createVk $
    initStandardFenceCreateInfo &*
    setFenceSignaled False

  liftIO $ do
    vkaQueueSubmit submissionQueue
      [
        createVk (
          initStandardSubmitInfo &*
          setSubmitWaitSemaphoresAndStageFlags [] &*
          setListCountAndRef @"commandBufferCount" @"pCommandBuffers" [commandBuffer] &*
          setListCountAndRef @"signalSemaphoreCount" @"pSignalSemaphores" []
        )
      ]
      executionCompleteFence
    vkaWaitForFence device executionCompleteFence maxBound

  return result

vkaCmdPipelineBarrier ::
  VkCommandBuffer ->
  VkPipelineStageFlags ->
  VkPipelineStageFlags ->
  VkDependencyFlags ->
  [VkMemoryBarrier] ->
  [VkBufferMemoryBarrier] ->
  [VkImageMemoryBarrier] ->
  IO ()
vkaCmdPipelineBarrier
  commandBuffer
  srcStageMask
  dstStageMask
  depFlags
  memoryBarriers
  bufferMemoryBarriers
  imageMemoryBarriers
  =
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

initStandardImageMemoryBarrier :: CreateVkStruct VkImageMemoryBarrier '["sType", "pNext"] ()
initStandardImageMemoryBarrier =
  set @"sType" VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER &*
  set @"pNext" VK_NULL

loadKtxTexture ::
  (MonadUnliftIO io, MonadThrow io) =>
  VkDevice ->
  VkPhysicalDeviceMemoryProperties ->
  VkSampleCountFlagBits ->
  VkImageTiling ->
  VkImageUsageFlags ->
  [Word32] ->
  VkImageLayout ->
  FilePath ->
  ResourceT io ()
loadKtxTexture device pdmp sampleCountFlagBit tiling usageFlags qfis initialLayout filePath =
  withBinaryFile filePath ReadMode . runFileReaderT $
  KTX.readAndCheckIdentifier >> KTX.readHeader >>= KTX.runKtxBodyReaderT (do
    KTX.skipMetadata
    textureDataSize <- KTX.getTextureDataSize

    let textureDataVkSize = fromIntegral textureDataSize

    stagingBuffer <-
      lift . lift $
      allocateAcquireVk_ (bufferResource device) $
      createVk $
      initStandardBufferCreateInfo &*
      set @"flags" 0 &*
      set @"size" textureDataVkSize &*
      set @"usage" VK_BUFFER_USAGE_TRANSFER_SRC_BIT &*
      setSharingQueueFamilyIndices []

    stagingBufferMemory <-
      lift . lift $
      fromMaybeM (throwVkaExceptionM "Failed to find a suitable memory type for the staging buffer.") $
      allocateAndBindBufferMemory device pdmp stagingBuffer (
        return . allAreSet (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) . getField @"propertyFlags" . snd,
        \_ _ -> return EQ
      )

    bufferRegions <-
      with (mappedMemory device stagingBufferMemory 0 textureDataVkSize) $ \stagingBufferPtr ->
        evalBufferWriterT KTX.readTextureDataIntoBuffer (castPtr stagingBufferPtr, textureDataSize) 0

    -- TODO: end the KTX-reading context here, returning the staging buffer and its memory along with the KTX Header and BufferRegions.
    header@KTX.Header {..} <- ask

    let
      isCubeMap = KTX.isCubeMap header
      isArray = KTX.isArray header

    image <-
      lift . lift $
      allocateAcquireVk_ (imageResource device) $
      createVk $
      initStandardImageCreateInfo &*
      set @"flags" (
        setIf isCubeMap VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT .|.
        setIf isArray VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
      ) &*
      set @"imageType" (
        case (header'pixelHeight, header'pixelDepth) of
          (0, 0) -> VK_IMAGE_TYPE_1D
          (_, 0) -> VK_IMAGE_TYPE_2D
          _ -> VK_IMAGE_TYPE_3D
      ) &*
      set @"format" (
        fromMaybe (throwVkaException "Unsupported KTX format.") $
        KTX.getVkFormatFromGlTypeAndFormat header'glType header'glFormat <|>
        KTX.getVkFormatFromGlInternalFormat header'glInternalFormat
      ) &*
      setVk @"extent" (
        set @"width" header'pixelWidth &*
        set @"height" header'pixelHeight &*
        set @"depth" header'pixelDepth
      ) &*
      set @"mipLevels" header'numberOfMipmapLevels &*
      set @"arrayLayers" header'numberOfArrayElements &*
      set @"samples" sampleCountFlagBit &*
      set @"tiling" tiling &*
      set @"usage" usageFlags &*
      setSharingQueueFamilyIndices qfis &*
      set @"initialLayout" initialLayout

    imageMemory <-
      lift . lift $
      fromMaybeM (throwVkaExceptionM "Failed to find a suitable memory type for the image.") $
      allocateAndBindImageMemory device pdmp image (
        return . allAreSet VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT . getField @"propertyFlags" . snd,
        \_ _ -> return EQ
      )

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
    let fillArray' = fillArray countPtr
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
