{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module VulkanExample where

import Prelude.Local

import ApplicationException
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource.Local
import Data.Acquire.Local
import Data.Bits.Local
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Reflection
import Data.Tuple
import Foreign.C.String
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFWAux
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
#ifndef NDEBUG
import Graphics.Vulkan.Ext.VK_EXT_debug_report
#endif
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create.Local
import Graphics.Vulkan.Marshal.Create.DataFrame
import Graphics.VulkanAux

import UnliftIO.Exception

runVulkanExample :: ResourceT IO () -> IO ()
runVulkanExample exampleBody =
  runResourceT exampleBody
  `catch` (
    \(e :: ApplicationException) ->
      putStrLn $ displayException e
  )
  `catch` (
    \(e :: VkaResultException) ->
      putStrLn $ displayException e
  )
  `catch` (
    \(e :: GLFWException) ->
      putStrLn $ displayException e
  )

vulkanExampleMain ::
  String {- ^ example name -} ->
  [CString] {- ^ requested vulkan extensions -} ->
  [CString] {- ^ requested device extensions -} ->
  (Given VkPhysicalDeviceFeatures => Maybe VkPhysicalDeviceFeatures) {- ^ get enabled features given supported features -} ->
  (
    Given VkDevice =>
    PhysicalDevice ->
    GLFW.Window ->
    VkSurfaceKHR {- ^ window surface -} ->
    Map QueueType Queue ->
    ResourceT IO ()
  ) ->
  IO ()
vulkanExampleMain exampleName requestedVulkanExtensions requestedDeviceExtensions maybeEnabledFeatures example =
  runVulkanExample $ do
    (vulkanInstance, physicalDevice@PhysicalDevice{..}, window, windowSurface) <- initGlfwAndVulkan exampleName requestedVulkanExtensions
    (device, queuesByType) <-
      createVkDevice physicalDevice (defaultDeviceExtensions ++ requestedDeviceExtensions) Nothing . Map.fromList $ [
        (GraphicsQueueType, QueueSpec 1.0 zeroBits graphicsQueueFamilyQualification),
        (TransferQueueType, QueueSpec 1.0 zeroBits transferQueueFamilyQualification),
        (ComputeQueueType, QueueSpec 1.0 zeroBits computeQueueFamilyQualification),
        (PresentQueueType, QueueSpec 1.0 zeroBits (presentQueueFamilyQualification physicalDevice'object windowSurface))
      ]

    give device $ example physicalDevice window windowSurface queuesByType

defaultDeviceExtensions :: [CString]
defaultDeviceExtensions = [ VK_KHR_SWAPCHAIN_EXTENSION_NAME ]

initGlfwAndVulkan ::
  MonadResource m =>
  String {- ^ exampleName -} ->
  [CString] {- ^ extensions -} ->
  m (VkInstance, PhysicalDevice, GLFW.Window, VkSurfaceKHR)
initGlfwAndVulkan exampleName extensions = do
  liftIO . GLFW.setErrorCallback . Just $ \errorCode errorMessage ->
    putStr $ "GLFW error callback: " ++ show errorCode ++ " - " ++ errorMessage
  ioPutStrLn "GLFW error callback set."

  allocateAcquire_ acquireInitializedGLFW
  ioPutStrLn "GLFW initialized."

  glfwExtensions <- liftIO GLFW.getRequiredInstanceExtensions

  vkInstance <- do
    vkaAllocateResource_ vkaInstanceResource $
      createVk $
      initStandardInstanceCreateInfo &*
      setVkRef @"pApplicationInfo" (
        createVk $
        initStandardApplicationInfo &*
        setStrRef @"pApplicationName" ("Example - " ++ exampleName) &*
        set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0) &*
        setStrRef @"pEngineName" "" &*
        set @"engineVersion" 0 &*
        set @"apiVersion" VK_API_VERSION_1_1
      ) &*
      setStrListCountAndRef @"enabledLayerCount" @"ppEnabledLayerNames" (
#ifndef NDEBUG
        ["VK_LAYER_KHRONOS_validation", "VK_LAYER_LUNARG_api_dump"]
#else
        []
#endif
      ) &*
      setListCountAndRef @"enabledExtensionCount" @"ppEnabledExtensionNames" (
        extensions ++ glfwExtensions
#ifndef NDEBUG
        ++
        [
          VK_EXT_DEBUG_REPORT_EXTENSION_NAME
        ]
#endif
      )
  ioPutStrLn "Vulkan instance created."


#ifndef NDEBUG
  vkaRegisterDebugCallback vkInstance
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

  physicalDeviceArray <- vkaGetArray_ (vkaEnumeratePhysicalDevices vkInstance)

  physicalDevice <-
    liftIO $
    forM (vkaElems physicalDeviceArray) (\pd ->
      liftM3 (PhysicalDevice pd)
        (vkaGet_ . vkGetPhysicalDeviceProperties $ pd)
        (vkaGet_ . vkGetPhysicalDeviceMemoryProperties $ pd)
        (vkaGet_ . vkGetPhysicalDeviceFeatures $ pd)
    ) <&>
    sortBy (
      mconcat [
        prefer [VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU, VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU] `on` getField @"deviceType" . physicalDevice'properties,
        compare `on` vkaGetPhysicalDeviceLocalMemorySize . physicalDevice'memoryProperties
      ]
    ) <&>
    headOr (throwAppEx "No physical device found")
  ioPutStrLn "Physical device selected."

  window <- allocateAcquire_ $ acquireVulkanGLFWWindow 800 600 "Vulkan Sandbox"
  ioPutStrLn "Window created."

  windowSurface <- allocateAcquire_ $ newVulkanGLFWWindowSurface vkInstance window
  ioPutStrLn "Window surface created."

  return (vkInstance, physicalDevice, window, windowSurface)

createVkDevice ::
  (MonadResource m, Ord k) =>
  PhysicalDevice ->
  [CString] {- ^ device extensions -} ->
  (Given VkPhysicalDeviceFeatures => Maybe VkPhysicalDeviceFeatures) {- ^ get enabled features given supported features -} ->
  Map k (QueueSpec m) {- ^ map of specifications for how to creature queues. resulting queues will be in map with same structure -} ->
  m (VkDevice, Map k Queue)
createVkDevice PhysicalDevice{..} extensions maybeEnabledFeatures queueSpecsByKey = do
  queueFamilies <- vkaGetPhysicalDeviceQueueFamilies physicalDevice'object

  selectedQueueFamiliesByKey <-
    selectionFromM queueFamilies (fmap queueSpec'familyQualification queueSpecsByKey) <&>
    fromMaybe (throwAppEx "Failed to find qualifying queue families.")
  ioPutStrLn "Queue family indices selected."

  let keyListsByQueueFamily = groupValuesByKeyWith (compare `on` queueFamily'index) snd fst $ Map.toList selectedQueueFamiliesByKey

  device <-
    vkaAllocateResource_ (vkaDeviceResource physicalDevice'object) $
    createVk $
    initStandardDeviceCreateInfo &*
    setListCountAndRef @"queueCreateInfoCount" @"pQueueCreateInfos" (
      keyListsByQueueFamily <&> \(qf, keys) ->
        createVk $
        initStandardDeviceQueueCreateInfo &*
        set @"flags" zeroBits &*
        set @"queueFamilyIndex" (queueFamily'index qf) &*
        setListCountAndRef @"queueCount" @"pQueuePriorities" (queueSpec'priority . (queueSpecsByKey Map.!) <$> keys)
    ) &*
    setListCountAndRef @"enabledExtensionCount" @"ppEnabledExtensionNames" extensions &*
    (
      case give physicalDevice'features $ maybeEnabledFeatures of
        Just enabledFeatures -> setVkRef @"pEnabledFeatures" enabledFeatures
        Nothing -> set @"pEnabledFeatures" VK_NULL
    )
  ioPutStrLn "Vulkan device created."

  queuesByKey <-
    fmap Map.fromList . sequence $ do
      (qf, keys) <- keyListsByQueueFamily
      let qfi = queueFamily'index qf
      (qi, key) <- zip [0..] keys
      return $ do
        queue <- give device $ vkaGetDeviceQueue qfi qi
        commandPool <-
          give device $ vkaAllocateResource_ vkaCommandPoolResource $
          createVk $
          initStandardCommandPoolCreateInfo &*
          set @"flags" (queueSpec'commandPoolFlags $ queueSpecsByKey Map.! key) &*
          set @"queueFamilyIndex" qfi
        return $ (key, Queue queue qf commandPool)
  ioPutStrLn "Device queues obtained, and corresponding command pools created."

  return (device, queuesByKey)

data PhysicalDevice =
  PhysicalDevice {
    physicalDevice'object :: VkPhysicalDevice,
    physicalDevice'properties :: VkPhysicalDeviceProperties,
    physicalDevice'memoryProperties :: VkPhysicalDeviceMemoryProperties,
    physicalDevice'features :: VkPhysicalDeviceFeatures
  }

data QueueSpec m =
  QueueSpec {
    queueSpec'priority :: Float,
    queueSpec'commandPoolFlags :: VkCommandPoolCreateFlags,
    queueSpec'familyQualification :: QualificationM m QueueFamily
  }

data Queue =
  Queue {
    queue'object :: VkQueue,
    queue'family :: QueueFamily,
    queue'commandPool :: VkCommandPool
  }

data QueueType = GraphicsQueueType | ComputeQueueType | TransferQueueType | PresentQueueType deriving (Eq, Ord)

graphicsQueueFamilyQualification :: MonadIO m => QualificationM m QueueFamily
graphicsQueueFamilyQualification =
  (
    return . someAreSet VK_QUEUE_GRAPHICS_BIT . getField @"queueFlags" . queueFamily'properties,
    preferWhereM [
      return . noneAreSet VK_QUEUE_COMPUTE_BIT . getField @"queueFlags" . queueFamily'properties
    ]
  )

computeQueueFamilyQualification :: MonadIO m => QualificationM m QueueFamily
computeQueueFamilyQualification =
  (
    return . someAreSet VK_QUEUE_COMPUTE_BIT . getField @"queueFlags" . queueFamily'properties,
    preferWhereM [
      return . noneAreSet VK_QUEUE_GRAPHICS_BIT . getField @"queueFlags" . queueFamily'properties
    ]
  )

transferQueueFamilyQualification :: MonadIO m => QualificationM m QueueFamily
transferQueueFamilyQualification =
  (
    return . someAreSet (VK_QUEUE_GRAPHICS_BIT .|. VK_QUEUE_COMPUTE_BIT .|. VK_QUEUE_TRANSFER_BIT) . getField @"queueFlags" . queueFamily'properties,
    preferWhereM [
      return . noneAreSet (VK_QUEUE_GRAPHICS_BIT .|. VK_QUEUE_COMPUTE_BIT) . getField @"queueFlags" . queueFamily'properties
    ]
  )

presentQueueFamilyQualification :: MonadIO m => VkPhysicalDevice -> VkSurfaceKHR -> QualificationM m QueueFamily
presentQueueFamilyQualification physicalDevice surface =
  (
    fmap (VK_TRUE ==) . vkaGet_ . vkaGetPhysicalDeviceSurfaceSupportKHR physicalDevice surface . queueFamily'index,
    preferWhereM [
      return . someAreSet VK_QUEUE_GRAPHICS_BIT . getField @"queueFlags" . queueFamily'properties
    ]
  )
