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
import Data.Function
import Data.Functor
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.UI.GLFW (WindowHint(..), ClientAPI(..))
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

main :: IO ()
main =
  (
    initWindow width height >>= \case
      Just window -> do
        vkInstance <- do
          (count, extensionsArray) <- GLFW.getRequiredInstanceExtensions
          extensions <- peekArray (fromIntegral count) extensionsArray
          initVulkan validationLayers extensions
        putStrLn "Instance created, entering main loop."
        mainLoop window
        putStrLn "Main loop ended, cleaning up."
        cleanup window vkInstance
      Nothing ->
        putStrLn "Failed to initialize the GLFW window."
  )
  `catch` (
    \(e :: VulkanException) ->
      putStrLn $ displayException e
  )
  where
    (width, height) = (800, 600)
    validationLayers =
      [
#ifndef NDEBUG
        "VK_LAYER_LUNARG_standard_validation"
#endif
      ]

data VulkanException =
  VulkanException {
    vkeCode :: Maybe VkResult,
    vkeMessage :: String
  }
  deriving (Eq, Show, Read)

instance Exception VulkanException where
  displayException (VulkanException maybeCode message) =
    "Vulkan error" ++
    maybe "" (\code -> " (" ++ show code ++ ")") maybeCode ++
    ": " ++ message

onVkFailureThrow :: String -> IO VkResult -> IO ()
onVkFailureThrow message vkAction = do
  result <- vkAction
  when (result /= VK_SUCCESS) $ throwIO $ VulkanException (Just result) message

throwVkMsg :: String -> IO a
throwVkMsg message = throwIO $ VulkanException Nothing message

initWindow :: Int -> Int -> IO (Maybe GLFW.Window)
initWindow width height = do
  GLFW.init
  GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  GLFW.windowHint $ WindowHint'Resizable False
  GLFW.createWindow width height "Vulkan" Nothing Nothing

initVulkan :: [String] -> [CString] -> IO VkInstance
initVulkan validationLayers extensions = do
  createInstance

  where
    createInstance :: IO VkInstance
    createInstance =
      getUnsupportedValidationLayerNames validationLayers >>= \case
        [] -> do
          putStrLn "All expected validation layers are available."
          withPtr instanceCreateInfo $ \instanceCreateInfoPtr ->
            alloca $ \vkInstancePtr -> do
              onVkFailureThrow "vkCreateInstance failed." $
                vkCreateInstance instanceCreateInfoPtr VK_NULL vkInstancePtr
              peek vkInstancePtr
        unsupportedLayers -> throwVkMsg $ "Expected validations are not available: " ++ show unsupportedLayers

      where
        instanceCreateInfo :: VkInstanceCreateInfo
        instanceCreateInfo =
          createVk @VkInstanceCreateInfo $
          set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO &*
          setVkRef @"pApplicationInfo" (
            createVk @VkApplicationInfo $
            set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO &*
            setStrRef @"pApplicationName" "Hello Triangle" &*
            set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0) &*
            setStrRef @"pEngineName" "No Engine" &*
            set @"engineVersion" (_VK_MAKE_VERSION 1 0 0) &*
            set @"apiVersion" VK_API_VERSION_1_0 &*
            set @"pNext" VK_NULL
          ) &*
          set @"enabledExtensionCount" (fromIntegral $ length extensions) &*
          setListRef @"ppEnabledExtensionNames" extensions &*
          set @"enabledLayerCount" (fromIntegral $ length validationLayers) &*
          setStrListRef @"ppEnabledLayerNames" validationLayers &*
          set @"pNext" VK_NULL

getUnsupportedValidationLayerNames :: [String] -> IO [String]
getUnsupportedValidationLayerNames [] = return []
getUnsupportedValidationLayerNames expectedLayerNames =
  getAvailableValidationLayers
  <&> fmap (getStringField @"layerName")
  <&> \case
    [] -> expectedLayerNames
    availableLayerNames -> filter (not . elemOf availableLayerNames) expectedLayerNames
  where
    (<&>) = flip (<$>)
    infixl 1 <&>
    elemOf = flip elem

getAvailableValidationLayers :: IO [VkLayerProperties]
getAvailableValidationLayers =
  alloca $ \layerCountPtr -> do
    vkEnumerateInstanceLayerProperties layerCountPtr VK_NULL
    layerCount <- fromIntegral <$> peek layerCountPtr
    if layerCount > 0 then
      allocaArray layerCount $ \layersPtr -> do
        vkEnumerateInstanceLayerProperties layerCountPtr layersPtr
        peekArray layerCount layersPtr
    else
      return []

mainLoop :: GLFW.Window -> IO ()
mainLoop window = whileM_ (not <$> GLFW.windowShouldClose window) GLFW.pollEvents

cleanup :: GLFW.Window -> VkInstance -> IO ()
cleanup window vkInstance = do
  vkDestroyInstance vkInstance VK_NULL
  GLFW.destroyWindow window
  GLFW.terminate
