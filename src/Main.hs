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
    initWindow >>= \case
      Just window -> do
        vkInstance <- do
          (count, extensionsArray) <- GLFW.getRequiredInstanceExtensions
          initVulkan =<< peekArray (fromIntegral count) extensionsArray
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

(width, height) = (800, 600);

initWindow :: IO (Maybe GLFW.Window)
initWindow = do
  GLFW.init
  GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  GLFW.windowHint $ WindowHint'Resizable False
  GLFW.createWindow width height "Vulkan" Nothing Nothing

initVulkan :: [CString] -> IO VkInstance
initVulkan extensions = do
  createInstance

  where
    createInstance :: IO VkInstance
    createInstance =
      withPtr instanceCreateInfo $ \pInstanceCreateInfo ->
        alloca $ \pVkInstance -> do
          onVkFailureThrow "vkCreateInstance failed." $
            vkCreateInstance pInstanceCreateInfo VK_NULL pVkInstance
          peek pVkInstance

      where
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
          set @"enabledLayerCount" 0 &*
          set @"ppEnabledLayerNames" VK_NULL &*
          set @"pNext" VK_NULL

mainLoop :: GLFW.Window -> IO ()
mainLoop window = whileM_ (not <$> GLFW.windowShouldClose window) GLFW.pollEvents

cleanup :: GLFW.Window -> VkInstance -> IO ()
cleanup window vkInstance = do
  vkDestroyInstance vkInstance VK_NULL
  GLFW.destroyWindow window
  GLFW.terminate
