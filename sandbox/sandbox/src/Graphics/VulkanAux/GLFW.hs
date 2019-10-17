module Graphics.VulkanAux.GLFW where

import Data.Acquire.Local
import Data.Function
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.VulkanAux.Exception

newVulkanGLFWWindowSurface :: VkInstance -> GLFW.Window -> Acquire VkSurfaceKHR
newVulkanGLFWWindowSurface vulkanInstance window =
  (
    alloca $ \surfacePtr -> do
      GLFW.createWindowSurface vulkanInstance window nullPtr surfacePtr & onVkFailureThrow_ "GLFW.createWindowSurface"
      peek surfacePtr
  )
  `mkAcquire`
  \surface -> vkDestroySurfaceKHR vulkanInstance surface VK_NULL
