module Graphics.VulkanAux.DebugReportCallback where

import Control.Monad.Trans.Resource.Local
import Data.Acquire.Local
import Data.Functor
import Foreign.Ptr
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Ext.VK_EXT_debug_report
import Graphics.VulkanAux.Resource

vkaRegisterDebugCallback :: MonadUnliftIO io => VkInstance -> VkDebugReportFlagsEXT -> HS_vkDebugReportCallbackEXT -> ResourceT io ()
vkaRegisterDebugCallback vulkanInstance flags debugCallback = do
  debugCallbackPtr <- allocate_ (newVkDebugReportCallbackEXT debugCallback) freeHaskellFunPtr
  void . allocateAcquireVk_ (vkaRegisteredDebugReportCallbackResource vulkanInstance) $
    createVk $
    set @"sType" VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT &*
    set @"pNext" VK_NULL &*
    set @"flags" flags &*
    set @"pfnCallback" debugCallbackPtr

vkaRegisteredDebugReportCallbackResource :: VkInstance -> VkaResource VkDebugReportCallbackCreateInfoEXT VkDebugReportCallbackEXT
vkaRegisteredDebugReportCallbackResource vulkanInstance =
  VkaResource
    (vkGetInstanceProc @VkCreateDebugReportCallbackEXT vulkanInstance <*> pure vulkanInstance)
    (vkGetInstanceProc @VkDestroyDebugReportCallbackEXT vulkanInstance <*> pure vulkanInstance)
    "vkCreateDebugReportCallbackEXT"
    [VK_SUCCESS]
