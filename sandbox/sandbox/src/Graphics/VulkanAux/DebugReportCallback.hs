module Graphics.VulkanAux.DebugReportCallback where

import Control.Monad.Trans.Resource.Local
import Data.Functor
import Foreign.Ptr
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Ext.VK_EXT_debug_report
import Graphics.VulkanAux.Resource

vkaRegisterDebugCallback ::
  MonadResource m =>
  VkInstance ->
  VkDebugReportFlagsEXT ->
  HS_vkDebugReportCallbackEXT ->
  m ()
vkaRegisterDebugCallback vulkanInstance flags debugCallback = do
  (_, debugCallbackPtr) <- allocate (newVkDebugReportCallbackEXT debugCallback) freeHaskellFunPtr
  void . vkaAllocateResource_ (vkaRegisteredDebugReportCallbackResource vulkanInstance) $
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
