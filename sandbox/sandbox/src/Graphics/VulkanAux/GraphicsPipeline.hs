module Graphics.VulkanAux.GraphicsPipeline where

import Prelude.Local

import Control.Monad.Trans.Resource.Local
import Data.Function
import Data.Functor
import Data.Reflection
import Foreign.Marshal.Array
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.VulkanAux.Array
import Graphics.VulkanAux.Exception

vkaCreateGraphicsPipelines :: Given VkDevice => VkPipelineCache -> [VkGraphicsPipelineCreateInfo] -> IO (VkaArray VkPipeline)
vkaCreateGraphicsPipelines pipelineCache createInfos@(lengthNum -> count) =
  if count > 0 then
    withArray createInfos $ \createInfosPtr ->
      vkaNewArray_ count $ \arrayPtr ->
        vkCreateGraphicsPipelines given pipelineCache count createInfosPtr VK_NULL arrayPtr & onVkFailureThrow_ "vkCreateGraphicsPipelines"
  else
    throwVkaException "Cannot allocate 0 graphics pipelines."

vkaRegisterGraphicsPipelineForDestruction :: (MonadResource m, Given VkDevice) => VkPipeline -> m ReleaseKey
vkaRegisterGraphicsPipelineForDestruction pipeline = register $ vkDestroyPipeline given pipeline VK_NULL

vkaRegisterGraphicsPipelineForDestruction_ :: (MonadResource m, Given VkDevice) => VkPipeline -> m ()
vkaRegisterGraphicsPipelineForDestruction_ = void . vkaRegisterGraphicsPipelineForDestruction

initStandardGraphicsPipelineCreateInfo :: CreateVkStruct VkGraphicsPipelineCreateInfo '["sType", "pNext"] ()
initStandardGraphicsPipelineCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineShaderStageCreateInfo :: CreateVkStruct VkPipelineShaderStageCreateInfo '["sType", "pNext"] ()
initStandardPipelineShaderStageCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineVertexInputStateCreateInfo :: CreateVkStruct VkPipelineVertexInputStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineVertexInputStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineInputAssemblyStateCreateInfo :: CreateVkStruct VkPipelineInputAssemblyStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineInputAssemblyStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineViewportStateCreateInfo :: CreateVkStruct VkPipelineViewportStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineViewportStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineRasterizationStateCreateInfo :: CreateVkStruct VkPipelineRasterizationStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineRasterizationStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineMultisampleStateCreateInfo :: CreateVkStruct VkPipelineMultisampleStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineMultisampleStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineDepthStencilStateCreateInfo :: CreateVkStruct VkPipelineDepthStencilStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineDepthStencilStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

initStandardPipelineColorBlendStateCreateInfo :: CreateVkStruct VkPipelineColorBlendStateCreateInfo '["sType", "pNext"] ()
initStandardPipelineColorBlendStateCreateInfo =
  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO &*
  set @"pNext" VK_NULL

