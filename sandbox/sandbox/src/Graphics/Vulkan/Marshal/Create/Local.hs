{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.Vulkan.Marshal.Create.Local (
  module Graphics.Vulkan.Marshal.Create,
  copyField
) where

import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Create
  
copyField :: forall fname a. (CanWriteField fname a, CanReadField fname a) => a -> CreateVkStruct a '[fname] ()
copyField src = set @fname (getField @fname src)
