{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.Vulkan.Marshal.Create.Local (
  module Graphics.Vulkan.Marshal.Create,
  copyFieldFrom,
  copyField
) where

import Data.Reflection
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Create
  
copyFieldFrom :: forall fname a. (CanWriteField fname a, CanReadField fname a) => a -> CreateVkStruct a '[fname] ()
copyFieldFrom src = set @fname (getField @fname src)

copyField :: forall fname a. (CanWriteField fname a, CanReadField fname a, Given a) => CreateVkStruct a '[fname] ()
copyField = copyFieldFrom given
