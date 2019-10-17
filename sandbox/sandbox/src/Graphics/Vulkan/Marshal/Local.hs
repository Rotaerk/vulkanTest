{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.Vulkan.Marshal.Local (
  module Graphics.Vulkan.Marshal,
  getFieldArrayAssocs,
  getFieldArrayElems
) where

import Data.Functor
import Graphics.Vulkan.Marshal
  
getFieldArrayAssocs :: forall fname a. CanReadFieldArray fname a => a -> [(Int, FieldType fname a)]
getFieldArrayAssocs a = [0 .. fieldArrayLength @fname @a] <&> \i -> (i, getFieldArrayUnsafe @fname i a)

getFieldArrayElems :: forall fname a. CanReadFieldArray fname a => a -> [(FieldType fname a)]
getFieldArrayElems = fmap snd . getFieldArrayAssocs @fname @a
