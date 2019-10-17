{-# LANGUAGE TupleSections #-}

module Graphics.VulkanAux.Array (
  VkaArray(),
  newVkaArray, newVkaArray_,
  acquireVkaArray, acquireVkaArray_,
  vkaNumElements,
  vkaElems,
  vkaAssocs
) where

import Prelude.Local

import Data.Acquire.Local
import Data.Array.Base
import Data.Array.Storable
import Data.Tuple.Extra
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

newtype VkaArray vk = VkaArray { unVkaArray :: StorableArray Word32 vk }

newVkaArray :: Storable vk => Word32 -> (Ptr vk -> IO r) -> IO (r, VkaArray vk)
newVkaArray count fill = do
  sarr <- newArray_ (0, count-1)
  (, VkaArray sarr) <$> withStorableArray sarr fill

newVkaArray_ :: Storable vk => Word32 -> (Ptr vk -> IO ()) -> IO (VkaArray vk)
newVkaArray_ = fmap snd .: newVkaArray

acquireVkaArray :: Storable vk => Word32 -> (Ptr vk -> IO r) -> (Ptr vk -> IO ()) -> Acquire (r, VkaArray vk)
acquireVkaArray count fill destroy =
  second VkaArray <$>
  do
    sarr <- newArray_ (0, count-1)
    (, sarr) <$> withStorableArray sarr fill
  `mkAcquire`
  (flip withStorableArray destroy . snd)

acquireVkaArray_ :: Storable vk => Word32 -> (Ptr vk -> IO ()) -> (Ptr vk -> IO ()) -> Acquire (VkaArray vk)
acquireVkaArray_ = fmap snd .:. acquireVkaArray

vkaNumElements :: Storable vk => VkaArray vk -> Word32
vkaNumElements = fromIntegral . unsafePerformIO . getNumElements . unVkaArray

vkaElems :: Storable vk => VkaArray vk -> [vk]
vkaElems = unsafePerformIO . getElems . unVkaArray

vkaAssocs :: Storable vk => VkaArray vk -> [(Word32, vk)]
vkaAssocs = unsafePerformIO . getAssocs . unVkaArray
