{-# LANGUAGE TupleSections #-}

module Graphics.VulkanAux.VkaIArray (
  VkaIArray(),
  newVkaIArray, newVkaIArray_,
  acquireVkaIArray, acquireVkaIArray_,
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

newtype VkaIArray vk = VkaIArray { unVkaIArray :: StorableArray Word32 vk }

newVkaIArray :: Storable vk => Word32 -> (Ptr vk -> IO r) -> IO (r, VkaIArray vk)
newVkaIArray count fill = do
  sarr <- newArray_ (0, count-1)
  (, VkaIArray sarr) <$> withStorableArray sarr fill

newVkaIArray_ :: Storable vk => Word32 -> (Ptr vk -> IO ()) -> IO (VkaIArray vk)
newVkaIArray_ = fmap snd .: newVkaIArray

acquireVkaIArray :: Storable vk => Word32 -> (Ptr vk -> IO r) -> (Ptr vk -> IO ()) -> Acquire (r, VkaIArray vk)
acquireVkaIArray count fill destroy =
  second VkaIArray <$>
  do
    sarr <- newArray_ (0, count-1)
    (, sarr) <$> withStorableArray sarr fill
  `mkAcquire`
  (flip withStorableArray destroy . snd)

acquireVkaIArray_ :: Storable vk => Word32 -> (Ptr vk -> IO ()) -> (Ptr vk -> IO ()) -> Acquire (VkaIArray vk)
acquireVkaIArray_ = fmap snd .:. acquireVkaIArray

vkaNumElements :: Storable vk => VkaIArray vk -> Word32
vkaNumElements = fromIntegral . unsafePerformIO . getNumElements . unVkaIArray

vkaElems :: Storable vk => VkaIArray vk -> [vk]
vkaElems = unsafePerformIO . getElems . unVkaIArray

vkaAssocs :: Storable vk => VkaIArray vk -> [(Word32, vk)]
vkaAssocs = unsafePerformIO . getAssocs . unVkaIArray
