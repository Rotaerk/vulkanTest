module Foreign.Marshal.Array.Sized where

import Data.Acquire
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

data SizedArray a = SizedArray { arraySize :: Int, arrayPtr :: Ptr a }

mallocSizedArray :: Storable a => Int -> IO (SizedArray a)
mallocSizedArray size = SizedArray size <$> mallocArray size

freeSizedArray :: SizedArray a -> IO ()
freeSizedArray = free . arrayPtr

acquireSizedArray :: Storable a => Int -> Acquire (SizedArray a)
acquireSizedArray size = mallocSizedArray size `mkAcquire` freeSizedArray
