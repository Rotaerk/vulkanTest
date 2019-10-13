module Data.Bits.Local (
  module Data.Bits,
  allAreSet,
  someAreSet,
  noneAreSet,
  setIf
) where

import Data.Bits

allAreSet :: Bits b => b -> b -> Bool
allAreSet bits = (bits ==) . (bits .&.)

someAreSet :: Bits b => b -> b -> Bool
someAreSet bits = (zeroBits /=) . (bits .&.)

noneAreSet :: Bits b => b -> b -> Bool
noneAreSet bits = (zeroBits ==) . (bits .&.)

setIf :: Bits b => Bool -> b -> b
setIf True b = b
setIf False _ = zeroBits
