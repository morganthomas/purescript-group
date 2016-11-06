module Data.Group where

import Data.Monoid (class Monoid)
import Data.Ring (class Ring, negate)
import Data.Monoid.Additive (Additive(..))

class Monoid g <= Group g where
  ginverse :: g -> g

class Group g <= CommutativeGroup g

instance ringsAreAdditiveGroups :: (Ring r) => Group (Additive r) where
  ginverse (Additive x) = Additive (negate x)

instance ringsAreCommutativeAdditiveGroups :: (Ring r) => CommutativeGroup (Additive r)
