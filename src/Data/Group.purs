module Data.Group where

import Data.Monoid (class Monoid)
import Data.Ring (class Ring, negate)
import Data.Monoid.Additive (Additive(..))

-- | A `Group` is a `Monoid` with inverses. Instances
-- | must satisfy the following law in addition to the monoid laws:
-- |
-- | ```text
-- | forall x. ginverse x <> x = mempty = x <> ginverse x
-- | ```
class Monoid g <= Group g where
  ginverse :: g -> g

-- | A `CommutativeGroup` is a `Monoid` with a commutative monoid operation.
-- | Instances must satisfy the following law in addition to the group laws:
-- |
-- | ```text
-- | forall x, y. x <> y = y <> x
-- | ```
class Group g <= CommutativeGroup g

instance ringsAreAdditiveGroups :: (Ring r) => Group (Additive r) where
  ginverse (Additive x) = Additive (negate x)

instance ringsAreCommutativeAdditiveGroups :: (Ring r) => CommutativeGroup (Additive r)
