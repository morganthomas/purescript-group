module Data.Group where

import Prelude

import Data.Monoid as M
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Dual (Dual(..))
import Data.Semigroup.Commutative (class Commutative)

-- | A `Group` is a `Monoid` with inverses. Instances
-- | must satisfy the following law in addition to the monoid laws:
-- |
-- | - Inverse: `forall x. ginverse x <> x = mempty = x <> ginverse x`
class M.Monoid g <= Group g where
  ginverse :: g -> g

instance groupUnit :: Group Unit where
  ginverse _ = unit

instance groupDual :: (Group g) => Group (Dual g) where
  ginverse (Dual x) = Dual (ginverse x)

instance groupAdditive :: (Ring r) => Group (Additive r) where
  ginverse (Additive x) = Additive (negate x)

-- | An Abelian group is a group with a commutative operation.
type Abelian a b = Group a => Commutative a => b

-- | Append a value (or its inverse) to itself a certain number of times.
-- |
-- | For the `Additive Int` type, this is the same as multiplication.
power :: forall g. Group g => g -> Int -> g
power x p
  | p < 0     = M.power (ginverse x) $ negate p
  | otherwise = M.power x p
