-- Copyright 2016 Morgan Thomas
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--    http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Data.Group where

import Data.Monoid (class Monoid)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Semigroup.Commutative (class Commutative)
import Prelude

-- | A `Group` is a `Monoid` with inverses. Instances
-- | must satisfy the following law in addition to the monoid laws:
-- |
-- | - Inverse: `forall x. ginverse x <> x = mempty = x <> ginverse x`
class Monoid g <= Group g where
  ginverse :: g -> g

instance groupUnit :: Group Unit where
  ginverse _ = unit

instance groupDual :: (Group g) => Group (Dual g) where
  ginverse (Dual x) = Dual (ginverse x)

instance groupAdditive :: (Ring r) => Group (Additive r) where
  ginverse (Additive x) = Additive (negate x)

instance groupMultiplicative :: (DivisionRing r) => Group (Multiplicative r) where
  ginverse (Multiplicative x) = Multiplicative (recip x)

-- | An Abelian group is a group with a commutative operation.
type Abelian a b = Group a => Commutative a => b
