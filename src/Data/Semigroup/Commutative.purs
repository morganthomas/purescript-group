-- Copyright 2017 rightfold
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

module Data.Semigroup.Commutative where

import Data.Monoid.Additive (Additive)
import Data.Monoid.Dual (Dual)
import Data.Monoid.Multiplicative (Multiplicative)
import Prelude

-- | A `Commutative` is a `Semigroup` with a commutative operation. Instances
-- | must satisfy the following law in addition to the semigroup laws:
-- |
-- | - Commutativity: `forall x, y. x <> y = y <> x`
class Semigroup g <= Commutative g

instance commutativeVoid :: Commutative Void

instance commutativeUnit :: Commutative Unit

instance commutativeDual :: (Commutative g) => Commutative (Dual g)

-- | Addition commutes for any `Semiring`
instance commutativeAdditive :: (Semiring r) => Commutative (Additive r)

-- | Multiplication commutes only for a `CommutativeRing`.
instance commutativeMultiplicative :: (CommutativeRing r) => Commutative (Multiplicative r)
