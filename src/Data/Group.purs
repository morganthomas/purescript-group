module Data.Group where

import Data.Monoid (class Monoid)
import Data.Field (class Field, negate)
import Data.Monoid.Additive (Additive(..))

class Monoid g <= Group g where
  ginverse :: g -> g

instance fieldsAreAdditiveGroups :: (Field f) => Group (Additive f) where
  ginverse (Additive x) = Additive (negate x)
