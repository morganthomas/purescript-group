module Data.Group.Action
  ( class LeftAction
  , class RightAction
  , lact
  , ract
  , (<+)
  , (+>)
  ) where

import Data.Group (class Group)

-- | A group `g` acting on a space `s`. Instances must satisfy the following
-- | laws in addition to the `Group` laws:
-- |
-- | - Identity: `forall s. lact mempty s = s`
-- | - Compatibility: `forall g h s. lact (g <> h) s = lact g (lact h s)`
class Group g <= LeftAction g s where
  lact :: g -> s -> s

-- | A group `g` acting on a space `s`. Instances must satisfy the following
-- | laws in addition to the `Group` laws:
-- |
-- | - Identity: `forall s. ract s mempty = s`
-- | - Compatibility: `forall g h s. ract s (g <> h) = ract (ract s g) h`
class Group g <= RightAction g s where
  ract :: s -> g -> s

infixl 6 lact as <+
infixl 6 ract as +>
