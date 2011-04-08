module Graphics.Rendering.Diagrams.SplitMonoid where

import Graphics.Rendering.Diagrams.Util
import Data.Monoid

infix 5 :|

-- | A value of type @Split m@ is either a single @m@, or a pair of
--   @m@'s separated by a divider.
data Split m = M m
             | m :| m

-- | If @m@ is a @Monoid@, then @Split m@ is a monoid which combines
--   values on either side of a split, keeping only the rightmost
--   split.
instance Monoid m => Monoid (Split m) where
  mempty = M mempty

  (M m1)       `mappend` (M m2)       = M (m1 <> m2)
  (M m1)       `mappend` (m1' :| m2)  = m1 <> m1'         :| m2
  (m1  :| m2)  `mappend` (M m2')      = m1                :| m2 <> m2'
  (m11 :| m12) `mappend` (m21 :| m22) = m11 <> m12 <> m21 :| m22