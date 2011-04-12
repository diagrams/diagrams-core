{-# LANGUAGE MultiParamTypeClasses #-}

-- XXX comment me
module Graphics.Rendering.Diagrams.Monoids where

import Graphics.Rendering.Diagrams.Util
import Data.Monoid

------------------------------------------------------------
--  Monoid actions
------------------------------------------------------------

-- | Type class for monoid actions. Instances are required to satisfy
--   the laws
--
--   * @apply mempty = id@
--
--   * @apply (m1 `mappend` m2) = apply m1 . apply m2@
--
--   Additionally, if the type @s@ has any algebraic structure, @apply
--   m@ should be a homomorphism.  For example, if @s@ is also a
--   monoid we should have @apply m mempty = mempty@ and @apply m (s1
--   `mappend` s2) = (apply m s1) `mappend` (apply m s2)@.
class Monoid m => Action m s where
  apply :: m -> s -> s

------------------------------------------------------------
--  Split monoids
------------------------------------------------------------

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

split :: Monoid m => Split m
split = mempty :| mempty