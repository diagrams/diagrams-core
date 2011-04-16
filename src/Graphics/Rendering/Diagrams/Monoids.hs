{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
  #-}

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
--   * @act mempty = id@
--
--   * @act (m1 `mappend` m2) = act m1 . act m2@
--
--   Additionally, if the type @s@ has any algebraic structure, @act
--   m@ should be a homomorphism.  For example, if @s@ is also a
--   monoid we should have @act m mempty = mempty@ and @act m (s1
--   `mappend` s2) = (act m s1) `mappend` (act m s2)@.
--
--   By default, @act = const id@, so for monoidal types @M@ which
--   should have no effect on other types, it suffices to write
--
--   > instance Action M m
--
class Action m s where
  act :: m -> s -> s
  act = const id

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

-- | By default, the action of a split monoid are just the same as for
--   the underlying monoid.
instance (Action m n) => Action (Split m) n where
  act (M m) n      = act m n
  act (m1 :| m2) n = act m1 (act m2 n)