{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Monoids
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Various monoid-related definitions (monoid actions, split monoids,
-- applicative monoids) used in the core diagrams library.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Monoids
       ( -- * Monoid actions

         Action(..)

         -- * Split monoids
         -- $split

       , Split(..), split

         -- * Applicative monoids

       , AM(..), inAM2

       ) where

import Graphics.Rendering.Diagrams.Util

import Data.Monoid
import Data.Foldable
import Control.Applicative

------------------------------------------------------------
--  Monoid actions
------------------------------------------------------------

-- | Type class for monoid actions, where monoidal values of type @m@
--   \"act\" on values of another type @s@.  Instances are required to
--   satisfy the laws
--
--   * @act mempty = id@
--
--   * @act (m1 ``mappend`` m2) = act m1 . act m2@
--
--   Additionally, if the type @s@ has any algebraic structure, @act
--   m@ should be a homomorphism.  For example, if @s@ is also a
--   monoid we should have @act m mempty = mempty@ and @act m (s1
--   ``mappend`` s2) = (act m s1) ``mappend`` (act m s2)@.
--
--   By default, @act = const id@, so for a monoidal type @M@ which
--   should have no action on anything, it suffices to write
--
--   > instance Action M s
--
--   with no method implementations.
class Action m s where

  -- | Convert a monoidal value of type @m@ to an action on @s@ values.
  act :: m -> s -> s
  act = const id

------------------------------------------------------------
--  Split monoids
------------------------------------------------------------

-- $split
-- Sometimes we want to accumulate values from some monoid, but have
-- the ability to introduce a \"split\" which separates values on
-- either side.  For example, this is used when accumulating
-- transformations to be applied to primitive diagrams: the 'freeze'
-- operation introduces a split, since only transformations occurring
-- outside the freeze should be applied to attributes.

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

-- | A convenient name for @mempty :| mempty@, so @a \<\> split \<\> b == a :| b@.
split :: Monoid m => Split m
split = mempty :| mempty

-- | By default, the action of a split monoid is the same as for
--   the underlying monoid, as if the split were removed.
instance (Action m n) => Action (Split m) n where
  act (M m) n      = act m n
  act (m1 :| m2) n = act m1 (act m2 n)

------------------------------------------------------------
--  Applicative monoids
------------------------------------------------------------

-- | A wrapper for an 'Applicative' structure containing a monoid.
--   Such structures have a @Monoid@ instance based on \"idiomatic\"
--   application of 'mappend' within the @Applicative@ context.
--   @instance Monoid m => Monoid (e -> m)@ is one well-known special
--   case.  (However, the standard @Monoid@ instance for @Maybe@ is
--   /not/ an instance of this pattern; nor is the standard instance
--   for lists.)
newtype AM f m = AM (f m)
  deriving (Functor, Applicative)

-- | Apply a binary function inside an 'AM' newtype wrapper.
inAM2 :: (f m -> f m -> f m) -> (AM f m -> AM f m -> AM f m)
inAM2 g (AM f1) (AM f2) = AM (g f1 f2)

-- | @f1 ``mappend`` f2@ is defined as @'mappend' '<$>' f1 '<*>' f2@.
instance (Applicative f, Monoid m) => Monoid (AM f m) where
  mempty  = pure mempty
  mappend = inAM2 (liftA2 mappend)

{- See Applicative laws here:

http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#t:Applicative
-}

{- left identity:

  AM (pure mempty) `mappend` AM f
=           { definition }
  AM $ fmap mappend (pure mempty) <*> f
=           { naturality of pure, fmap f . pure = pure . f }
  AM $ pure (mappend mempty) <*> f
=           { monoid law (left identity) }
  AM $ pure id <*> f
=           { applicative law (identity) }
  AM f
-}

{- right identity:

  AM f `mappend` AM (pure mempty)
=           { definition }
  AM $ fmap mappend f <*> pure mempty
=           { applicative law (interchange) }
  AM $ pure ($mempty) <*> fmap mappend f
=           { applicative/functor law }
  AM $ pure ($mempty) <*> (pure mappend <*> f)
=           { applicative law (composition) }
  AM $ pure (.) <*> pure ($mempty) <*> pure mappend <*> f
=           { applicative law (homomorphism) }
  AM $ pure ((.) ($mempty)) <*> pure mappend <*> f
=           { applicative law (homomorphism) }
  AM $ pure (($mempty) . mappend) <*> f
=           { monoid law (right identity) }
  AM $ pure id <*> f
=           { applicative law (identity) }
  AM f
-}

{- associativity:

  (AM f1 `mappend` AM f2) `mappend` AM f3
=           { definition }
  AM $ fmap mappend (AM f1 `mappend` AM f2) <*> f3
=           { definition }
  AM $ fmap mappend (fmap mappend f1 <*> f2) <*> f3
=           { applicative/functor law }
  AM $ pure mappend <*> (pure mappend <*> f1 <*> f2) <*> f3
=           { applicative law (composition) }
  AM $ pure (.) <*> pure mappend <*> (pure mappend <*> f1) <*> f2 <*> f3
=           { applicative law (homomorphism) }
  AM $ pure (mappend .) <*> (pure mappend <*> f1) <*> f2 <*> f3
=           { applicative law (composition) }
  AM $ pure (.) <*> pure (mappend .) <*> pure mappend <*> f1 <*> f2 <*> f3
=           { applicative law (homomorphism) }
  AM $ pure ((mappend .) . mappend) <*> f1 <*> f2 <*> f3
=           { monoid law (associativity) }
  AM $ pure ((. mappend) . (.) . mappend) <*> f1 <*> f2 <*> f3
=
  -- XXX finish this proof (although I have no doubt it goes through)


=
  AM f1 `mappend` (AM f2 `mappend` AM f3)
-}

{-
\x y z -> (x `mappend` y) `mappend` z
\x y -> mappend (mappend x y)
\x -> mappend . (mappend x)
(mappend .) . mappend
-}

{-
\x y z -> x `mappend` (y `mappend` z)
\x y z -> mappend x (mappend y z)
\x y -> mappend x . mappend y
\x -> ((.) (mappend x)) . mappend
\x -> (.) ((.) (mappend x)) mappend
\x -> (.mappend) ((.) (mappend x))
(. mappend) . (.) . mappend
-}


-- | An applicative monoid acts on a value of a monoidal type by
--   having each element in the structure act on the value
--   independently, and then folding the resulting structure.
instance (Action m n, Foldable f, Functor f, Monoid n) => Action (AM f m) n where
  act (AM f) n = fold $ fmap (flip act n) f

-- XXX need to prove that this satisfies the laws!  There are other
-- "obvious" instances too.