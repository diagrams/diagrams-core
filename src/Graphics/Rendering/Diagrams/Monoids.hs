{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
  #-}

-- XXX comment me
module Graphics.Rendering.Diagrams.Monoids where

import Graphics.Rendering.Diagrams.Util

import Data.Monoid
import Data.Foldable
import Control.Applicative

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

------------------------------------------------------------
--  Applicative monoids
------------------------------------------------------------

-- | A wrapper for an 'Applicative' structure containing a monoid,
--   which has a monoid structure itself based on application.
newtype AM f m = AM (f m)
  deriving (Functor, Applicative)

inAM2 :: (f m -> f m -> f m) -> (AM f m -> AM f m -> AM f m)
inAM2 g (AM f1) (AM f2) = AM (g f1 f2)

-- | @f1 `mappend` f2@ is defined as @mappend <$> f1 <*> f2@.
instance (Applicative f, Monoid m) => Monoid (AM f m) where
  mempty  = pure mempty
  mappend = inAM2 (liftA2 mappend)

instance (Action m n, Foldable f, Functor f, Monoid n) => Action (AM f m) n where
  act (AM f) n = fold $ fmap (flip act n) f

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