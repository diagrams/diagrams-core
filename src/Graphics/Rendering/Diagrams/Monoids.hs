{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , DeriveFunctor
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances
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
       ( -- * Monoids and semigroups

         Monoid'

         -- * Monoid actions

       , Action(..)

         -- * Split monoids
         -- $split

       , Split(..), split

         -- * Forgetful monoids
         -- $forget

       , Forgetful(..), unForget, forget

       , Deletable(..), unDelete, toDeletable, deleteL, deleteR

         -- * Applicative monoids

       , AM(..), inAM2

         -- * Coproduct monoid
       , (:+:)
       , inL, inR
       , mappendL, mappendR
       , killL, killR
       , untangle
       ) where

import Graphics.Rendering.Diagrams.V

import Data.Semigroup
import Data.Foldable
import Control.Applicative
import Data.Either (lefts, rights)

------------------------------------------------------------
--  Monoids and semigroups
------------------------------------------------------------

-- Poor man's constraint synonym.  Eventually, once it becomes
-- standard, we can make this a real constraint synonym and get rid of
-- the UndecidableInstances flag.  Better yet, hopefully the Monoid
-- class will eventually have a Semigroup superclass.

-- | The @Monoid'@ class is a synonym for things which are instances
--   of both 'Semigroup' and 'Monoid'.  Ideally, the 'Monoid' class
--   itself will eventually include a 'Semigroup' superclass and we
--   can get rid of this.
class (Semigroup m, Monoid m) => Monoid' m
instance (Semigroup m, Monoid m) => Monoid' m

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

-- | If @m@ is a @Semigroup@, then @Split m@ is a semigroup which
--   combines values on either side of a split, keeping only the
--   rightmost split.
instance Semigroup m => Semigroup (Split m) where
  (M m1)       <> (M m2)       = M (m1 <> m2)
  (M m1)       <> (m1' :| m2)  = m1 <> m1'         :| m2
  (m1  :| m2)  <> (M m2')      = m1                :| m2 <> m2'
  (m11 :| m12) <> (m21 :| m22) = m11 <> m12 <> m21 :| m22

instance (Semigroup m, Monoid m) => Monoid (Split m) where
  mempty  = M mempty
  mappend = (<>)

-- | A convenient name for @mempty :| mempty@, so @a \<\> split \<\> b == a :| b@.
split :: Monoid m => Split m
split = mempty :| mempty

-- | By default, the action of a split monoid is the same as for
--   the underlying monoid, as if the split were removed.
instance Action m n => Action (Split m) n where
  act (M m) n      = act m n
  act (m1 :| m2) n = act m1 (act m2 n)

------------------------------------------------------------
--  Forgetful monoids
------------------------------------------------------------

-- $forget
-- Sometimes we want to be able to \"forget\" some information.  We
-- define two monoid transformers that allow forgetting information.
-- @Forgetful@ introduces special values which cause anything to their
-- right to be forgotten.  @Deletable@ introduces special \"left and
-- right bracket\" elements which cause everything inside them to be
-- forgotten.


-- | A value of type @Forgetful m@ is either a \"normal\" value of
--   type @m@, which combines normally with other normal values, or a
--   \"forgetful\" value, which combines normally with other values to
--   its left but discards values combined on the right.  Also, when
--   combining a forgetful value with a normal one the result is
--   always forgetful.
data Forgetful m = Normal m
                 | Forgetful m
  deriving Functor

-- | Project the wrapped value out of a `Forgetful` value.
unForget :: Forgetful m -> m
unForget (Normal m)    = m
unForget (Forgetful m) = m

-- | If @m@ is a 'Semigroup', then @Forgetful m@ is a semigroup with two
--   sorts of values, \"normal\" and \"forgetful\": the normal ones
--   combine normally and the forgetful ones discard anything to the
--   right.
instance Semigroup m => Semigroup (Forgetful m) where
  (Normal m1)    <> (Normal m2)    = Normal (m1 <> m2)
  (Normal m1)    <> (Forgetful m2) = Forgetful (m1 <> m2)
  (Forgetful m1) <> _              = Forgetful m1

instance (Semigroup m, Monoid m) => Monoid (Forgetful m) where
  mempty  = Normal mempty
  mappend = (<>)


-- | A convenient name for @Forgetful mempty@, so @a \<\> forget \<\>
--   b == Forgetful a@.
forget :: Monoid m => Forgetful m
forget = Forgetful mempty

instance Action m n => Action (Forgetful m) n where
  act (Normal m) n    = act m n
  act (Forgetful m) n = act m n

type instance V (Forgetful m) = V m

-- | If @m@ is a 'Monoid', then @Deletable m@ (intuitively speaking)
--   adds two distinguished new elements @[@ and @]@, such that an
--   occurrence of [ \"deletes\" everything from it to the next ]. For
--   example,
--
--   > abc[def]gh == abcgh
--
--   This is all you really need to know to /use/ @Deletable m@
--   values; to understand the actual implementation, read on.
--
--   To properly deal with nesting and associativity we need to be
--   able to assign meanings to things like @[[@, @][@, and so on. (We
--   cannot just define, say, @[[ == [@, since then @([[)] == [] ==
--   id@ but @[([]) == [id == [@.)  Formally, elements of @Deletable
--   m@ are triples of the form (r, m, l) representing words @]^r m
--   [^l@.  When combining two triples (r1, m1, l1) and (r2, m2, l2)
--   there are three cases:
--
--   * If l1 == r2 then the [s from the left and ]s from the right
--     exactly cancel, and we are left with (r1, m1 \<\> m2, l2).
--
--   * If l1 < r2 then all of the [s cancel with some of the ]s, but
--     m1 is still inside the remaining ]s and is deleted, yielding (r1
--     + r2 - l1, m2, l2)
--
--   * The remaining case is symmetric with the second.

data Deletable m = Deletable Int m Int
  deriving Functor

type instance V (Deletable m) = V m

-- | Project the wrapped value out of a `Deletable` value.
unDelete :: Deletable m -> m
unDelete (Deletable _ m _) = m

-- | Inject a value into a `Deletable` wrapper.  Satisfies the
--   property
--
-- > unDelete . toDeletable === id
--
toDeletable :: m -> Deletable m
toDeletable m = Deletable 0 m 0

instance Semigroup m => Semigroup (Deletable m) where
  (Deletable r1 m1 l1) <> (Deletable r2 m2 l2)
    | l1 == r2  = Deletable r1 (m1 <> m2) l2
    | l1 <  r2  = Deletable (r1 + r2 - l1) m2 l2
    | otherwise = Deletable r1 m1 (l2 + l1 - r2)

instance (Semigroup m, Monoid m) => Monoid (Deletable m) where
  mempty = Deletable 0 mempty 0
  mappend = (<>)

-- | A \"left bracket\", which causes everything between it and the
--   next right bracket to be deleted.
deleteL :: Monoid m => Deletable m
deleteL = Deletable 0 mempty 1

-- | A \"right bracket\", denoting the end of the section that should
--   be deleted.
deleteR :: Monoid m => Deletable m
deleteR = Deletable 1 mempty 0

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
inAM2 :: (f m -> f m -> f m) -> AM f m -> AM f m -> AM f m
inAM2 g (AM f1) (AM f2) = AM (g f1 f2)

instance (Applicative f, Semigroup m) => Semigroup (AM f m) where
  (<>) = inAM2 (liftA2 (<>))

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
  act (AM f) n = fold $ fmap (`act` n) f

-- XXX need to prove that this satisfies the laws!  There are other
-- "obvious" instances too.

------------------------------------------------------------
-- Monoid coproduct
------------------------------------------------------------

-- | @m :+: n@ is the coproduct of monoids @m@ and @n@.  Values of
--   type @m :+: n@ consist of alternating lists of @m@ and @n@
--   values.  The empty list is the identity, and composition is list
--   concatenation, with appropriate combining of adjacent elements
--   when possible.
newtype m :+: n = MCo { unMCo :: [Either m n] }

-- For efficiency and simplicity, we implement it just as [Either m
-- n]: of course, this does not preserve the invariant of strictly
-- alternating types, but it doesn't really matter as long as we don't
-- let anyone inspect the internal representation.

-- | Injection from the left monoid into a coproduct.
inL :: m -> m :+: n
inL m = MCo [Left m]

-- | Injection from the right monoid into a coproduct.
inR :: n -> m :+: n
inR n = MCo [Right n]

-- | Prepend a value from the left monoid.
mappendL :: m -> m :+: n -> m :+: n
mappendL = mappend . inL

-- | Prepend a value from the right monoid.
mappendR :: n -> m :+: n -> m :+: n
mappendR = mappend . inR

{-
normalize :: (Monoid m, Monoid n) => m :+: n -> m :+: n
normalize (MCo es) = MCo (normalize' es)
  where normalize' []  = []
        normalize' [e] = [e]
        normalize' (Left e1:Left e2 : es) = normalize' (Left (e1 <> e2) : es)
        normalize' (Left e1:es) = Left e1 : normalize' es
        normalize' (Right e1:Right e2:es) = normalize' (Right (e1 <> e2) : es)
        normalize' (Right e1:es) = Right e1 : normalize' es
-}

instance Semigroup (m :+: n) where
  (MCo es1) <> (MCo es2) = MCo (es1 ++ es2)

-- | The coproduct of two monoids is itself a monoid.
instance Monoid (m :+: n) where
  mempty = MCo []
  mappend = (<>)

-- | @killR@ takes a value in a coproduct monoid and sends all the
--   values from the right monoid to the identity.
killR :: Monoid m => m :+: n -> m
killR = mconcat . lefts . unMCo

-- | @killL@ takes a value in a coproduct monoid and sends all the
--   values from the left monoid to the identity.
killL :: Monoid n => m :+: n -> n
killL = mconcat . rights . unMCo

-- | Take a value from a coproduct monoid where the left monoid has an
--   action on the right, and \"untangle\" it into a pair of values.  In
--   particular,
--
-- > m1 <> n1 <> m2 <> n2 <> m3 <> n3 <> ...
--
--   is sent to
--
-- > (m1 <> m2 <> m3 <> ..., (act m1 n1) <> (act (m1 <> m2) n2) <> (act (m1 <> m2 <> m3) n3) <> ...)
--
--   That is, before combining @n@ values, every @n@ value is acted on
--   by all the @m@ values to its left.
untangle :: (Action m n, Monoid m, Monoid n) => m :+: n -> (m,n)
untangle (MCo elts) = untangle' mempty elts
  where untangle' cur [] = cur
        untangle' (curM, curN) (Left m : elts')  = untangle' (curM `mappend` m, curN) elts'
        untangle' (curM, curN) (Right n : elts') = untangle' (curM, curN `mappend` act curM n) elts'

-- | Coproducts act on other things by having each of the components
--   act individually.
instance (Action m r, Action n r) => Action (m :+: n) r where
  act = appEndo . mconcat . map Endo . map (either act act) . unMCo