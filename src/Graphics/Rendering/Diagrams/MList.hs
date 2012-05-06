{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverlappingInstances
           , UndecidableInstances
           , TypeFamilies
           , GeneralizedNewtypeDeriving
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.MList
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Heterogeneous lists of monoids.
--
-----------------------------------------------------------------------------
module Graphics.Rendering.Diagrams.MList
       ( -- * Heterogeneous monoidal lists

         -- $mlist

         Nil(..), (:::)(..)

       , MList(..)

         -- * Converting to tuples
       , Tuple, ToTuple(..)

         -- * Accessing embedded values
       , (:>:)(..)

         -- * Monoid actions of heterogeneous lists

         -- $mlist-actions

       , SM(..)
       ) where

import Data.Semigroup
import Graphics.Rendering.Diagrams.Monoids

-- $mlist
--
-- The idea of /heterogeneous lists/ has been around for a long time.
-- Here, we adopt heterogeneous lists where the element types are all
-- monoids: this allows us to leave out identity values, so that a
-- heterogeneous list containing only a single non-identity value can
-- be created without incurring constraints due to all the other
-- types, by leaving all the other values out.

infixr 5 :::

-- | The empty heterogeneous list.
data Nil     = Nil
  deriving (Show, Eq, Ord)

-- | Cons for heterogeneous lists.
data a ::: l = Missing l -- ^ The @a@ value is missing, and should be
                         --   construed as 'mempty'.
             | a ::: l   -- ^ An @a@ value followed by a heterogeneous
                         --   list @l@.
  deriving (Show, Eq, Ord)

-- MList -----------------------------------

-- | Type class for heterogeneous monoidal lists, with a single method
--   allowing construction of an empty list.
class MList l where
  -- | The /empty/ heterogeneous list of type @l@. Of course, @empty
  -- == 'mempty'@, but unlike 'mempty', @empty@ does not require
  -- 'Monoid' constraints on all the elements of @l@.
  empty   :: l

instance MList Nil where
  empty     = Nil

instance MList l => MList (a ::: l) where
  empty   = Missing empty

-- Monoid ----------------------------------

instance Semigroup Nil where
  _ <> _ = Nil

instance Monoid Nil where
  mempty  = Nil
  mappend = (<>)

instance (Semigroup a, Semigroup tl) => Semigroup (a ::: tl) where
  (Missing t1) <> (Missing t2) = Missing (t1 <> t2)
  (Missing t1) <> (a2 ::: t2)  = a2 ::: (t1 <> t2)
  (a1 ::: t1)  <> (Missing t2) = a1 ::: (t1 <> t2)
  (a1 ::: t1)  <> (a2 ::: t2)  = (a1 <> a2) ::: (t1 <> t2)

-- | Heterogeneous monoidal lists are themselves instances of 'Monoid'
--   as long as all their elements are instances of 'Semigroup'.
instance (Semigroup a, Semigroup tl, MList tl) => Monoid (a ::: tl) where
  mempty  = Missing empty
  mappend = (<>)

-- ToTuple ---------------------------------

-- | A type function to compute the tuple-based representation for
--   instances of 'MList'.
type family Tuple l :: *
type instance Tuple Nil       = ()
type instance Tuple (a ::: b) = (a, Tuple b)

-- | @toTuple@ can be used to convert a heterogeneous list to its
--   tuple-based representation.
class ToTuple l where
  toTuple :: l -> Tuple l

instance ToTuple Nil where
  toTuple _ = ()

instance (Monoid a, ToTuple l) => ToTuple (a ::: l) where
  toTuple (Missing l) = (mempty, toTuple l)
  toTuple (a ::: l)   = (a, toTuple l)

-- Embedding -------------------------------------------

-- | The relation @l :>: a@ holds when @a@ is the type of an element
--   in @l@.  For example,  @(Char ::: Int ::: Bool ::: Nil) :>: Int@.
class l :>: a where
  -- | Inject a value into an otherwise empty heterogeneous list.
  inj  :: a -> l

  -- | Get the value of type @a@ from a heterogeneous list.
  get  :: l -> a

  -- | Alter the value of type @a@ by applying the given function to it.
  alt  :: (a -> a) -> l -> l

instance (MList t, Monoid a) => (:>:) (a ::: t) a where
  inj a                = a ::: empty
  get (Missing _)      = mempty
  get (a ::: _)        = a
  alt f (Missing l)    = f mempty ::: l
  alt f (a ::: l)      = f a ::: l

instance (t :>: a) => (:>:) (b ::: t) a where
  inj a                = Missing (inj a)
  get (Missing l)      = get l
  get (_ ::: l)        = get l
  alt f (Missing l)    = Missing (alt f l)
  alt f (a ::: l)      = a ::: alt f l

-- Monoid actions -----------------------------------------

-- $mlist-actions
-- Monoidal heterogeneous lists may act on one another as you would
-- expect, with each element in the first list acting on each in the
-- second.  Unfortunately, coding this up in type class instances is a
-- bit fiddly.

-- | @SM@, an abbreviation for \"single monoid\" (as opposed to a
--   heterogeneous list of monoids), is only used internally to help
--   guide instance selection when defining the action of
--   heterogeneous monoidal lists on each other.
newtype SM m = SM m

instance Action Nil l where
  act _ a = a

instance (Action (SM a) l2, Action l1 l2) => Action (a ::: l1) l2 where
  act (Missing l1) l2 = act l1 l2
  act (a ::: l1) l2   = act (SM a) (act l1 l2)

instance Action (SM a) Nil where
  act _ _ = Nil

instance (Action a a', Action (SM a) l) => Action (SM a) (a' ::: l) where
  act (SM a) (Missing l) = Missing (act (SM a) l)
  act (SM a) (a' ::: l)  = act a a' ::: act (SM a) l
