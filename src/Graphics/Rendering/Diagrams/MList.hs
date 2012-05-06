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

         (:::), (*:)

       , MList(..)

         -- * Accessing embedded values
       , (:>:)(..)

         -- * Monoid actions of heterogeneous lists

         -- $mlist-actions

       , SM(..)
       ) where

import Control.Arrow
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
infixr 5 *:

type a ::: l = (Option a, l)

(*:) :: a -> l -> a ::: l
a *: l = (Option (Just a), l)

-- MList -----------------------------------

-- | Type class for heterogeneous monoidal lists, with a single method
--   allowing construction of an empty list.
class MList l where
  -- | The /empty/ heterogeneous list of type @l@. Of course, @empty
  -- == 'mempty'@, but unlike 'mempty', @empty@ does not require
  -- 'Monoid' constraints on all the elements of @l@.
  empty   :: l

instance MList () where
  empty     = ()

instance MList l => MList (a ::: l) where
  empty   = (Option Nothing, empty)

-- Embedding -------------------------------------------

-- | The relation @l :>: a@ holds when @a@ is the type of an element
--   in @l@.  For example,  @(Char ::: Int ::: Bool ::: Nil) :>: Int@.
class l :>: a where
  -- | Inject a value into an otherwise empty heterogeneous list.
  inj  :: a -> l

  -- | Get the value of type @a@ from a heterogeneous list, if there
  --   is one.
  get  :: l -> Option a

  -- | Alter the value of type @a@ by applying the given function to it.
  alt  :: (Option a -> Option a) -> l -> l

instance MList t => (:>:) (a ::: t) a where
  inj a = (Option (Just a), empty)
  get   = fst
  alt   = first

instance (t :>: a) => (:>:) (b ::: t) a where
  inj a = (Option Nothing, inj a)
  get   = get . snd
  alt   = second . alt

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

instance Action () l where
  act _ a = a

instance (Action (SM a) l2, Action l1 l2) => Action (a, l1) l2 where
  act (a,l) = act (SM a) . act l

instance Action (SM a) () where
  act _ _ = ()

instance (Action a a', Action (SM a) l) => Action (SM a) (Option a', l) where
  act (SM a) (Option Nothing,   l) = (Option Nothing, act (SM a) l)
  act (SM a) (Option (Just a'), l) = (Option (Just (act a a')), act (SM a) l)
