{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Transform
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Graphics.Rendering.Diagrams defines the core library of primitives
-- forming the basis of an embedded domain-specific language for
-- describing and rendering diagrams.
--
-- The Transform module defines some generic transformations
-- parameterized by any vector space.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Transform
       ( -- * Transformations

         -- ** Invertible linear transformations
         (:-:)(..), (<->), linv, lapp

         -- ** General transformations
       , Transformation(..)
       , inv
       , apply
       , fromLinear

         -- * The 'Transformable' class

       , HasLinearMap
       , Transformable(..)

         -- * Vector space independent transformations
         -- | Some transformations are specific to a particular vector
         --   space, but a few can be defined generically over any
         --   vector space.

       , translation, translate
       , scaling, scale

       ) where

import Data.AdditiveGroup
import Data.VectorSpace
import Data.LinearMap
import Data.Basis
import Data.MemoTrie

import Data.Monoid
import qualified Data.Map as M

import Graphics.Rendering.Diagrams.Expressions

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

------------------------------------------------------------
--  Transformations  ---------------------------------------
------------------------------------------------------------

--------------------------------------------------
--  Invertible linear transformations  -----------
--------------------------------------------------

-- | @(u :-: v)@ is an invertible linear map.
data (:-:) u v = (u :-* v) :-: (v :-* u)
infixr 7 :-:

-- | Create an invertible linear map from two functions which are
--   assumed to be linear inverses.
(<->) :: (HasLinearMap u, HasLinearMap v) => (u -> v) -> (v -> u) -> (u :-: v)
f <-> g = linear f :-: linear g

-- | Linear maps from a vector space to itself form a monoid under
--   composition.
instance HasLinearMap v => Monoid (v :-: v) where
  mempty = idL :-: idL
  (f :-: f') `mappend` (g :-: g') = (f *.* g :-: g' *.* f')

-- | Invert a linear map.
linv :: (u :-: v) -> (v :-: u)
linv (f :-: g) = (g :-: f)

-- | Apply a linear map to a vector.
lapp :: (VectorSpace v, Scalar u ~ Scalar v, HasLinearMap u) => (u :-: v) -> u -> v
lapp (f :-: _) = lapply f

--------------------------------------------------
--  Projective transformations  ------------------
--------------------------------------------------

-- | General transformations.  XXX more documentation...
newtype Transformation v = Transformation ((v, Scalar v) :-: (v, Scalar v))

-- Note that transformations are implemented as invertible linear maps
-- in projective space.

-- | Invert a transformation.
inv :: Transformation v -> Transformation v
inv (Transformation t) = Transformation (linv t)

-- | Transformations are closed under composition.
instance (HasLinearMap v, HasLinearMap (Scalar v)
         ,Scalar (Scalar v) ~ Scalar v)
    => Monoid (Transformation v) where
  mempty = Transformation mempty
  mappend (Transformation a2) (Transformation a1) = Transformation (a2 <> a1)

-- | Map from projective space back down to the normal vector space,
--   by dividing through by the extra scalar component.
project :: (Fractional (Scalar v), VectorSpace v) => (v, Scalar v) -> v
project (v,c) = v ^/ c

-- | Apply a transformation to a vector.
apply :: (HasLinearMap v, HasLinearMap (Scalar v),
          Fractional (Scalar v), Scalar (Scalar v) ~ Scalar v)
      => Transformation v -> v -> v
apply (Transformation a) v = project $ lapp a (v,1)

-- | Create a general transformation from an invertible linear
--   transformation.
fromLinear :: (HasLinearMap v, HasLinearMap (Scalar v),
               Scalar (Scalar v) ~ Scalar v)
           => (v :-: v) -> Transformation v
fromLinear t = Transformation $ (\(v,c) -> (lapp t v, c)) <->
                                (\(v,c) -> (lapp (linv t) v, c))

------------------------------------------------------------
--  The 'Transformable' class  -----------------------------
------------------------------------------------------------

-- | 'HasLinearMap' is a poor man's class constraint synonym, just to
--   help shorten some of the ridiculously long constraint sets.
class (HasBasis v, HasTrie (Basis v), VectorSpace v) => HasLinearMap v
instance (HasBasis v, HasTrie (Basis v), VectorSpace v) => HasLinearMap v

-- | Type class for things which can be transformed.
class (HasLinearMap (TSpace t), HasLinearMap (Scalar (TSpace t)))
    => Transformable t where
  type TSpace t :: *         -- Vector space of transformations
  transform :: Transformation (TSpace t) -> t -> t

instance Transformable t => Transformable [t] where
  type TSpace [t] = TSpace t
  transform t = map (transform t)

instance (HasLinearMap v, HasLinearMap (Scalar v), Transformable v)
    => Transformable (NameSet v) where
  type TSpace (NameSet v) = TSpace v
  transform t (NameSet ns) = NameSet $ M.map (map (transform t)) ns

-- | It's useful to have the contravariant function instance by
--   default; covariant instances (with @v@ in a positive position) can
--   be written on a case-by-case basis.
instance Transformable v => Transformable (v -> a) where
  type TSpace (v -> a) = TSpace v
  transform t f = f . transform (inv t)

-- | Create a translation.
translation :: (HasLinearMap v, HasLinearMap (Scalar v),
                Scalar (Scalar v) ~ Scalar v)
            => v -> Transformation v
translation v0 = Transformation $ (\(v,c) -> (v ^+^ v0 ^* c,c)) <->
                                  (\(v,c) -> (v ^-^ v0 ^* c,c))

-- | Translate by a vector.
translate :: (Transformable t, Scalar (Scalar (TSpace t)) ~ Scalar (TSpace t))
          => TSpace t -> t -> t
translate = transform . translation

-- | Create a scale transformation.
scaling :: (HasLinearMap v, HasLinearMap (Scalar v),
            Scalar (Scalar v) ~ Scalar v, Fractional (Scalar v))
        => Scalar v -> Transformation v
scaling s = fromLinear $ (s *^) <-> (^/ s)

-- | Scale uniformly in every dimension by the given scalar.
scale :: (Transformable t, Scalar (Scalar (TSpace t)) ~ Scalar (TSpace t),
          Fractional (Scalar (TSpace t)))
      => Scalar (TSpace t) -> t -> t
scale 0 = error "scale by zero!  Halp!"  -- XXX what should be done here?
scale s = transform $ scaling s
