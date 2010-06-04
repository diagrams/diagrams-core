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
         (:-:)(..), (<->), inv, apply

         -- ** Transformation transformations
       , Transformation(..)
       , pinv
       , papply
       , fromLinear
       , translation

         -- * The 'Transformable' class

       , HasLinearMap
       , Transformable(..)

         -- * Vector space independent transformations
         -- | Some transformations are specific to a particular vector
         --   space, but a few can be defined generically over any
         --   vector space.

       , translate
       , scale

       ) where

import Data.AdditiveGroup
import Data.VectorSpace
import Data.LinearMap
import Data.Basis
import Data.MemoTrie

import Data.Monoid

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
inv :: (u :-: v) -> (v :-: u)
inv (f :-: g) = (g :-: f)

-- | Apply a linear map to a vector.
apply :: (VectorSpace v, Scalar u ~ Scalar v, HasLinearMap u) => (u :-: v) -> u -> v
apply (f :-: _) = lapply f

--------------------------------------------------
--  Transformation transformations  ------------------
--------------------------------------------------

-- | A projective transformation is a linear transformation one
--   dimension up.  XXX write something better here.
newtype Transformation v = Transformation ((v, Scalar v) :-: (v, Scalar v))

-- | Invert a projective transformation.
pinv :: Transformation v -> Transformation v
pinv (Transformation t) = Transformation (inv t)

-- | Transformation transformations are closed under composition.
instance (HasLinearMap v, HasLinearMap (Scalar v)
         ,Scalar (Scalar v) ~ Scalar v)
    => Monoid (Transformation v) where
  mempty = Transformation mempty
  mappend (Transformation a2) (Transformation a1) = Transformation (a2 <> a1)

-- | XXX comment me
project :: (Fractional (Scalar v), VectorSpace v) => (v, Scalar v) -> v
project (v,c) = v ^/ c

-- | XXX comment me
papply :: (HasLinearMap v, HasLinearMap (Scalar v)
          ,Fractional (Scalar v), Scalar (Scalar v) ~ Scalar v)
          => Transformation v -> v -> v
papply (Transformation a) v = project $ apply a (v,1)

-- | Treat a linear transformation as a projective transformation.
fromLinear :: (HasLinearMap v, HasLinearMap (Scalar v)
                        ,Scalar (Scalar v) ~ Scalar v)
                        => (v :-: v) -> Transformation v
fromLinear t = Transformation $ (\(v,c) -> (apply t v, c)) <->
                            (\(v,c) -> (apply (inv t) v, c))


-- | Treat a translation as a projective transformation.
translation :: (HasLinearMap v, HasLinearMap (Scalar v)
                           ,Scalar (Scalar v) ~ Scalar v)
                          => v -> Transformation v
translation v0 = Transformation $ (\(v,c) -> (v ^+^ v0 ^* c,c)) <->
                              (\(v,c) -> (v ^-^ v0 ^* c,c))

------------------------------------------------------------
--  The 'Transformable' class  -----------------------------
------------------------------------------------------------

-- | 'HasLinearMap' is a poor man's class constraint synonym, just to
--   help shorten some of the ridiculously long constraint sets.
class (HasBasis v, HasTrie (Basis v), VectorSpace v) => HasLinearMap v
instance (HasBasis v, HasTrie (Basis v), VectorSpace v) => HasLinearMap v

-- | Type class for things which can be transformed by projective
--   transformations.
class (HasLinearMap (TSpace t), HasLinearMap (Scalar (TSpace t)))
    => Transformable t where
  type TSpace t :: *         -- Vector space of transformations
  transform :: Transformation (TSpace t) -> t -> t

-- | Translate by a vector.
translate :: (Transformable t, Scalar (Scalar (TSpace t)) ~ Scalar (TSpace t))
             => TSpace t -> t -> t
translate = transform . translation

-- | Scale uniformly in every dimension by the given scalar.
scale :: (Transformable t, Scalar (Scalar (TSpace t)) ~ Scalar (TSpace t)
         ,Fractional (Scalar (TSpace t)))
         => Scalar (TSpace t) -> t -> t
scale 0 = error "scale by zero!  Halp!"  -- XXX what should be done here?
scale s = transform . fromLinear $ (s *^) <-> (^/ s)

