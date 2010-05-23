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
       ( -- * Affine transformations

         Affine(..)
       , papply
       , fromLinear
       , fromTranslate

         -- * The 'Transformable' class

       , HasLinearMap
       , Transformable(..)

         -- * Vector space independent transformations
         -- $
         -- Some transformations are specific to a particular vector
         -- space, but a few can be defined generically over any
         -- vector space.

       , translate
       , scale

       ) where

import Data.AdditiveGroup
import Data.VectorSpace
import Data.LinearMap
import Data.Basis
import Data.MemoTrie

import Data.Monoid

-- for convenience
class (HasBasis v, HasTrie (Basis v), VectorSpace v) => HasLinearMap v
instance (HasBasis v, HasTrie (Basis v), VectorSpace v) => HasLinearMap v

-- | A linear map (conveniently paired with its inverse).
data (:-:) u v = (u :-* v) :-: (v :-* u)
infixr 7 :-:

instance HasLinearMap v => Monoid (v :-: v) where
  mempty = idL :-: idL
  (f :-: f') `mappend` (g :-: g') = (f *.* g :-: g' *.* f')

-- | Invert a linear map
inv :: (u :-: v) -> (v :-: u)
inv (f :-: g) = (g :-: f)

apply :: (VectorSpace v, Scalar u ~ Scalar v, HasLinearMap u) => (u :-: v) -> u -> v
apply (f :-: _) = lapply f

-- | An affine transformation consists of a linear transformation
--   followed by a translation.

-- Right now, we have projective versions of everything.
-- We may want to go back through and use the old names.

data Affine v = Affine { getLinear    :: v :-* v
                       , getTranslate :: v
                       }

newtype Projective v = Projective ((v, Scalar v) :-* (v, Scalar v))

-- | Affine transformations are closed under composition.

instance (HasLinearMap v, VectorSpace v)
         => Monoid (Affine v) where
  mempty  = Affine idL zeroV
  mappend (Affine a2 b2) (Affine a1 b1) =
    Affine (a2 *.* a1) (lapply a2 b1 ^+^ b2)

instance (HasLinearMap v, HasLinearMap (Scalar v)
         ,Scalar (Scalar v) ~ Scalar v)
    => Monoid (Projective v) where
  mempty  = Projective idL
  mappend (Projective a2) (Projective a1) = Projective (a2 *.* a1)

-- | Apply an affine transformation.

aapply :: (HasLinearMap v) => Affine v -> v -> v
aapply (Affine a b) v = lapply a v ^+^ b

project :: (Fractional (Scalar v), VectorSpace v) => (v, Scalar v) -> v
project (v,c) = v ^/ c

papply :: (HasLinearMap v, HasLinearMap (Scalar v)
          ,Fractional (Scalar v), Scalar (Scalar v) ~ Scalar v)
          => Projective v -> v -> v
papply (Projective a) v = project $ lapply a (v,1)

-- | Treat a linear transformation as an affine transformation.

{-
fromLinear :: AdditiveGroup v => (v :-* v) -> Affine v
fromLinear t = Affine t zeroV

projectiveFromLinear :: (HasLinearMap v, HasLinearMap (Scalar v)
                        ,Scalar (Scalar v) ~ Scalar v)
                        => (v :-* v) -> Projective v
projectiveFromLinear t = Projective $ linear $ \(v,c) -> (lapply t v,c)
-}

fromLinear :: (HasLinearMap v, HasLinearMap (Scalar v)
                        ,Scalar (Scalar v) ~ Scalar v)
                        => (v :-* v) -> Projective v
fromLinear t = Projective $ linear $ \(v,c) -> (lapply t v,c)

-- | Treat a translation as an affine transformation.

{-
fromTranslate :: (HasLinearMap v) => v -> Affine v
fromTranslate = Affine idL

projectiveFromTranslate :: (HasLinearMap v, HasLinearMap (Scalar v)
                           ,Scalar (Scalar v) ~ Scalar v)
                          => v -> Projective v
projectiveFromTranslate v0 = Projective $ linear $ \(v,c) -> (v ^+^ v0,c)
-}

fromTranslate :: (HasLinearMap v, HasLinearMap (Scalar v)
                           ,Scalar (Scalar v) ~ Scalar v)
                          => v -> Projective v
fromTranslate v0 = Projective $ linear $ \(v,c) -> (v ^+^ v0 ^* c,c)

-- | Type class for things which can be transformed by affine
--   transformations.

{-
class (HasLinearMap (TSpace t)) => Transformable t where
  type TSpace t :: *         -- Vector space of transformations
  transform :: Affine (TSpace t) -> t -> t

class (HasLinearMap (PSpace t),HasLinearMap (Scalar (PSpace t)))
    => Projectable t where
  type PSpace t :: *         -- Vector space of transformations
  projTransform :: Projective (PSpace t) -> t -> t
-}

class (HasLinearMap (TSpace t),HasLinearMap (Scalar (TSpace t)))
    => Transformable t where
  type TSpace t :: *         -- Vector space of transformations
  transform :: Projective (TSpace t) -> t -> t

-- | Translate by a vector.

{-
translate :: Transformable t => TSpace t -> t -> t
translate = transform . fromTranslate

projTranslate :: (Projectable t, Scalar (Scalar (PSpace t)) ~ Scalar (PSpace t))
                 => PSpace t -> t -> t
projTranslate = projTransform . projectiveFromTranslate
-}

translate :: (Transformable t, Scalar (Scalar (TSpace t)) ~ Scalar (TSpace t))
             => TSpace t -> t -> t
translate = transform . fromTranslate

-- | Scale uniformly in every dimension by the given scalar.

{-
scale :: Transformable t => Scalar (TSpace t) -> t -> t
scale = transform . fromLinear . linear . (*^)

projScale :: (Projectable t, Scalar (Scalar (PSpace t)) ~ Scalar (PSpace t))
             => Scalar (PSpace t) -> t -> t
projScale = projTransform . projectiveFromLinear . linear . (*^)
-}

scale :: (Transformable t, Scalar (Scalar (TSpace t)) ~ Scalar (TSpace t))
         => Scalar (TSpace t) -> t -> t
scale = transform . fromLinear . linear . (*^)

