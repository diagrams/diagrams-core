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
       , aapply
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
class (HasBasis v, HasTrie (Basis v)) => HasLinearMap v
instance (HasBasis v, HasTrie (Basis v)) => HasLinearMap v

-- | An affine transformation consists of a linear transformation
--   followed by a translation.

data Affine v = Affine { getLinear    :: v :-* v
                       , getTranslate :: v
                       }

-- | Affine transformations are closed under composition.

instance (HasLinearMap v, VectorSpace v)
         => Monoid (Affine v) where
  mempty  = Affine idL zeroV
  mappend (Affine a2 b2) (Affine a1 b1) =
    Affine (a2 *.* a1) (lapply a2 b1 ^+^ b2)

-- | Apply an affine transformation.

aapply :: (HasLinearMap v) => Affine v -> v -> v
aapply (Affine a b) v = lapply a v ^+^ b

-- | Treat a linear transformation as an affine transformation.

fromLinear :: AdditiveGroup v => (v :-* v) -> Affine v
fromLinear t = Affine t zeroV

-- | Treat a translation as an affine transformation.

fromTranslate :: (HasLinearMap v) => v -> Affine v
fromTranslate = Affine idL

-- | Type class for things which can be transformed by affine
--   transformations.

class (HasLinearMap (TSpace t)) => Transformable t where
  type TSpace t :: *         -- Vector space of transformations
  transform :: Affine (TSpace t) -> t -> t

-- | Translate by a vector.

translate :: Transformable t => TSpace t -> t -> t
translate = transform . fromTranslate

-- | Scale uniformly in every dimension by the given scalar.

scale :: Transformable t => Scalar (TSpace t) -> t -> t
scale = transform . fromLinear . linear . (*^)