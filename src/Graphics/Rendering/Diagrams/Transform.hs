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
       ( -- * Points

         Point, (.-.), (.+^)

         -- * Transformations

         -- ** Invertible linear transformations
         (:-:)(..), (<->), linv, lapp

         -- ** General transformations
       , Transformation(..)
       , inv, transp
       , apply
       , papply
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
--  Points  ------------------------------------------------
------------------------------------------------------------

-- | @Point@ is a newtype wrapper around vectors that we wish to treat
--   as points, so we don't get them mixed up.  Translations affect
--   points, but leave vectors unchanged.
newtype Point v = P v

-- | Form a vector from the difference of two points.
(.-.) :: VectorSpace v => Point v -> Point v -> v
P v1 .-. P v2 = v1 ^-^v2

-- | Add a point and a vector.
(.+^) :: VectorSpace v => Point v -> v -> Point v
P v1 .+^ v2 = P (v1 ^+^ v2)

------------------------------------------------------------
--  Transformations  ---------------------------------------
------------------------------------------------------------

-------------------------------------------------------
--  Invertible linear transformations  ----------------
-------------------------------------------------------

-- | @(v1 :-: v2)@ is a linear map paired with its inverse.
data (:-:) u v = (u :-* v) :-: (v :-* u)
infixr 7 :-:

-- | Create an invertible linear map from two functions which are
--   assumed to be linear inverses.
(<->) :: (HasLinearMap u, HasLinearMap v) => (u -> v) -> (v -> u) -> (u :-: v)
f <-> g = linear f :-: linear g

-- | Invertible linear maps from a vector space to itself form a
--   monoid under composition.
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
--  Affine transformations  ----------------------
--------------------------------------------------

-- | General (affine) transformations, represented by an invertible
-- linear map, its /transpose/, and a vector representing a
-- translation component.
data Transformation v = Transformation (v :-: v) (v :-: v) v

-- | Invert a transformation.
inv :: HasLinearMap v => Transformation v -> Transformation v
inv (Transformation t t' v) = Transformation (linv t) (linv t')
                                             (negateV (lapp (linv t) v))

-- | Get the transpose of a transformation (ignoring the translation
--   component).
transp :: Transformation v -> (v :-: v)
transp (Transformation _ t' _) = t'

-- | Transformations are closed under composition; @t1 <> t2@ is the
--   transformation which performs first @t2@, then @t1@.
instance HasLinearMap v => Monoid (Transformation v) where
  mempty = Transformation mempty mempty zeroV
  mappend (Transformation t1 t1' v1) (Transformation t2 t2' v2)
    = Transformation (t1 <> t2) (t2' <> t1') (v1 ^+^ lapp t1 v2)

-- | Apply a transformation to a vector.  Note that any translational
--   component of the transformation will not affect the vector, since
--   vectors are invariant under translation.
apply :: HasLinearMap v => Transformation v -> v -> v
apply (Transformation t _ _) = lapp t

-- | Apply a transformation to a point.
papply :: HasLinearMap v => Transformation v -> Point v -> Point v
papply (Transformation t _ v) (P p) = P $ lapp t p ^+^ v

-- | Create a general affine transformation from an invertible linear
--   transformation, its transpose, and a translation component.
fromLinear :: (v :-: v) -> (v :-: v) -> v -> Transformation v
fromLinear = Transformation

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
translation :: ( HasLinearMap v, HasLinearMap (Scalar v)
               , Scalar (Scalar v) ~ Scalar v
               , InnerSpace v, Num (Scalar v))
            => v -> Transformation v
translation v0 = fromLinear mempty mempty v0

-- | Translate by a vector.
translate :: ( Transformable t, v ~ TSpace t, Scalar (Scalar v) ~ Scalar v
             , InnerSpace v, Num (Scalar v) )
          => TSpace t -> t -> t
translate = transform . translation

-- | Create a scale transformation.
scaling :: (HasLinearMap v, HasLinearMap (Scalar v),
            Scalar (Scalar v) ~ Scalar v, Fractional (Scalar v))
        => Scalar v -> Transformation v
scaling s = fromLinear lin lin zeroV   -- scaling is its own transpose
  where lin = (s *^) <-> (^/ s)

-- | Scale uniformly in every dimension by the given scalar.
scale :: (Transformable t, Scalar (Scalar (TSpace t)) ~ Scalar (TSpace t),
          Fractional (Scalar (TSpace t)))
      => Scalar (TSpace t) -> t -> t
scale 0 = error "scale by zero!  Halp!"  -- XXX what should be done here?
scale s = transform $ scaling s
