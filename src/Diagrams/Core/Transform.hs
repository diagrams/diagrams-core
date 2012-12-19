{-# LANGUAGE TypeOperators
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , GeneralizedNewtypeDeriving
           , TypeSynonymInstances
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Transform
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- "Diagrams" defines the core library of primitives
-- forming the basis of an embedded domain-specific language for
-- describing and rendering diagrams.
--
-- The @Transform@ module defines generic transformations
-- parameterized by any vector space.
--
-----------------------------------------------------------------------------

module Diagrams.Core.Transform
       (
         -- * Transformations

         -- ** Invertible linear transformations
         (:-:)(..), (<->), linv, lapp

         -- ** General transformations
       , Transformation(..)
       , inv, transp, transl
       , apply
       , papply
       , fromLinear

         -- * The Transformable class

       , HasLinearMap
       , Transformable(..)

         -- * Translational invariance

       , TransInv(..)

         -- * Vector space independent transformations
         -- | Most transformations are specific to a particular vector
         --   space, but a few can be defined generically over any
         --   vector space.

       , translation, translate
       , scaling, scale

       ) where

import qualified Data.Map as M
import           Data.Semigroup
import qualified Data.Set as S

import           Data.AdditiveGroup
import           Data.AffineSpace ((.-.))
import           Data.Basis
import           Data.LinearMap
import           Data.MemoTrie
import           Data.Monoid.Action
import           Data.Monoid.Deletable
import           Data.VectorSpace

import           Diagrams.Core.HasOrigin
import           Diagrams.Core.Points
import           Diagrams.Core.V

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

instance HasLinearMap v => Semigroup (v :-: v) where
  (f :-: f') <> (g :-: g') = f *.* g :-: g' *.* f'

-- | Invertible linear maps from a vector space to itself form a
--   monoid under composition.
instance HasLinearMap v => Monoid (v :-: v) where
  mempty = idL :-: idL
  mappend = (<>)

-- | Invert a linear map.
linv :: (u :-: v) -> (v :-: u)
linv (f :-: g) = g :-: f

-- | Apply a linear map to a vector.
lapp :: (VectorSpace v, Scalar u ~ Scalar v, HasLinearMap u) => (u :-: v) -> u -> v
lapp (f :-: _) = lapply f

--------------------------------------------------
--  Affine transformations  ----------------------
--------------------------------------------------

-- | General (affine) transformations, represented by an invertible
--   linear map, its /transpose/, and a vector representing a
--   translation component.
--
--   By the /transpose/ of a linear map we mean simply the linear map
--   corresponding to the transpose of the map's matrix
--   representation.  For example, any scale is its own transpose,
--   since scales are represented by matrices with zeros everywhere
--   except the diagonal.  The transpose of a rotation is the same as
--   its inverse.
--
--   The reason we need to keep track of transposes is because it
--   turns out that when transforming a shape according to some linear
--   map L, the shape's /normal vectors/ transform according to L's
--   inverse transpose.  This is exactly what we need when
--   transforming bounding functions, which are defined in terms of
--   /perpendicular/ (i.e. normal) hyperplanes.

data Transformation v = Transformation (v :-: v) (v :-: v) v

type instance V (Transformation v) = v

-- | Invert a transformation.
inv :: HasLinearMap v => Transformation v -> Transformation v
inv (Transformation t t' v) = Transformation (linv t) (linv t')
                                             (negateV (lapp (linv t) v))

-- | Get the transpose of a transformation (ignoring the translation
--   component).
transp :: Transformation v -> (v :-: v)
transp (Transformation _ t' _) = t'

-- | Get the translational component of a transformation.
transl :: Transformation v -> v
transl (Transformation _ _ v) = v

-- | Transformations are closed under composition; @t1 <> t2@ is the
--   transformation which performs first @t2@, then @t1@.
instance HasLinearMap v => Semigroup (Transformation v) where
  Transformation t1 t1' v1 <> Transformation t2 t2' v2
    = Transformation (t1 <> t2) (t2' <> t1') (v1 ^+^ lapp t1 v2)

instance HasLinearMap v => Monoid (Transformation v) where
  mempty = Transformation mempty mempty zeroV
  mappend = (<>)

-- | Transformations can act on transformable things.
instance (HasLinearMap v, v ~ (V a), Transformable a)
         => Action (Transformation v) a where
  act = transform

-- | Apply a transformation to a vector.  Note that any translational
--   component of the transformation will not affect the vector, since
--   vectors are invariant under translation.
apply :: HasLinearMap v => Transformation v -> v -> v
apply (Transformation t _ _) = lapp t

-- | Apply a transformation to a point.
papply :: HasLinearMap v => Transformation v -> Point v -> Point v
papply (Transformation t _ v) (P p) = P $ lapp t p ^+^ v

-- | Create a general affine transformation from an invertible linear
--   transformation and its transpose.  The translational component is
--   assumed to be zero.
fromLinear :: AdditiveGroup v => (v :-: v) -> (v :-: v) -> Transformation v
fromLinear l1 l2 = Transformation l1 l2 zeroV

------------------------------------------------------------
--  The Transformable class  -------------------------------
------------------------------------------------------------

-- | 'HasLinearMap' is a poor man's class constraint synonym, just to
--   help shorten some of the ridiculously long constraint sets.
class (HasBasis v, HasTrie (Basis v), VectorSpace v) => HasLinearMap v
instance (HasBasis v, HasTrie (Basis v), VectorSpace v) => HasLinearMap v

-- | Type class for things @t@ which can be transformed.
class HasLinearMap (V t) => Transformable t where

  -- | Apply a transformation to an object.
  transform :: Transformation (V t) -> t -> t

instance HasLinearMap v => Transformable (Transformation v) where
  transform t1 t2 = t1 <> t2

instance HasLinearMap v => HasOrigin (Transformation v) where
  moveOriginTo p = translate (origin .-. p)

instance (Transformable a, Transformable b, V a ~ V b)
      => Transformable (a,b) where
  transform t (x,y) =  ( transform t x
                       , transform t y
                       )

instance (Transformable a, Transformable b, Transformable c, V a ~ V b, V a ~ V c)
      => Transformable (a,b,c) where
  transform t (x,y,z) = ( transform t x
                        , transform t y
                        , transform t z
                        )

-- Transform functions by conjugation. That is, reverse-transform argument and
-- forward-transform result. Intuition: If someone shrinks you, you see your
-- environment enlarged. If you rotate right, you see your environment
-- rotating left. Etc. This technique was used extensively in Pan for modular
-- construction of image filters. Works well for curried functions, since all
-- arguments get inversely transformed.

instance ( HasBasis (V b), HasTrie (Basis (V b))
         , Transformable a, Transformable b, V b ~ V a) =>
         Transformable (a -> b) where
  transform tr f = transform tr . f . transform (inv tr)

instance Transformable t => Transformable [t] where
  transform = map . transform

instance (Transformable t, Ord t) => Transformable (S.Set t) where
  transform = S.map . transform

instance Transformable t => Transformable (M.Map k t) where
  transform = M.map . transform

instance HasLinearMap v => Transformable (Point v) where
  transform = papply

instance Transformable m => Transformable (Deletable m) where
  transform = fmap . transform

instance Transformable Double where
  transform = apply

instance Transformable Rational where
  transform = apply

------------------------------------------------------------
--  Translational invariance  ------------------------------
------------------------------------------------------------

-- | @TransInv@ is a wrapper which makes a transformable type
--   translationally invariant; the translational component of
--   transformations will no longer affect things wrapped in
--   @TransInv@.
newtype TransInv t = TransInv { unTransInv :: t }
  deriving (Show, Semigroup, Monoid)

type instance V (TransInv t) = V t

instance VectorSpace (V t) => HasOrigin (TransInv t) where
  moveOriginTo = const id

instance Transformable t => Transformable (TransInv t) where
  transform tr (TransInv t) = TransInv (translate (negateV (transl tr)) . transform tr $ t)

------------------------------------------------------------
--  Generic transformations  -------------------------------
------------------------------------------------------------

-- | Create a translation.
translation :: HasLinearMap v => v -> Transformation v
translation = Transformation mempty mempty

-- | Translate by a vector.
translate :: (Transformable t, HasLinearMap (V t)) => V t -> t -> t
translate = transform . translation

-- | Create a uniform scaling transformation.
scaling :: (HasLinearMap v, Fractional (Scalar v))
        => Scalar v -> Transformation v
scaling s = fromLinear lin lin      -- scaling is its own transpose
  where lin = (s *^) <-> (^/ s)

-- | Scale uniformly in every dimension by the given scalar.
scale :: (Transformable t, Fractional (Scalar (V t)), Eq (Scalar (V t)))
      => Scalar (V t) -> t -> t
scale 0 = error "scale by zero!  Halp!"  -- XXX what should be done here?
scale s = transform $ scaling s
