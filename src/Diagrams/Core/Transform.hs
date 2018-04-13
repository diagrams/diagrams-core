{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-unused-imports       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Transform
-- Copyright   :  (c) 2011-2015 diagrams-core team (see LICENSE)
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
       , dropTransl
       , apply
       , papply
       , fromLinear
       , fromOrthogonal
       , fromSymmetric
       , basis
       , dimension
       , onBasis
       , listRep
       , matrixRep
       , matrixHomRep
       , determinant
       , isReflection
       , avgScale
       , eye

         -- * The Transformable class

       , HasLinearMap
       , HasBasis
       , Transformable(..)

         -- * Translational invariance

       , TransInv(TransInv)

         -- * Vector space independent transformations
         -- | Most transformations are specific to a particular vector
         --   space, but a few can be defined generically over any
         --   vector space.

       , translation, translate
       , scaling, scale

       ) where

import           Control.Lens            (Rewrapped, Traversable, Wrapped (..),
                                          iso, (&), (.~))
import qualified Data.Map                as M
import           Data.Semigroup
import qualified Data.Set                as S

import           Data.Monoid.Action
import           Data.Monoid.Deletable

import           Linear.Affine
import           Linear.Vector

import           Data.Foldable           (Foldable, toList)
import           Data.Functor.Rep

import           Diagrams.Core.HasOrigin
import           Diagrams.Core.Measure
import           Diagrams.Core.Points    ()
import           Diagrams.Core.V

------------------------------------------------------------
--  Transformations  ---------------------------------------
------------------------------------------------------------

-------------------------------------------------------
--  Invertible linear transformations  ----------------
-------------------------------------------------------

-- | @(v1 :-: v2)@ is a linear map paired with its inverse.
data (:-:) u v = (u -> v) :-: (v -> u)
infixr 7 :-:

-- | Create an invertible linear map from two functions which are
--   assumed to be linear inverses.
(<->) :: (u -> v) -> (v -> u) -> (u :-: v)
f <-> g = f :-: g

instance Semigroup (a :-: a) where
  (f :-: f') <> (g :-: g') = f . g :-: g' . f'

-- | Invertible linear maps from a vector space to itself form a
--   monoid under composition.
instance Monoid (v :-: v) where
  mempty  = id :-: id
  mappend = (<>)

-- | Invert a linear map.
linv :: (u :-: v) -> (v :-: u)
linv (f :-: g) = g :-: f

-- | Apply a linear map to a vector.
lapp :: (u :-: v) -> u -> v
lapp (f :-: _) = f

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
--   inverse transpose.  (For a more detailed explanation and proof,
--   see <https://wiki.haskell.org/Diagrams/Dev/Transformations>.)
--   This is exactly what we need when transforming bounding
--   functions, which are defined in terms of /perpendicular/
--   (i.e. normal) hyperplanes.
--
--   For more general, non-invertible transformations, see
--   @Diagrams.Deform@ (in @diagrams-lib@).

data Transformation v n = Transformation (v n :-: v n) (v n :-: v n) (v n)

type instance V (Transformation v n) = v
type instance N (Transformation v n) = n

-- | Identity matrix.
eye :: (HasBasis v, Num n) => v (v n)
eye = tabulate $ \(E e) -> zero & e .~ 1

-- | Invert a transformation.
inv :: (Functor v, Num n) => Transformation v n -> Transformation v n
inv (Transformation t t' v) = Transformation (linv t) (linv t')
                                             (negated (lapp (linv t) v))

-- | Get the transpose of a transformation (ignoring the translation
--   component).
transp :: Transformation v n -> (v n :-: v n)
transp (Transformation _ t' _) = t'

-- | Get the translational component of a transformation.
transl :: Transformation v n -> v n
transl (Transformation _ _ v) = v

-- | Drop the translational component of a transformation, leaving only
--   the linear part.
dropTransl :: (Additive v, Num n) => Transformation v n -> Transformation v n
dropTransl (Transformation a a' _) = Transformation a a' zero

-- | Transformations are closed under composition; @t1 <> t2@ is the
--   transformation which performs first @t2@, then @t1@.
instance (Additive v, Num n) => Semigroup (Transformation v n) where
  Transformation t1 t1' v1 <> Transformation t2 t2' v2
    = Transformation (t1 <> t2) (t2' <> t1') (v1 ^+^ lapp t1 v2)

instance (Additive v, Num n) => Monoid (Transformation v n) where
  mempty = Transformation mempty mempty zero
  mappend = (<>)

-- | Transformations can act on transformable things.
instance (Transformable a, V a ~ v, N a ~ n) => Action (Transformation v n) a where
  act = transform

-- | Apply a transformation to a vector.  Note that any translational
--   component of the transformation will not affect the vector, since
--   vectors are invariant under translation.
apply :: Transformation v n -> v n -> v n
apply (Transformation (t :-: _) _ _) = t

-- | Apply a transformation to a point.
papply :: (Additive v, Num n) => Transformation v n -> Point v n -> Point v n
papply (Transformation t _ v) (P p) = P $ lapp t p ^+^ v

-- | Create a general affine transformation from an invertible linear
--   transformation and its transpose.  The translational component is
--   assumed to be zero.
fromLinear :: (Additive v, Num n) => (v n :-: v n) -> (v n :-: v n) -> Transformation v n
fromLinear l1 l2 = Transformation l1 l2 zero

-- | An orthogonal linear map is one whose inverse is also its transpose.
fromOrthogonal :: (Additive v, Num n) => (v n :-: v n) -> Transformation v n
fromOrthogonal t = fromLinear t (linv t)

-- | A symmetric linear map is one whose transpose is equal to its self.
fromSymmetric :: (Additive v, Num n) => (v n :-: v n) -> Transformation v n
fromSymmetric t = fromLinear t t

-- | Get the dimension of an object whose vector space is an instance of
--   @HasLinearMap@, e.g. transformations, paths, diagrams, etc.
dimension :: forall a. (Additive (V a), Traversable (V a)) => a -> Int
dimension _ = length (basis :: [V a Int])

-- | Get the matrix equivalent of the linear transform,
--   (as a list of columns) and the translation vector.  This
--   is mostly useful for implementing backends.
onBasis :: (Additive v, Traversable v, Num n) => Transformation v n -> ([v n], v n)
onBasis (Transformation (f :-: _) _ t) = (map f basis, t)

-- Remove the nth element from a list
remove :: Int -> [a] -> [a]
remove n xs = ys ++ tail zs
  where
    (ys, zs) = splitAt n xs

-- Minor matrix of cofactore C(i,j)
minor :: Int -> Int -> [[a]] -> [[a]]
minor i j xs = remove j $ map (remove i) xs

-- The determinant of a square matrix represented as a list of lists
-- representing column vectors, that is [column].
det :: Num a => [[a]] -> a
det (a:[]) = head a
det m = sum [(-1)^i * (c1 !! i) * det (minor i 0 m) | i <- [0 .. (n-1)]]
  where
    c1 = head m
    n = length m

-- | Convert a vector v to a list of scalars.
listRep :: Foldable v => v n -> [n]
listRep = toList

-- | Convert the linear part of a `Transformation` to a matrix
--   representation as a list of column vectors which are also lists.
matrixRep :: (Additive v, Traversable v, Num n) => Transformation v n -> [[n]]
matrixRep (Transformation (f :-: _) _ _) = map (toList . f) basis

-- | Convert a `Transformation v` to a homogeneous matrix representation.
--   The final list is the translation.
--   The representation leaves off the last row of the matrix as it is
--   always [0,0, ... 1] and this representation is the defacto standard
--   for backends.
matrixHomRep :: (Additive v, Traversable v, Num n) => Transformation v n -> [[n]]
matrixHomRep t = mr ++ [toList tl]
  where
    mr = matrixRep t
    tl = transl t

-- | The determinant of (the linear part of) a `Transformation`.
determinant :: (Additive v, Traversable v, Num n) => Transformation v n -> n
determinant = det . matrixRep

-- | Determine whether a `Transformation` includes a reflection
--   component, that is, whether it reverses orientation.
isReflection :: (Additive v, Traversable v, Num n, Ord n) => Transformation v n -> Bool
isReflection = (<0) . determinant

-- | Compute the \"average\" amount of scaling performed by a
--   transformation.  Satisfies the properties
--
--   @
--   avgScale (scaling k) == k
--   avgScale (t1 <> t2)  == avgScale t1 * avgScale t2
--   @
--
avgScale :: (Additive v, Traversable v, Floating n) => Transformation v n -> n
avgScale t = (abs . determinant) t ** (recip . fromIntegral . dimension) t

{-

avgScale is computed as the nth root of the positive determinant.
This works because the determinant is the factor by which a transformation
scales area/volume. See http://en.wikipedia.org/wiki/Determinant.

Proofs for the specified properties:

1. |det (scaling k)|^(1/n) = (k^n)^(1/n) = k
2. |det t1|^(1/n) * |det t2|^(1/n)
   = (|det t1| * |det t2|)^(1/n)
   = |det t1 * det t2|^(1/n)
   = |det (t1 <> t2)|^(1/n)

-}

------------------------------------------------------------
--  The Transformable class  -------------------------------
------------------------------------------------------------

-- | 'HasLinearMap' is a constraint synonym, just to
--   help shorten some of the ridiculously long constraint sets.
type HasLinearMap v = (HasBasis v, Traversable v)

-- | An 'Additive' vector space whose representation is made up of basis elements.
type HasBasis v = (Additive v, Representable v, Rep v ~ E v)

-- | Type class for things @t@ which can be transformed.
class Transformable t where

  -- | Apply a transformation to an object.
  transform :: Transformation (V t) (N t) -> t -> t

instance (Additive v, Num n) => Transformable (Transformation v n) where
  transform t1 t2 = t1 <> t2

instance (Additive v, Num n) => HasOrigin (Transformation v n) where
  moveOriginTo p = translate (origin .-. p)

instance (Transformable t, Transformable s, V t ~ V s, N t ~ N s)
      => Transformable (t, s) where
  transform t (x,y) =  ( transform t x
                       , transform t y
                       )

instance (Transformable t, Transformable s, Transformable u, V s ~ V t, N s ~ N t, V s ~ V u, N s ~ N u)
      => Transformable (t,s,u) where
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

instance ( V t ~ v, N t ~ n, V t ~ V s, N t ~ N s, Functor v, Num n
         , Transformable t, Transformable s)
         => Transformable (s -> t) where
  transform tr f = transform tr . f . transform (inv tr)

instance Transformable t => Transformable [t] where
  transform = map . transform

instance (Transformable t, Ord t) => Transformable (S.Set t) where
  transform = S.map . transform

instance Transformable t => Transformable (M.Map k t) where
  transform = M.map . transform

instance (Additive v, Num n) => Transformable (Point v n) where
  transform = papply

instance Transformable m => Transformable (Deletable m) where
  transform = fmap . transform

------------------------------------------------------------
--  Translational invariance  ------------------------------
------------------------------------------------------------

-- | @TransInv@ is a wrapper which makes a transformable type
--   translationally invariant; the translational component of
--   transformations will no longer affect things wrapped in
--   @TransInv@.
newtype TransInv t = TransInv t
  deriving (Eq, Ord, Show, Semigroup, Monoid)

instance Wrapped (TransInv t) where
  type Unwrapped (TransInv t) = t
  _Wrapped' = iso (\(TransInv t) -> t) TransInv

instance Rewrapped (TransInv t) (TransInv t')

type instance V (TransInv t) = V t
type instance N (TransInv t) = N t

instance HasOrigin (TransInv t) where
  moveOriginTo = const id

instance (Num (N t), Additive (V t), Transformable t) => Transformable (TransInv t) where
  transform (Transformation a a' _) (TransInv t)
    = TransInv (transform (Transformation a a' zero) t)

instance (InSpace v n t, Transformable t, HasLinearMap v, Floating n)
    => Transformable (Measured n t) where
  transform t = scaleLocal n . fmap (transform t')
    where
      t' = t <> scaling (1 / avgScale t)
      n = avgScale t

------------------------------------------------------------
--  Generic transformations  -------------------------------
------------------------------------------------------------

-- | Create a translation.
translation :: v n -> Transformation v n
translation = Transformation mempty mempty

-- | Translate by a vector.
translate :: (Transformable t) => Vn t -> t -> t
translate = transform . translation

-- | Create a uniform scaling transformation.
scaling :: (Additive v, Fractional n) => n -> Transformation v n
scaling s = fromSymmetric lin
  where lin = (s *^) <-> (^/ s)

-- | Scale uniformly in every dimension by the given scalar.
scale :: (InSpace v n a, Eq n, Fractional n, Transformable a)
      => n -> a -> a
scale 0 = error "scale by zero!  Halp!"  -- XXX what should be done here?
scale s = transform $ scaling s

