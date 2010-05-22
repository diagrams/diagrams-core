{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeFamilies #-}

-- XXX comment me!

module Graphics.Rendering.Diagrams.Transform where

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

translate :: Transformable t => TSpace t -> t -> t
translate = transform . fromTranslate

scale :: Transformable t => Scalar (TSpace t) -> t -> t
scale = transform . fromLinear . linear . (*^)