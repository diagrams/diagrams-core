{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Points
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A type for /points/ (as distinct from vectors).
--
-----------------------------------------------------------------------------

module Diagrams.Core.Points
       ( -- * Points

         Point(..), origin, (*.)
       , _relative

       -- vector-space-points
         , relative, reflectThrough, mirror
         , relative2, relative3
       ) where

-- We import from Data.AffineSpace.Point (defined in the
-- vector-space-points package) and re-export.  We also define an
-- instance of V for Point here.

import Control.Lens (Iso', iso)

-- import Data.AffineSpace.Point
-- import Data.AffineSpace

import Data.Data
import Linear
import Linear.Affine

import Diagrams.Core.V

deriving instance Typeable Point
deriving instance (Typeable v, Typeable n, Data (v n)) => Data (Point v n)

type instance V (Point v a) = v
type instance N (Point v a) = a

mirror :: (Additive v, Num a) => Point v a -> Point v a
mirror = reflectThrough origin

-- | Apply a transformation relative to the given point.
relative :: (Num a, Affine p) => p a -> (Diff p a -> Diff p a) -> p a -> p a
relative p f = (p .+^) . f . (.-. p)

-- | Scale a point by a scalar.
(*.) :: (Functor f, Num a) => a -> Point f a -> Point f a
s *. P v = P (s *^ v)

-- -- | Apply a transformation relative to the given point.
relative2 :: (Num a, Affine p) => p a -> (Diff p a -> Diff p a -> Diff p a) -> p a -> p a -> p a
relative2 p f x y = (p .+^) $ f (inj x) (inj y) where inj = (.-. p)

-- | Apply a transformation relative to the given point.
relative3 :: (Num a, Affine p)
  => p a -> (Diff p a -> Diff p a -> Diff p a -> Diff p a)
  -> p a -> p a -> p a -> p a
relative3 p f x y z = (p .+^) $ f (inj x) (inj y) (inj z) where inj = (.-. p)

-- | Mirror a point through a given point.
reflectThrough :: (Num a, Affine p) => p a -> p a -> p a
reflectThrough o = relative o negated

-- | An isomorphism between points and vectors, given a reference
-- point.  This is provided for defining new lenses on points.
_relative :: (Additive v, Num n) => Point v n -> Iso' (Point v n) (v n)
_relative p0 = iso (.-. p0) (p0 .+^)

