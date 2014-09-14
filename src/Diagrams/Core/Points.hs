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

       , relative, reflectThrough, mirror
       , relative2, relative3
       ) where

import           Control.Lens    (Iso', iso)

import           Data.Data
import           Linear
import           Linear.Affine

import           Diagrams.Core.V

deriving instance Typeable Point

type instance V (Point v n) = v
type instance N (Point v n) = n

mirror :: (Additive v, Num n) => Point v n -> Point v n
mirror = reflectThrough origin

-- | Apply a transformation relative to the given point.
relative :: (Affine p, Num n) => p n -> (Diff p n -> Diff p n) -> p n -> p n
relative p f = (p .+^) . f . (.-. p)

-- | Scale a point by a scalar.
(*.) :: (Functor f, Num n) => n -> Point f n -> Point f n
s *. P v = P (s *^ v)

-- -- | Apply a transformation relative to the given point.
relative2 :: (Affine p, Num n) => p n -> (Diff p n -> Diff p n -> Diff p n) -> p n -> p n -> p n
relative2 p f x y = (p .+^) $ f (inj x) (inj y) where inj = (.-. p)

-- | Apply a transformation relative to the given point.
relative3 :: (Affine p, Num n)
  => p n -> (Diff p n -> Diff p n -> Diff p n -> Diff p n)
  -> p n -> p n -> p n -> p n
relative3 p f x y z = (p .+^) $ f (inj x) (inj y) (inj z) where inj = (.-. p)

-- | Mirror a point through a given point.
reflectThrough :: (Affine p, Num n) => p n -> p n -> p n
reflectThrough o = relative o negated

-- | An isomorphism between points and vectors, given a reference
--   point.  This is provided for defining new lenses on points.
_relative :: (Additive v, Num n) => Point v n -> Iso' (Point v n) (v n)
_relative p0 = iso (.-. p0) (p0 .+^)

