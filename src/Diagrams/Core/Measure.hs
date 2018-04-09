{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
  -- for Data.Semigroup import, which becomes redundant under GHC 8.4

module Diagrams.Core.Measure
  ( Measured (..)
  , Measure
  , fromMeasured
  , output
  , local
  , global
  , normalized
  , normalised
  , scaleLocal
  , atLeast
  , atMost
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Control.Monad.Reader as R
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Semigroup
import           Data.Typeable

import           Diagrams.Core.V

import           Linear.Vector

-- | 'Measured n a' is an object that depends on 'local', 'normalized'
--   and 'global' scales. The 'normalized' and 'global' scales are
--   calculated when rendering a diagram.
--
--   For attributes, the 'local' scale gets multiplied by the average
--   scale of the transform.
newtype Measured n a = Measured { unmeasure :: (n,n,n) -> a }
  deriving (Typeable, Functor, Applicative, Monad, Additive, R.MonadReader (n,n,n))
-- (local, global, normalized) -> output

type instance V (Measured n a) = V a
type instance N (Measured n a) = N a

-- | A measure is a 'Measured' number.
type Measure n = Measured n n

-- | @fromMeasured globalScale normalizedScale measure -> a@
fromMeasured :: Num n => n -> n -> Measured n a -> a
fromMeasured g n (Measured m) = m (1,g,n)

-- | Output units don't change.
output :: n -> Measure n
output = pure

-- | Local units are scaled by the average scale of a transform.
local :: Num n => n -> Measure n
local x = views _1 (*x)

-- | Global units are scaled so that they are interpreted relative to
--   the size of the final rendered diagram.
global :: Num n => n -> Measure n
global x = views _2 (*x)

-- | Normalized units get scaled so that one normalized unit is the size of the
--   final diagram.
normalized :: Num n => n -> Measure n
normalized x = views _3 (*x)

-- | Just like 'normalized' but spelt properly.
normalised :: Num n => n -> Measure n
normalised x = views _3 (*x)

-- | Scale the local units of a 'Measured' thing.
scaleLocal :: Num n => n -> Measured n a -> Measured n a
scaleLocal s = R.local (_1 *~ s)

-- | Calculate the smaller of two measures.
atLeast :: Ord n => Measure n -> Measure n -> Measure n
atLeast = liftA2 max

-- | Calculate the larger of two measures.
atMost :: Ord n => Measure n -> Measure n -> Measure n
atMost = liftA2 min

instance Num a => Num (Measured n a) where
  (+) = (^+^)
  (-) = (^-^)
  (*) = liftA2 (*)

  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance Fractional a => Fractional (Measured n a) where
  (/)   = liftA2 (/)
  recip = fmap recip

  fromRational = pure . fromRational

instance Floating a => Floating (Measured n a) where
  pi      = pure pi
  exp     = fmap exp
  sqrt    = fmap sqrt
  log     = fmap log
  (**)    = liftA2 (**)
  logBase = liftA2 logBase
  sin     = fmap sin
  tan     = fmap tan
  cos     = fmap cos
  asin    = fmap asin
  atan    = fmap atan
  acos    = fmap acos
  sinh    = fmap sinh
  tanh    = fmap tanh
  cosh    = fmap cosh
  asinh   = fmap asinh
  atanh   = fmap atanh
  acosh   = fmap acosh

instance Semigroup a => Semigroup (Measured n a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Measured n a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

instance Distributive (Measured n) where
  distribute a = Measured $ \x -> fmap (\(Measured m) -> m x) a

instance Representable (Measured n) where
  type Rep (Measured n) = (n,n,n)
  tabulate = Measured
  index    = unmeasure

instance Profunctor Measured where
  lmap f (Measured m) = Measured $ \(l,g,n) -> m (f l, f g, f n)
  rmap f (Measured m) = Measured $ f . m

