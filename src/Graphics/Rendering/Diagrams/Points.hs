{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , DeriveFunctor
           , DeriveDataTypeable
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Points
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A type for /points/ (as distinct from vectors).
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Points
       ( -- * Points

         Point(..), origin, (*.)

       ) where

import Data.VectorSpace
import qualified Data.AffineSpace as AS

import Graphics.Rendering.Diagrams.V

type instance V (Point v) = v

instance Newtype (Point v) v where
  pack = P
  unpack (P v) = v

-- | The origin of the vector space @v@.
origin :: AdditiveGroup v => Point v
origin = P zeroV

instance AdditiveGroup v => AS.AffineSpace (Point v) where
  type AS.Diff (Point v) = v
  P v1 .-. P v2 = v1 ^-^ v2
  P v1 .+^ v2   = P (v1 ^+^ v2)

-- | Scale a point by a scalar.
(*.) :: VectorSpace v => Scalar v -> Point v -> Point v
s *. P v = P (s *^ v)
