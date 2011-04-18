{-# LANGUAGE TypeFamilies
           , DeriveFunctor
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

------------------------------------------------------------
--  Points  ------------------------------------------------
------------------------------------------------------------

-- | @Point@ is a newtype wrapper around vectors that we wish to treat
--   as points, so we don't get them mixed up. The distinction is
--   important: translations affect points, but leave vectors
--   unchanged.  Points are instances of the 'AffineSpace' class from
--   "Data.AffineSpace".
newtype Point v = P v
  deriving (Eq, Ord, Read, Show, Functor)

type instance V (Point v) = v

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
