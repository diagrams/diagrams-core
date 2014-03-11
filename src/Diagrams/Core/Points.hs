{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

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

       ) where

-- We import from Data.AffineSpace.Point (defined in the
-- vector-space-points package) and re-export.  We also define an
-- instance of V for Point here.

import Control.Lens (Iso', iso)

import Data.AffineSpace.Point
import Data.AffineSpace

import Diagrams.Core.V

type instance V (Point v) = v

-- | An isomorphism between points and vectors, given a reference
-- point.  This is provided for defining new lenses on points.
_relative :: AffineSpace (Point v) => Point v -> Iso' (Point v) v
_relative p0 = iso (.-. p0) (p0 .+^)
