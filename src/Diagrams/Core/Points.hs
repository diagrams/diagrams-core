{-# LANGUAGE TypeFamilies
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

-- We just import from Data.AffineSpace.Point (defined in the
-- vector-space-points package) and re-export.  We also define an
-- instance of V for Point here.
import Data.AffineSpace.Point

import Graphics.Rendering.Diagrams.V

type instance V (Point v) = v