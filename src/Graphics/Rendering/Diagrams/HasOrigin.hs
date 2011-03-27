{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.HasOrigin
-- Copyright   :  (c) Brent Yorgey 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
--
-- Types which have an intrinsic notion of a \"local origin\",
-- i.e. things which are NOT invariant under translation.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.HasOrigin
       ( HasOrigin(..), moveOriginBy
       ) where

import Graphics.Rendering.Diagrams.Points

import Data.AdditiveGroup (AdditiveGroup)
import Data.AffineSpace ((.-^))

-- | Class of types which have an intrinsic notion of a \"local
--   origin\", i.e. things which are not invariant under translation,
--   and which allow the origin to be moved.
class HasOrigin t where
  type OriginSpace t :: *

  -- | Move the local origin to another point.
  moveOriginTo :: Point (OriginSpace t) -> t -> t

-- | Move the local origin by a relative vector.
moveOriginBy :: HasOrigin t => OriginSpace t -> t -> t
moveOriginBy = moveOriginTo . P

instance AdditiveGroup v => HasOrigin (Point v) where
  type OriginSpace (Point v) = v
  moveOriginTo (P u) p = p .-^ u