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
       ( HasOrigin(..)
       ) where

import Graphics.Rendering.Diagrams.Points

import Data.AdditiveGroup (AdditiveGroup)
import Data.AffineSpace ((.-^))

class HasOrigin t where
  type OriginSpace t :: *

  moveOriginTo :: Point (OriginSpace t) -> t -> t

instance AdditiveGroup v => HasOrigin (Point v) where
  type OriginSpace (Point v) = v
  moveOriginTo (P u) p = p .-^ u