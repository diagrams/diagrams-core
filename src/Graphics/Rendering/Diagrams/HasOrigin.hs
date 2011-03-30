{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.HasOrigin
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Types which have an intrinsic notion of a \"local origin\",
-- i.e. things which are NOT invariant under translation.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.HasOrigin
       ( HasOrigin(..), moveOriginBy, moveTo
       ) where

import Graphics.Rendering.Diagrams.Points

import Data.AffineSpace ((.-^))
import Data.VectorSpace

-- | Class of types which have an intrinsic notion of a \"local
--   origin\", i.e. things which are not invariant under translation,
--   and which allow the origin to be moved.
class VectorSpace v => HasOrigin t v | t -> v where

  -- | Move the local origin to another point.
  moveOriginTo :: Point v -> t -> t

-- | Move the local origin by a relative vector.
moveOriginBy :: HasOrigin t v => v -> t -> t
moveOriginBy = moveOriginTo . P

-- | Apply the translation that sends the origin to the given point.
--   Note that this is dual to 'moveOriginTo'.  In 'moveOriginTo' we
--   think of fixing the diagram and moving the origin.  In 'moveTo'
--   we think of fixing the origin and moving the diagram.
moveTo :: HasOrigin t v => Point v -> t -> t
moveTo = moveOriginBy . (origin .-.)


instance VectorSpace v => HasOrigin (Point v) v where
  moveOriginTo (P u) p = p .-^ u