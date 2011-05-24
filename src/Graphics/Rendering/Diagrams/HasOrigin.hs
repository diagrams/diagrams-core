{-# LANGUAGE FlexibleInstances
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
-- /i.e./ things which are /not/ invariant under translation.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.HasOrigin
       ( HasOrigin(..), moveOriginBy, moveTo
       ) where

import Graphics.Rendering.Diagrams.V
import Graphics.Rendering.Diagrams.Points

import Data.AffineSpace ((.-^), (.-.))
import Data.VectorSpace

-- | Class of types which have an intrinsic notion of a \"local
--   origin\", i.e. things which are not invariant under translation,
--   and which allow the origin to be moved.
--
--   One might wonder why not just use 'Transformable' instead of
--   having a separate class for 'HasOrigin'; indeed, for types which
--   are instances of both we should have the identity
--
--   > moveOriginTo (origin .^+ v) === translate (negateV v)
--
--   The reason is that some things (e.g. vectors, 'Trail's) are
--   transformable but are translationally invariant, i.e. have no
--   origin.
class VectorSpace (V t) => HasOrigin t where

  -- | Move the local origin to another point.
  --
  --   Note that this function is in some sense dual to 'translate'
  --   (for types which are also 'Transformable'); moving the origin
  --   itself while leaving the object \"fixed\" is dual to fixing the
  --   origin and translating the diagram.
  moveOriginTo :: Point (V t) -> t -> t

-- | Move the local origin by a relative vector.
moveOriginBy :: HasOrigin t => V t -> t -> t
moveOriginBy = moveOriginTo . P

-- | Translate the object by the translation that sends the origin to
--   the given point. Note that this is dual to 'moveOriginTo', i.e. we
--   should have
--
--   > moveTo (origin .^+ v) === moveOriginTo (origin .^- v)
--
--   For types which are also 'Transformable', this is essentially the
--   same as 'translate', i.e.
--
--   > moveTo (origin .^+ v) === translate v
moveTo :: HasOrigin t => Point (V t) -> t -> t
moveTo = moveOriginBy . (origin .-.)

instance VectorSpace v => HasOrigin (Point v) where
  moveOriginTo (P u) p = p .-^ u