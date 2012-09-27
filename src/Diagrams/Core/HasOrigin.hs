{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , TypeFamilies
           , UndecidableInstances
  #-}

-- The UndecidableInstances flag is needed under 6.12.3 for the
-- HasOrigin (a,b) instance.

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.HasOrigin
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Types which have an intrinsic notion of a \"local origin\",
-- /i.e./ things which are /not/ invariant under translation.
--
-----------------------------------------------------------------------------

module Diagrams.Core.HasOrigin
       ( HasOrigin(..), moveOriginBy, moveTo, place
       ) where

import qualified Data.Map as M
import qualified Data.Set as S

import           Data.AffineSpace ((.-^), (.-.))
import           Data.VectorSpace

import           Diagrams.Core.Points
import           Diagrams.Core.V

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

-- | A flipped variant of 'moveTo', provided for convenience.  Useful
--   when writing a function which takes a point as an argument, such
--   as when using 'withName' and friends.
place :: HasOrigin t => t -> Point (V t) -> t
place = flip moveTo

instance VectorSpace v => HasOrigin (Point v) where
  moveOriginTo (P u) p = p .-^ u

instance (HasOrigin a, HasOrigin b, V a ~ V b) => HasOrigin (a,b) where
  moveOriginTo p (x,y) = (moveOriginTo p x, moveOriginTo p y)

instance HasOrigin a => HasOrigin [a] where
  moveOriginTo = map . moveOriginTo

instance (HasOrigin a, Ord a) => HasOrigin (S.Set a) where
  moveOriginTo = S.map . moveOriginTo

instance HasOrigin a => HasOrigin (M.Map k a) where
  moveOriginTo = M.map . moveOriginTo