{-# LANGUAGE TypeFamilies
           , GeneralizedNewtypeDeriving
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Query
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The @Query@ module defines a type for \"queries\" on diagrams, which
-- are functions from points in a vector space to some monoid.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Query
       ( Query(..)
       ) where

import Control.Applicative
import Data.Semigroup

import Data.AffineSpace
import Data.VectorSpace

import Graphics.Rendering.Diagrams.HasOrigin
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.V

------------------------------------------------------------
--  Queries  -----------------------------------------------
------------------------------------------------------------

-- | A query is a function that maps points in a vector space to
--   values in some monoid. Queries naturally form a monoid, with
--   two queries being combined pointwise.
--
--   The idea for annotating diagrams with monoidal queries came from
--   the graphics-drawingcombinators package, <http://hackage.haskell.org/package/graphics-drawingcombinators>.
newtype Query v m = Query { runQuery :: Point v -> m }
  deriving (Functor, Applicative, Semigroup, Monoid)

type instance V (Query v m) = v

instance VectorSpace v => HasOrigin (Query v m) where
  moveOriginTo (P u) (Query f) = Query $ \p -> f (p .+^ u)

instance HasLinearMap v => Transformable (Query v m) where
  transform t (Query f) = Query $ f . papply (inv t)