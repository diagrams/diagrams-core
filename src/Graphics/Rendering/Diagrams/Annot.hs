{-# LANGUAGE TypeFamilies
           , GeneralizedNewtypeDeriving
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Annot
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- "Graphics.Rendering.Diagrams" defines the core library of primitives
-- forming the basis of an embedded domain-specific language for
-- describing and rendering diagrams.
--
-- The @Annot@ module defines a type for \"annotations\" on diagrams, which
-- are functions from points in a vector space to some monoid.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Annot
       ( Annot(..)
       ) where

import Graphics.Rendering.Diagrams.V
import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.HasOrigin

import Data.VectorSpace
import Data.AffineSpace

import Data.Monoid
import Control.Applicative

------------------------------------------------------------
--  Annotations  -------------------------------------------
------------------------------------------------------------

-- | An annotation is a function that maps points in a vector space to
--   values in some monoid. Annotations naturally form a monoid, with
--   two annotations being combined pointwise.
newtype Annot v m = Annot { queryAnnot :: Point v -> m }
  deriving (Functor, Applicative, Monoid)

type instance V (Annot v m) = v

instance VectorSpace v => HasOrigin (Annot v m) where
  moveOriginTo (P u) (Annot f) = Annot $ \p -> f (p .+^ u)

instance HasLinearMap v => Transformable (Annot v m) where
  transform t (Annot f) = Annot $ f . papply (inv t)