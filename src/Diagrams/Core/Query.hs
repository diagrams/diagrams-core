{-# LANGUAGE FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Query
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The @Query@ module defines a type for \"queries\" on diagrams, which
-- are functions from points in a vector space to some monoid.
--
-----------------------------------------------------------------------------

module Diagrams.Core.Query
       ( Query(Query), runQuery
       ) where

import Control.Applicative
import Control.Lens (Wrapped(..), Rewrapped, iso)
import Data.Semigroup
import Data.Monoid.Action (Action(..))

import Data.Basis (HasBasis)
import Data.AffineSpace
import Data.VectorSpace

import Diagrams.Core.HasOrigin
import Diagrams.Core.Points
import Diagrams.Core.Transform
import Diagrams.Core.V

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

instance Wrapped (Query v m) where
    type Unwrapped (Query v m) = (Point v -> m)
    _Wrapped' = iso runQuery Query

instance Rewrapped (Query v m) (Query v' m')

type instance V (Query v m) = v

instance VectorSpace v => HasOrigin (Query v m) where
  moveOriginTo (P u) (Query f) = Query $ \p -> f (p .+^ u)

instance HasLinearMap v => Transformable (Query v m) where
  transform t (Query f) = Query $ f . papply (inv t)

instance (v ~ v', HasLinearMap v) => Action (Transformation v) (Query v' m) where
  act = transform
