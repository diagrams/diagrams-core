{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Query
-- Copyright   :  (c) 2011-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The @Query@ module defines a type for \"queries\" on diagrams, which
-- are functions from points in a vector space to some monoid.
--
-----------------------------------------------------------------------------

module Diagrams.Core.Query
  ( Query (..)
  ) where

import           Control.Applicative
import           Control.Lens            (Rewrapped, Wrapped (..), iso)
import           Data.Semigroup

import           Linear.Affine
import           Linear.Vector

import           Diagrams.Core.HasOrigin
import           Diagrams.Core.Transform
import           Diagrams.Core.V

------------------------------------------------------------
--  Queries  -----------------------------------------------
------------------------------------------------------------

-- | A query is a function that maps points in a vector space to
--   values in some monoid. Queries naturally form a monoid, with
--   two queries being combined pointwise.
--
--   The idea for annotating diagrams with monoidal queries came from
--   the graphics-drawingcombinators package, <http://hackage.haskell.org/package/graphics-drawingcombinators>.
newtype Query v n m = Query { runQuery :: Point v n -> m }
  deriving (Functor, Applicative, Semigroup, Monoid)

instance Wrapped (Query v n m) where
  type Unwrapped (Query v n m) = Point v n -> m
  _Wrapped' = iso runQuery Query

instance Rewrapped (Query v a m) (Query v' a' m')

type instance V (Query v n m) = v
type instance N (Query v n m) = n

instance (Additive v, Num n) => HasOrigin (Query v n m) where
  moveOriginTo (P u) (Query f) = Query $ \p -> f (p .+^ u)

instance (Additive v, Num n) => Transformable (Query v n m) where
  transform t (Query f) = Query $ f . papply (inv t)

