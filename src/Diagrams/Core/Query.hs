{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-unused-imports       #-}
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
import           Control.Lens
import           Data.Semigroup
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Profunctor
import           Data.Profunctor.Sieve
import           Data.Profunctor.Closed
import qualified Data.Profunctor.Rep    as P

import           Linear.Affine
import           Linear.Vector

import           Diagrams.Core.HasOrigin
import           Diagrams.Core.Transform
import           Diagrams.Core.V

------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------

-- | A query is a function that maps points in a vector space to
--   values in some monoid. Queries naturally form a monoid, with
--   two queries being combined pointwise.
--
--   The idea for annotating diagrams with monoidal queries came from
--   the graphics-drawingcombinators package,
--   <http://hackage.haskell.org/package/graphics-drawingcombinators>.
newtype Query v n m = Query { runQuery :: Point v n -> m }
  deriving (Functor, Applicative, Monad, Semigroup, Monoid)

instance Distributive (Query v n) where
  distribute a = Query $ \p -> fmap (\(Query q) -> q p) a

instance Representable (Query v n) where
  type Rep (Query v n) = Point v n
  tabulate = Query
  index    = runQuery

instance Functor v => Profunctor (Query v) where
  lmap f (Query q) = Query $ \p -> q (fmap f p)
  rmap = fmap

instance Functor v => Cosieve (Query v) (Point v) where
  cosieve = runQuery

instance Functor v => Closed (Query v) where
  closed (Query fab) = Query $ \fxa x -> fab (fmap ($ x) fxa)

instance Functor v => Costrong (Query v) where
  unfirst (Query f) = Query f'
    where f' fa = b where (b, d) = f ((\a -> (a, d)) <$> fa)
  unsecond (Query f) = Query f'
    where f' fa = b where (d, b) = f ((,) d <$> fa)

instance Functor v => P.Corepresentable (Query v) where
  type Corep (Query v) = Point v
  cotabulate = Query

-- | Setter over the input point of a query.
queryPoint :: Setter (Query v' n' m) (Query v n m) (Point v n) (Point v' n')
queryPoint = sets $ \f (Query q) -> Query $ q . f

instance Wrapped (Query v n m) where
  type Unwrapped (Query v n m) = Point v n -> m
  _Wrapped' = iso runQuery Query

instance Rewrapped (Query v a m) (Query v' a' m')

type instance V (Query v n m) = v
type instance N (Query v n m) = n

instance (Additive v, Num n) => HasOrigin (Query v n m) where
  moveOriginTo (P u) = queryPoint %~ (.+^ u)

instance (Additive v, Num n) => Transformable (Query v n m) where
  transform t = queryPoint %~ papply (inv t)

