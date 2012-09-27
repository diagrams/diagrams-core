{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.MList
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Type family for identifying associated vector spaces.
--
-----------------------------------------------------------------------------

module Diagrams.Core.V
       ( V

       ) where

import Data.Map
import Data.Monoid.Coproduct
import Data.Monoid.Deletable
import Data.Monoid.Split
import Data.Semigroup
import Data.Set

------------------------------------------------------------
-- Vector spaces -------------------------------------------
------------------------------------------------------------

-- | Many sorts of objects have an associated vector space in which
--   they \"live\".  The type function @V@ maps from object types to
--   the associated vector space.
type family V a :: *

type instance V Double    = Double
type instance V Rational  = Rational

-- Note, to use these instances one often needs a constraint of the form
--   V a ~ V b, etc.
type instance V (a,b)      = V a
type instance V (a,b,c)    = V a

type instance V (a -> b)   = V b
type instance V [a]        = V a
type instance V (Option a) = V a
type instance V (Set a)    = V a
type instance V (Map k a)  = V a

type instance V (Deletable m) = V m
type instance V (Split m)     = V m
type instance V (m :+: n)     = V m