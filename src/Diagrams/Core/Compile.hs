{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Compile
-- Copyright   :  (c) 2013 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module provides tools for compiling @QDiagrams@ into a more
-- convenient and optimized tree form, suitable for use by backends.
--
-----------------------------------------------------------------------------

module Diagrams.Core.Compile
  ( -- * Tools for backends
    RNode(..)
  , RTree

    -- * Backend API

  , renderDia
  , renderDiaT
  )
  where

import           Data.Typeable
import qualified Data.List.NonEmpty        as NEL
import           Data.Maybe                (fromMaybe)
import           Data.Monoid.Coproduct
import           Data.Monoid.MList
import           Data.Monoid.WithSemigroup (Monoid')
import           Data.Semigroup
import           Data.Tree

import           Diagrams.Core.Envelope    (OrderedField, diameter)
import           Diagrams.Core.Transform
import           Diagrams.Core.Types
import           Diagrams.Core.Style

import           Linear.Metric hiding (qd)


-- | Apply a style transformation on 'RStyle' nodes; the identity for
--   other 'RNode's.
onRStyle :: (Style v n -> Style v n) -> RNode b v n a -> RNode b v n a
onRStyle f (RStyle s) = RStyle (f s)
onRStyle _ n          = n

--------------------------------------------------

-- | Render a diagram, returning also the transformation which was
--   used to convert the diagram from its (\"global\") coordinate
--   system into the output coordinate system.  The inverse of this
--   transformation can be used, for example, to convert output/screen
--   coordinates back into diagram coordinates.  See also 'adjustDia'.
renderDiaT
  :: ( Backend b v n , HasLinearMap v, Metric v
#if __GLASGOW_HASKELL__ > 707
     , Typeable v
#else
     , Typeable1 v
#endif
     , Typeable n
     , OrderedField n
     , Monoid' m
     )
  => b -> Options b v n -> QDiagram b v n m -> (Transformation v n, Result b v n)
renderDiaT b opts d = undefined --(g2o, renderRTree b opts' . toRTree g2o $ d')
  where (opts', g2o, d') = adjustDia b opts d

-- | Render a diagram.
renderDia
  :: ( Backend b v n , HasLinearMap v, Metric v
#if __GLASGOW_HASKELL__ > 707
     , Typeable v
#else
     , Typeable1 v
#endif
     , Typeable n
     , OrderedField n
     , Monoid' m
     )
  => b -> Options b v n -> QDiagram b v n m -> Result b v n
renderDia b opts d = snd (renderDiaT b opts d)

