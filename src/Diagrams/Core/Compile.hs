{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Compile
-- Copyright   :  (c) 2013-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module provides tools for compiling @QDiagrams@ into a more
-- convenient and optimized tree form, suitable for use by backends.
--
-----------------------------------------------------------------------------

module Diagrams.Core.Compile
  ( -- * Tools for backends
    foldDia
  , foldDia'

    -- * Backend API

  , renderDia
  , renderDiaT
  )
  where

import           Control.Lens              hiding (transform)
import qualified Data.Foldable             as F
import           Data.Monoid.Coproduct
import qualified Data.Monoid as            M
import           Data.Monoid.WithSemigroup (Monoid')
import           Data.Tree.DUAL            (foldDUAL, foldDUAL')
import           Data.Typeable

import           Diagrams.Core.Envelope    (OrderedField, size)
import           Diagrams.Core.Style
import           Diagrams.Core.Transform
import           Diagrams.Core.Types

import           Linear.Metric             hiding (qd)

-- Typeable1 is a depreciated synonym in ghc > 707
#if __GLASGOW_HASKELL__ >= 707
#define Typeable1 Typeable
#endif

foldDiaWithScales
  :: (HasLinearMap v, Floating n, Typeable n, M.Monoid r)
  => (Style v n -> Prim b v n -> r)
  -> (Annotation b v n -> r -> r)
  -> n -- 'global' to 'output' scale factor
  -> n -- 'normalised' to 'output' scale factor
  -> QDiagram b v n m -- ^ diagram to fold
  -> r
foldDiaWithScales primF aF g n (QD dual) = foldDUAL lF aF dual
  where
    lF d = \case
      PrimLeaf p    ->
        let (tr, sty) = untangle d
        in  primF (unmeasureAttrs g n sty) (transform tr p)
      DelayedLeaf f ->
        let (QD dia) = f d g n
        in  foldDUAL lF aF dia

foldDiaWithScales'
  :: (HasLinearMap v, Metric v, OrderedField n, Typeable n, Monoid' m, M.Monoid r)
  => (Style v n -> Prim b v n -> r)
  -> (Annotation b v n -> r -> r)
  -> (Style v n -> r -> r)
  -> n
  -> n
  -> QDiagram b v n m -- ^ diagram to fold
  -> r
foldDiaWithScales' primF aF styF g n (QD dual) = foldDUAL' lF aF mkP styF dual
  where
    lF d = \case
      PrimLeaf p    ->
        let (tr, sty) = untangle d
        in  primF (unmeasureAttrs g n sty) (transform tr p)
      DelayedLeaf f ->
        let (QD dia) = f d g n
        in  foldDUAL' lF aF mkP styF dia

    -- The partial sty needs the total transform accumilated so far, but
    -- ignores any style before.
    mkP d w = transform t (unmeasureAttrs g n sty)
      where t        = killR d
            (_, sty) = untangle w -- killL is BAD

-- | Simple way to fold a diagram into a monadic result.
foldDia
  :: (HasLinearMap v, Metric v, OrderedField n, Typeable n, Monoid' m, M.Monoid r)
  => (Style v n -> Prim b v n -> r) -- ^ Fold a prim
  -> (Annotation b v n -> r -> r)   -- ^ Apply an annotation
  -> Transformation v n             -- ^ final transform for diagram
  -> QDiagram b v n m               -- ^ diagram to fold
  -> r
foldDia primF annF t d = foldDiaWithScales primF annF g n d
  where
    g = avgScale t
    n = normalizedFactor (size d)

-- | Fold a diagram into a monadic result. Similar to 'foldDia' but
--   gives access to the style before applying parts of a style (usually
--   'Clip') when it's going to be applied to multiple prims. This is
--   reset after each group and given as the second argument in the prim
--   rendering function.
foldDia'
  :: (HasLinearMap v, Metric v, OrderedField n, Typeable n, Monoid' m, M.Monoid r)
  => (Style v n -> Prim b v n -> r)
  -> (Annotation b v n -> r -> r)
  -> (Style v n -> r -> r)
  -> Transformation v n
  -> QDiagram b v n m -- ^ diagram to fold
  -> r
foldDia' primF annF styF t d = foldDiaWithScales' primF annF styF g n d
  where
    g = avgScale t
    n = normalizedFactor (size d)

-- | Get the normalized scale factor from a vector. For the
--   'normalizedFactor' of a diagram use this with the 'size' of the
--   diagram.
--
--   Note: The 'global' factor is the 'avgScale' of the output
--   transform.
normalizedFactor :: (F.Foldable v, Floating n) => v n -> n
normalizedFactor v = F.product v ** (1 / fromIntegral (lengthOf folded v))

-- | Render a diagram, returning also the transformation which was
--   used to convert the diagram from its (\"global\") coordinate
--   system into the output coordinate system.  The inverse of this
--   transformation can be used, for example, to convert output/screen
--   coordinates back into diagram coordinates.  See also 'adjustDia'.
renderDiaT
  :: (Backend b v n , HasLinearMap v, Metric v, Typeable1 v,
      Typeable n, OrderedField n, Monoid' m)
  => b -> Options b v n -> QDiagram b v n m -> (Transformation v n, Result b v n)
renderDiaT b opts d = (g2o, renderDUAL b opts' g2o d')
  where (opts', g2o, d') = adjustDia b opts d

-- | Render a diagram.
renderDia
  :: (Backend b v n , HasLinearMap v, Metric v, Typeable1 v,
      Typeable n, OrderedField n, Monoid' m)
  => b -> Options b v n -> QDiagram b v n m -> Result b v n
renderDia b opts d = snd (renderDiaT b opts d)

