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
    RNode(..)
  , RTree
  , toRTree

    -- * Backend API

  , renderDia
  , renderDiaT

    -- * Internals

  , toDTree
  , fromDTree
  )
  where

import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Maybe                (fromMaybe)
import           Data.Monoid.Coproduct
import           Data.Monoid.MList
import           Data.Monoid.WithSemigroup (Monoid')
import           Data.Semigroup
import           Data.Tree
import           Data.Tree.DUAL            (foldDUAL)
import           Data.Typeable

import           Diagrams.Core.Envelope    (OrderedField, diameter)
import           Diagrams.Core.Style
import           Diagrams.Core.Transform
import           Diagrams.Core.Types

import           Linear.Metric             hiding (qd)

-- Typeable1 is a depreciated synonym in ghc > 707
#if __GLASGOW_HASKELL__ >= 707
#define Typeable1 Typeable
#endif

emptyDTree :: DTree b v n a
emptyDTree = Node DEmpty []

-- | Convert a @QDiagram@ into a raw tree.
toDTree :: (HasLinearMap v, Floating n, Typeable n)
        => n -> n -> QDiagram b v n m -> DTree b v n Annotation
toDTree g n (QD qd) = fromMaybe emptyDTree $
  foldDUAL leaf uannot branch dannot annot qd
  where
    -- We ignore the accumulated d-annotations for prims (since we
    -- instead distribute them incrementally throughout the tree as they
    -- occur), or pass them to the continuation in the case of a delayed
    -- node.
    leaf da = \case
      PrimLeaf p    -> Node (DPrim p) []
      -- Pass accumilated d-annotations to make a delayed diagram. Mark
      -- with DDelay to prevent transforming again.
      DelayedLeaf f ->
        let dia = f da g n
        in  Node DDelay [toDTree g n dia]

    -- u-only leaves => empty DTree. We don't care about the
    -- u-annotations.
    uannot = emptyDTree

    branch = \case
      t :| [] -> t
      t :| ts -> Node DEmpty (t:ts)

    dannot d t = case get d of
      Option Nothing   -> t
      Option (Just d') ->
        let (tr,sty) = untangle d'
            -- the style is already transformed so place it above
        in  Node (DStyle sty) [Node (DTransform tr) [t]]

    annot a t = Node (DAnnot a) [t]

-- | Convert a @DTree@ to an @RTree@ which can be used directly by backends.
--   A @DTree@ includes nodes of type @DTransform (Transformation v)@;
--   in the @RTree@ transform is pushed down until it reaches a primitive node.
fromDTree :: (HasLinearMap v, Floating n)
          => DTree b v n Annotation -> RTree b v n Annotation
fromDTree = go mempty
  where
    go t (Node n ns) = case n of
      DPrim p       -> Node (RPrim $ transform t p) []
      DStyle s      -> Node (RStyle $ transform t s) ts'
      DAnnot a      -> Node (RAnnot a) ts'
      DTransform t' -> Node REmpty $ tts (t <> t')
      DDelay        -> Node REmpty $ tts mempty
      _             -> Node REmpty ts'
      where
        -- apply a transform to the node's trees
        tts t' = fmap (go t') ns
        -- trees with accumulated transform applied
        ts'    = tts t

-- | Compile a @QDiagram@ into an 'RTree', rewriting styles with the
--   given function along the way.  Suitable for use by backends when
--   implementing 'renderData'.  The first argument is the
--   transformation used to convert the diagram from local to output
--   units.
toRTree
  :: (HasLinearMap v, Metric v, Typeable1 v, Typeable n,
      OrderedField n, Monoid m, Semigroup m)
  => Transformation v n -> QDiagram b v n m -> RTree b v n Annotation
toRTree globalToOutput d
  = (fmap . onRStyle) (unmeasureAttrs gToO nToO)
  . fromDTree
  . toDTree gToO nToO
  $ d
  where
    gToO = avgScale globalToOutput

    -- Scaling factor from normalized units to output units: nth root
    -- of product of diameters along each basis direction.  Note at
    -- this point the diagram has already had the globalToOutput
    -- transformation applied, so output = global = local units.
    nToO = product (map (`diameter` d) basis) ** (1 / fromIntegral (dimension d))

-- rTree :: Iso' (QDiagram b v n Any) (DTree b v n Annotation)
-- rTree = iso (toRTree mempty) fromRTree

-- fromRTree :: RTree b v n Annotation -> QDiagram b v n Any
-- fromRTree (Node n rs) = case n of
--   RPrim p  -> mkQD p mempty mempty mempty mempty
--   RStyle s -> applyStyle s d
--   RAnnot a -> applyAnnotation a d
--   _        -> d
--   where d = foldMap fromRTree rs

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
  :: (Backend b v n , HasLinearMap v, Metric v, Typeable1 v,
      Typeable n, OrderedField n, Monoid' m)
  => b -> Options b v n -> QDiagram b v n m -> (Transformation v n, Result b v n)
renderDiaT b opts d = (g2o, renderRTree b opts' . toRTree g2o $ d')
  where (opts', g2o, d') = adjustDia b opts d

-- | Render a diagram.
renderDia
  :: (Backend b v n , HasLinearMap v, Metric v, Typeable1 v,
      Typeable n, OrderedField n, Monoid' m)
  => b -> Options b v n -> QDiagram b v n m -> Result b v n
renderDia b opts d = snd (renderDiaT b opts d)

