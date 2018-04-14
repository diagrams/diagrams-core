{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
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

import qualified Data.List.NonEmpty        as NEL
import           Data.Maybe                (fromMaybe)
import           Data.Monoid.Coproduct
import           Data.Monoid.MList
import           Data.Monoid.WithSemigroup (Monoid')
import           Data.Semigroup
import           Data.Tree
import           Data.Tree.DUAL
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

emptyDTree :: Tree (DNode b v n a)
emptyDTree = Node DEmpty []

uncurry3 :: (a -> b -> c -> r) -> (a, b, c) -> r
uncurry3 f (x, y, z) = f x y z

-- | Convert a @QDiagram@ into a raw tree.
toDTree :: (HasLinearMap v, Floating n, Typeable n)
        => n -> n -> QDiagram b v n m -> Maybe (DTree b v n Annotation)
toDTree g n (QD qd)
  = foldDUAL

      -- Prims at the leaves.  We ignore the accumulated d-annotations
      -- for prims (since we instead distribute them incrementally
      -- throughout the tree as they occur), or pass them to the
      -- continuation in the case of a delayed node.
      (\d -> withQDiaLeaf

               -- Prim: make a leaf node
               (\p -> Node (DPrim p) [])

               -- Delayed tree: pass the accumulated d-annotations to
               -- the continuation, convert the result to a DTree, and
               -- splice it in, adding a DDelay node to mark the point
               -- of the splice.
               (Node DDelay . (:[]) . fromMaybe emptyDTree . toDTree g n . ($ (d, g, n)) . uncurry3)
      )

      -- u-only leaves --> empty DTree. We don't care about the
      -- u-annotations.
      emptyDTree

      -- a non-empty list of child trees.
      (\ts -> case NEL.toList ts of
                [t] -> t
                ts' -> Node DEmpty ts'
      )

      -- Internal d-annotations.  We untangle the interleaved
      -- transformations and style, and carefully place the style
      -- /above/ the transform in the tree (since by calling
      -- 'untangle' we have already performed the action of the
      -- transform on the style).
      (\d t -> case get d of
                 Option Nothing   -> t
                 Option (Just d') ->
                   let (tr,sty) = untangle d'
                   in  Node (DStyle sty) [Node (DTransform tr) [t]]
      )

      -- Internal a-annotations.
      (\a t -> Node (DAnnot a) [t])
      qd

-- | Convert a @DTree@ to an @RTree@ which can be used dirctly by backends.
--   A @DTree@ includes nodes of type @DTransform (Transformation v)@;
--   in the @RTree@ transform is pushed down until it reaches a primitive node.
fromDTree :: forall b v n. (Floating n, HasLinearMap v)
          => DTree b v n Annotation -> RTree b v n Annotation
fromDTree = fromDTree' mempty
  where
    fromDTree' :: Transformation v n -> DTree b v n Annotation -> RTree b v n Annotation
    -- We put the accumulated transformation (accTr) and the prim
    -- into an RPrim node.
    fromDTree' accTr (Node (DPrim p) _)
      = Node (RPrim (transform accTr p)) []

    -- Styles are transformed then stored in their own node
    -- and accTr is push down the tree.
    fromDTree' accTr (Node (DStyle s) ts)
      = Node (RStyle (transform accTr s)) (fmap (fromDTree' accTr) ts)

    -- Transformations are accumulated and pushed down as well.
    fromDTree' accTr (Node (DTransform tr) ts)
      = Node REmpty (fmap (fromDTree' (accTr <> tr)) ts)

    fromDTree' accTr (Node (DAnnot a) ts)
      = Node (RAnnot a) (fmap (fromDTree' accTr) ts)

    -- Drop accumulated transformations upon encountering a DDelay
    -- node --- the tree unfolded beneath it already took into account
    -- any transformation at this point.
    fromDTree' _ (Node DDelay ts)
      = Node REmpty (fmap (fromDTree' mempty) ts)

    -- DEmpty nodes become REmpties, again accTr flows through.
    fromDTree' accTr (Node _ ts)
      = Node REmpty (fmap (fromDTree' accTr) ts)

-- | Compile a @QDiagram@ into an 'RTree', rewriting styles with the
--   given function along the way.  Suitable for use by backends when
--   implementing 'renderData'.  The first argument is the
--   transformation used to convert the diagram from local to output
--   units.
toRTree
  :: (HasLinearMap v, Metric v, Typeable n,
      OrderedField n, Monoid m, Semigroup m)
  => Transformation v n -> QDiagram b v n m -> RTree b v n Annotation
toRTree globalToOutput d
  = (fmap . onRStyle) (unmeasureAttrs gToO nToO)
  . fromDTree
  . fromMaybe (Node DEmpty [])
  . toDTree gToO nToO
  $ d
  where
    gToO = avgScale globalToOutput

    -- Scaling factor from normalized units to output units: nth root
    -- of product of diameters along each basis direction.  Note at
    -- this point the diagram has already had the globalToOutput
    -- transformation applied, so output = global = local units.
    nToO = product (map (`diameter` d) basis) ** (1 / fromIntegral (dimension d))

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
  :: (Backend b v n , HasLinearMap v, Metric v,
      Typeable n, OrderedField n, Monoid' m)
  => b -> Options b v n -> QDiagram b v n m -> (Transformation v n, Result b v n)
renderDiaT b opts d = (g2o, renderRTree b opts' . toRTree g2o $ d')
  where (opts', g2o, d') = adjustDia b opts d

-- | Render a diagram.
renderDia
  :: (Backend b v n , HasLinearMap v, Metric v,
      Typeable n, OrderedField n, Monoid' m)
  => b -> Options b v n -> QDiagram b v n m -> Result b v n
renderDia b opts d = snd (renderDiaT b opts d)

