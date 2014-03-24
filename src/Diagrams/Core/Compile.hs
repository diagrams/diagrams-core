{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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
  , toRTree

  -- * Internals

  , toDTree
  , fromDTree
  )
  where

import           Data.Data
import qualified Data.List.NonEmpty      as NEL
import           Data.Maybe              (fromMaybe)
import           Data.Monoid.Coproduct
import           Data.Monoid.MList
import           Data.Semigroup
import           Data.Tree
import           Data.Tree.DUAL
import           Data.VectorSpace
import           Diagrams.Core.Style
import           Diagrams.Core.Transform
import           Diagrams.Core.Types

emptyDTree :: Tree (DNode b v a)
emptyDTree = Node DEmpty []

-- | Convert a @QDiagram@ into a raw tree.
toDTree :: HasLinearMap v => QDiagram b v m -> Maybe (DTree b v Annotation)
toDTree (QD qd)
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
               (Node DDelay . (:[]) . fromMaybe emptyDTree . toDTree . ($ d))
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
fromDTree :: HasLinearMap v => DTree b v Annotation -> RTree b v Annotation
fromDTree = fromDTree' mempty
  where
    fromDTree' :: HasLinearMap v => Transformation v -> DTree b v Annotation -> RTree b v Annotation
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
-- given function along the way.  Suitable for use by backends when
-- implementing 'renderData'.  Styles must be rewritten before
-- converting to RTree in case a DelayedLeaf uses a modified
-- Attribute.
toRTree :: (Typeable v, Data v, HasLinearMap v, Floating (Scalar v)) => Transformation v -> QDiagram b v m -> RTree b v Annotation
toRTree globalToOutput d
  = (fmap . onRStyle) (toOutput (avgScale globalToOutput) (avgScale (globalToOutput <> normToGlobal)))
  . fromDTree
  . fromMaybe (Node DEmpty [])
  . toDTree
  $ d
  where
    -- XXX todo: query the diagram envelope extent in each basis
    -- direction (and its negative); construct a transformation which
    -- scales along each axis in order to normalize the range to 1
    -- unit.
    normToGlobal = undefined

onRStyle :: (Style v -> Style v) -> (RNode b v a -> RNode b v a)
onRStyle f (RStyle s) = RStyle (f s)
onRStyle _ n          = n

-- XXX comment me!
toOutput
  :: forall v. (Typeable v, Data v, HasLinearMap v, Floating (Scalar v))
  => Scalar v -> Scalar v -> Style v -> Style v
toOutput globalToOutput normToOutput = gmapAttrs convert
  where
    convert :: Measure v -> Measure v
    convert m@(Output _)   = m
    convert (Local s)      = Output s
    convert (Global s)     = Output (globalToOutput *^ s)
    convert (Normalized s) = Output (normToOutput *^ s)
