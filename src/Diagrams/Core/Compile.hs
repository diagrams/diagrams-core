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
  , toRTree

    -- * Backend API

  , renderDia
  , renderDiaT

    -- * Internals

  , toDTree
  , fromDTree
  , styleToOutput
  , toOutput
  )
  where

import           Data.Data
import qualified Data.List.NonEmpty        as NEL
import           Data.Maybe                (fromMaybe)
import           Data.Monoid.Coproduct
import           Data.Monoid.MList
import           Data.Monoid.WithSemigroup (Monoid')
import           Data.Semigroup
import           Data.Tree
import           Data.Tree.DUAL
import           Data.VectorSpace
import           Diagrams.Core.Envelope    (OrderedField, diameter)
import           Diagrams.Core.Style
import           Diagrams.Core.Transform
import           Diagrams.Core.Types

emptyDTree :: Tree (DNode v a)
emptyDTree = Node DEmpty []

uncurry3 :: (a -> b -> c -> r) -> (a, b, c) -> r
uncurry3 f (x, y, z) = f x y z

-- | Convert a @QDiagram@ into a raw tree.
toDTree :: HasLinearMap v => Scalar v -> Scalar v -> QDiagram v m
                          -> Maybe (DTree v Annotation)
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
               (Node DDelay . (:[]) . fromMaybe emptyDTree . toDTree g n
                            . ($ (d, g, n)) . uncurry3)
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
fromDTree :: HasLinearMap v => DTree v Annotation -> RTree v Annotation
fromDTree = fromDTree' mempty
  where
    fromDTree' :: HasLinearMap v => Transformation v -> DTree v Annotation -> RTree v Annotation
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
  :: (HasLinearMap v, InnerSpace v, Data v, Data (Scalar v), OrderedField (Scalar v), Monoid m, Semigroup m)
  => Transformation v -> QDiagram v m -> RTree v Annotation
toRTree globalToOutput d
  = (fmap . onRStyle) (styleToOutput gToO nToO)
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
    nToO = product (map (\v -> diameter v d) basis) ** (1 / fromIntegral (dimension d))

-- | Apply a style transformation on 'RStyle' nodes; the identity for
--   other 'RNode's.
onRStyle :: (Style v -> Style v) -> (RNode v a -> RNode v a)
onRStyle f (RStyle s) = RStyle (f s)
onRStyle _ n          = n

-- | Convert all 'Measure' values to 'Output' units.  The arguments
--   are, respectively, the scaling factor from global units to output
--   units, and from normalized units to output units.  It is assumed
--   that local units are identical to output units (which will be the
--   case if all transformations have been fully pushed down and
--   applied). Normalized units are based on a logical diagram size of
--   1 x 1.
styleToOutput
  :: forall v. (Data v, Data (Scalar v), Num (Scalar v), Ord (Scalar v), Fractional (Scalar v))
  => Scalar v -> Scalar v -> Style v -> Style v
styleToOutput globalToOutput normToOutput =
  gmapAttrs (toOutput globalToOutput normToOutput :: Measure v -> Measure v)

-- | Convert an aribrary 'Measure' to 'Output' units.
toOutput :: forall v. (Data v, Data (Scalar v), Num (Scalar v), Ord (Scalar v), Fractional (Scalar v))
  => Scalar v -> Scalar v -> Measure v -> Measure v
toOutput g n m =
  case m of
     m'@(Output _) -> m'
     Local s       -> Output s
     Global s      -> Output (g * s)
     Normalized s  -> Output (n * s)

     MinM m1 m2    -> outBin min (toOutput g n m1) (toOutput g n m2)
     MaxM m1 m2    -> outBin max (toOutput g n m1) (toOutput g n m2)
     ZeroM         -> Output 0
     NegateM m'    -> outUn negate (toOutput g n m')
     PlusM m1 m2   -> outBin (+) (toOutput g n m1) (toOutput g n m2)
     ScaleM s m'   -> outUn (s*) (toOutput g n m')
  where
    outUn  op (Output o1)             = Output (op o1)
    outUn  _  _ = error "outUn: The sky is falling!"
    outBin op (Output o1) (Output o2) = Output (o1 `op` o2)
    outBin _ _ _ = error "outBin: Both skies are falling!"


--------------------------------------------------

-- | Render a diagram, returning also the transformation which was
--   used to convert the diagram from its (\"global\") coordinate
--   system into the output coordinate system.  The inverse of this
--   transformation can be used, for example, to convert output/screen
--   coordinates back into diagram coordinates.  See also 'adjustDia'.
renderDiaT
  :: ( Backend b v
     , HasLinearMap v, InnerSpace v, Data v
     , OrderedField (Scalar v), Data (Scalar v)
     , Monoid' m
     )
  => b -> Options b v -> QDiagram v m -> (Transformation v, Result b v)
renderDiaT b opts d = (g2o, renderRTree b opts' . toRTree g2o $ d')
  where (opts', g2o, d') = adjustDia b opts d

-- | Render a diagram.
renderDia
  :: ( Backend b v
     , InnerSpace v, Data v
     , OrderedField (Scalar v), Data (Scalar v)
     , Monoid' m
     )
          => b -> Options b v -> QDiagram v m -> Result b v
renderDia b opts d = snd (renderDiaT b opts d)
