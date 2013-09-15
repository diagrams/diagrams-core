
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Compile
-- Copyright   :  (c) 2013 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- XXX comment me
--
-----------------------------------------------------------------------------

module Diagrams.Core.Compile
  ( DTree(..)
  , DNode(..)
  , toTree
  , getPrims
  )   where


import           Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty      as NEL
import           Data.Monoid.Coproduct
import           Data.Monoid.MList
import           Data.Monoid.Split
import           Data.Monoid.Action
import           Data.Semigroup
import           Data.Tree
import           Data.Tree.DUAL
import           Diagrams.Core.Style
import           Diagrams.Core.Transform
import           Diagrams.Core.Types

data DNode b v a = DStyle (Style v)
                 | DTransform (Split (Transformation v))
                 | DAnnot a
                 | DPrim (Prim b v)
                 | DEmpty

{- for some quick and dirty testing
  deriving Show

instance Show (Prim b v) where
  show _ = "prim"

instance Show (Transformation v) where
  show _ = "transform"

instance Show (Style v) where
  show _ = "style"
-}

type DTree b v a = Tree (DNode b v a)

toTree :: HasLinearMap v => QDiagram b v m -> Maybe (DTree b v ())
toTree (QD qd)
  = foldDUAL

      -- Prims at the leaves.  We ignore the accumulated
      -- d-annotations, since we will instead distribute them
      -- incrementally throughout the tree as they occur.
      (\_ p -> Node (DPrim p) [])

      -- u-only leaves --> empty DTree. We don't care about the
      -- u-annotations.
      (Node DEmpty [])

      -- a non-empty list of child trees.
      (\ts -> case NEL.toList ts of
                [t] -> t
                ts' -> Node DEmpty ts'
      )

      -- Internal d-annotations.  We untangle the interleaved
      -- transformations and style, and carefully place the style
      -- *above* the transform in the tree (since by calling
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

primList :: HasLinearMap v
       => DTree b v () -> [(Prim b v, (Split (Transformation v), Style v))]
primList = primList' (mempty, mempty)
  where
    primList' dacc (Node (DPrim p) ts) =
      (p, dacc) : concatMap (primList' dacc) ts
    primList' (t, s) (Node (DStyle sty) ts) =
      concatMap (primList' (t, s <> act t sty)) ts
    primList' (t, s) (Node (DTransform tr) ts) =
      concatMap (primList' (t <> tr, s)) ts
    primList' dacc (Node (DAnnot ()) ts) = concatMap (primList' dacc) ts
    primList' dacc (Node  DEmpty ts) = concatMap (primList' dacc) ts


getPrims :: HasLinearMap v
         => QDiagram b v m -> [(Prim b v, (Split (Transformation v), Style v))]
getPrims d = primList $ fromMaybe (Node DEmpty []) (toTree d)