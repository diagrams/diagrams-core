
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
  , RTree(..)
  , RNode(..)
  , fromDTree
  , toTree
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

type DTree b v a = Tree (DNode b v a)

data RNode b v a =  RStyle (Style v)
                  | RFrozenTr (Transformation v)
                  | RUnFrozenTr
                  | RAnnot a
                  | RPrim (Transformation v) (Prim b v)
                  | REmpty

type RTree b v a = Tree (RNode b v a )

 {--for some quick and dirty testing
  deriving Show

instance Show (Prim b v) where
  show _ = "prim"

instance Show (Transformation v) where
  show _ = "transform"

instance Show (Style v) where
  show _ = "style" -}

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

-- | Convert a DTree to an RTree which has separate nodes for frozen and
--   unforzen transormations. The unforzen transformations are accumulated.
--   When a frozen transformation is reached the transform is put in the node
--   for processsing by the backend and the accumulator is reset.
fromDTree :: HasLinearMap v => Transformation v -> DTree b v () -> RTree b v ()

-- Prims are left as is.
fromDTree accTr (Node (DPrim p) _)
  = Node (RPrim accTr p) []

-- Styles are stored in the node
fromDTree accTr (Node (DStyle s) ts)
  = Node (RStyle s) (fmap (fromDTree accTr) ts)

-- Unfrozen transformations are accumulated
fromDTree accTr (Node (DTransform (M tr)) ts)
  = Node RUnFrozenTr (fmap (fromDTree (accTr <> tr)) ts)

-- Frozen transformations are stored in the node and the accumulator is reset.
fromDTree accTr (Node (DTransform (tr1 :| tr2)) ts)
  = Node (RFrozenTr (accTr <> tr1)) (fmap (fromDTree tr2) ts)

-- DAnnot and DEmpty nodes become REmpties, in the future my want to
-- handle DAnnots differently if they are used.
fromDTree accTr (Node _ ts)
  = Node REmpty (fmap (fromDTree accTr) ts)