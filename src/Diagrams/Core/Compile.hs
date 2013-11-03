
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
  ( --DTree(..)
  --, DNode(..)
  --, RTree(..)
  --, RNode(..)
    fromDTree
  , toTree
  )   where


import           Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty      as NEL
import           Data.Monoid.Coproduct
import           Data.Monoid.MList
import           Data.Monoid.Split
import           Data.Semigroup
import           Data.Tree
import           Data.Tree.DUAL
import           Diagrams.Core.Transform
import           Diagrams.Core.Types

--data DNode b v a = DStyle (Style v)
--                 | DTransform (Split (Transformation v))
--                 | DAnnot a
--                 | DPrim (Prim b v)
--                 | DEmpty

--type DTree b v a = Tree (DNode b v a)

--data RNode b v a =  RStyle (Style v)
--                  | RFrozenTr (Transformation v)
--                  | RAnnot a
--                  | RPrim (Transformation v) (Prim b v)
--                  | REmpty

--type RTree b v a = Tree (RNode b v a )

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

-- | Convert a DTree to an RTree which can be used dirctly by the backends.
--   A DTree includes nodes of type @DTransform (Split (Transformation v))@.
--   In the RTree the frozen part of the transform is put in a node of type
--   @RFrozenTr (Transformation v)@ and the unfrozen part is pushed down until
--   it is either frozen or reaches a primitive node.
fromDTree :: HasLinearMap v => DTree b v () -> RTree b v ()
fromDTree = fromDTree' mempty
  where
    fromDTree' :: HasLinearMap v => Transformation v -> DTree b v () -> RTree b v ()
    -- We put the accumulated unforzen transformation (accTr) and the prim
    -- into an RPrim node.
    fromDTree' accTr (Node (DPrim p) _)
      = Node (RPrim accTr p) []

    -- Styles are transformed then stored in their own node
    -- and accTr is push down the tree.
    fromDTree' accTr (Node (DStyle s) ts)
      = Node (RStyle (transform accTr s)) (fmap (fromDTree' accTr) ts)

    -- Unfrozen transformations are accumulated and pushed down as well.
    fromDTree' accTr (Node (DTransform (M tr)) ts)
      = Node REmpty (fmap (fromDTree' (accTr <> tr)) ts)

    -- Frozen transformations are stored in the RFrozenTr node
    -- and accTr is reset to the unfrozen part of the transform.
    fromDTree' accTr (Node (DTransform (tr1 :| tr2)) ts)
      = Node (RFrozenTr (accTr <> tr1)) (fmap (fromDTree' tr2) ts)

    -- DAnnot and DEmpty nodes become REmpties, in the future my want to
    -- handle DAnnots separately if they are used, again accTr flows through.
    fromDTree' accTr (Node _ ts)
      = Node REmpty (fmap (fromDTree' accTr) ts)