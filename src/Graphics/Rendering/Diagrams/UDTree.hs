{-# LANGUAGE DeriveFunctor
           , TypeOperators
           , FlexibleContexts
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.UDTree
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Rose (n-way) trees with both upwards- and downwards-traveling
-- monoidal annotations, used as the basis for representing diagrams.
--
-----------------------------------------------------------------------------
module Graphics.Rendering.Diagrams.UDTree
       (
         -- * UD-trees
         UDTree(..)

         -- * Constructing UD-trees
       , leaf, branchD, branch

         -- * Modifying UD-trees
       , applyD, applyUpre, applyUpost, mapU

         -- * Accessors and destructors
       , getU, getU', foldUD, flatten

       ) where

import Data.Semigroup

import Graphics.Rendering.Diagrams.Monoids
import Graphics.Rendering.Diagrams.MList

-- | Abstractly, a UDTree is a rose (n-way) tree with data at the
--   leaves and two types of monoidal annotations, one (called @u@)
--   travelling \"up\" the tree and one (called @d@) traveling
--   \"down\".
--
--   Specifically, every node (both leaf nodes and internal nodes)
--   has two annotations, one of type @d@ and one of type @u@,
--   subject to the following constraints:
--
--   * The @d@ annotation at a leaf node is equal to the 'mconcat' of
--     all the @d@ annotations along the path from the root to the leaf
--     node.
--
--   * The @u@ annotation at an internal node is equal to @v1
--     ``mappend`` (mconcat us) ``mappend`` v2@ for some values @v1@
--     and @v2@ (possibly 'mempty'), where @us@ is the list (in
--     left-right order) of the @u@ annotations on the immediate child
--     nodes of the given node.  Intuitively, we are \"caching\" the
--     @mconcat@ of @u@ annotations from the leaves up, except that at
--     any point we may insert \"extra\" information.
--
--   In addition, @d@ may have an /action/ on @u@ (see the 'Action'
--   type class, defined in "Graphics.Rendering.Diagrams.Monoids"), in
--   which case applying a @d@ annotation to a tree will transform all
--   the @u@ annotations by acting on them.  The constraints on @u@
--   annotations are maintained since the action is required to be a
--   monoid homomorphism.

data UDTree u d a
  = Leaf u a
  | Branch u [d] [UDTree u d a]
  deriving (Functor)

-- XXX need to sort out all the semigroup/monoid stuff in here!

instance (Action d u, Monoid u, Monoid d) => Semigroup (UDTree u d a) where
  t1 <> t2 = branch [t1,t2]

-- | @UDTree@s form a monoid where @mappend@ corresponds to adjoining
--   two trees under a common parent root.  Note that this technically
--   does not satisfy associativity, but it does with respect to
--   'flatten' which is what we really care about.  @mconcat@ is
--   specialized to put all the trees under a single parent.
instance (Action d u, Monoid u, Monoid d) => Monoid (UDTree u d a) where
  mempty          = Branch mempty mempty []
  t1 `mappend` t2 = branch [t1,t2]
  mconcat         = branch

-- | Construct a leaf node from a @u@ annotation and datum.
leaf :: u -> a -> UDTree u d a
leaf = Leaf

-- | Construct a branch node with an explicit @d@ annotation.
branchD :: (Action d u, Monoid u) => d -> [UDTree u d a] -> UDTree u d a
branchD d ts = Branch (mconcat . map getU $ ts) [d] ts

-- | Construct a branch node with a default (identity) @d@ annotation.
branch :: (Action d u, Monoid u, Monoid d) => [UDTree u d a] -> UDTree u d a
branch ts = Branch (mconcat . map getU $ ts) [] ts

-- | Get the @u@ annotation at the root.
getU :: Action d u => UDTree u d a -> u
getU (Leaf u _)      = u
getU (Branch u ds _) = foldr act u ds

-- | Get a particular component from a the @u@ annotation at the root.
--   This method is provided for convenience, since its context only
--   requires an action of @d@ on @u'@, rather than on @u@ in its
--   entirety.
getU' :: (Action d (u' ::: Nil), u :>: u') => UDTree u d a -> u'
getU' (Leaf u _)      = get u
getU' (Branch u ds _) = hd $ foldr act (get u ::: Nil) ds
  where hd (u' ::: Nil) = u'
        hd (Missing _)  = error "Impossible case in UDTree.getU' (hd)"

-- | Add a @d@ annotation to the root, combining it (on the left) with
--   any pre-existing @d@ annotation, and transforming all @u@
--   annotations by the action of @d@.
applyD :: Action d u => d -> UDTree u d a -> UDTree u d a
applyD d l@(Leaf {})      = Branch (getU l) [d] [l]
applyD d (Branch u ds ts) = Branch u (d : ds) ts

-- | Add a @u@ annotation to the root, combining it (on the left) with
--   the existing @u@ annotation.
applyUpre :: (Semigroup u, Action d u) => u -> UDTree u d a -> UDTree u d a
applyUpre u' (Leaf u a) = Leaf (u' <> u) a
applyUpre u' b          = Branch (u' <> getU b) [] [b]

-- | Add a @u@ annotation to the root, combining it (on the right) with
--   the existing @u@ annotation.
applyUpost :: (Semigroup u, Action d u) => u -> UDTree u d a -> UDTree u d a
applyUpost u' (Leaf u a) = Leaf (u <> u') a
applyUpost u' b          = Branch (getU b <> u') [] [b]

-- | Map a function over all the @u@ annotations.  The function must
--   be a monoid homomorphism, and must commute with the action of @d@
--   on @u@.  That is, to use @mapU f@ safely it must be the case that
--   @f (act d u) == act d (f u)@.
mapU :: (u -> u') -> UDTree u d a -> UDTree u' d a
mapU f (Leaf u a)       = Leaf (f u) a
mapU f (Branch u ds ts) = Branch (f u) ds (map (mapU f) ts)

-- | A fold for UDTrees.
foldUD :: (Monoid r, Semigroup d, Monoid d, Action d u)
      => (u -> d -> a -> r)  -- ^ Function for processing leaf nodes.
                             --   Given the u annotation at this node, the
                             --   'mconcat' of all d annotations above, and the
                             --   leaf value.
      -> (u -> d -> r -> r)  -- ^ Function for processing internal
                             --   nodes.  Given the u and d
                             --   annotations at this node and the
                             --   'mconcat' of the recursive results.
      -> UDTree u d a -> r
foldUD = foldUD' mempty     -- Pass along accumulated d value
  where foldUD' d l _ (Leaf u a)
          = l (act d u) d a
        foldUD' d l b (Branch u ds ts)
          = b (act (d <> d') u) d' (mconcat $ map (foldUD' (d <> d') l b) ts)
         where d' = mconcat ds

-- | A specialized fold provided for convenience: flatten a tree into
--   a list of leaves along with their @d@ annotations.
flatten :: (Semigroup d, Monoid d, Action d u) => UDTree u d a -> [(a,d)]
flatten = foldUD (\_ d a -> [(a,d)]) (\_ _ r -> r)