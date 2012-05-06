{-# LANGUAGE DeriveFunctor
           , TypeOperators
           , FlexibleContexts
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.DUBLTree
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Rose (n-way) trees with both upwards- and downwards-traveling
-- monoidal annotations, used as the basis for representing diagrams.
--
-----------------------------------------------------------------------------
module Graphics.Rendering.Diagrams.DUBLTree
       (
         -- * DUBL-trees
         DUBLTree(..)

         -- * Constructing DUBL-trees
       , leaf, branch, branchGen

         -- * Modifying DUBL-trees
       , applyD, applyUpre, applyUpost, mapU

         -- * Accessors and destructors
       , getU, getU', foldDUBL, flatten

       ) where

import           Data.Functor ((<$>))
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Semigroup

import           Graphics.Rendering.Diagrams.Monoids
import           Graphics.Rendering.Diagrams.MList

-- | Abstractly, a DUBLTree is a rose (n-way) tree with data at the
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

data DUBLTree d u b l
    = Empty
    | Leaf u l
    | Branch (Option d) (Option u) (Maybe b) [DUBLTree d u b l]
  deriving (Functor)

oNothing = Option Nothing
oJust    = Option . Just

-- | @DUBLTree@s form a semigroup where @(\<\>)@ corresponds to
--   adjoining two trees under a common parent root.  Note that this
--   does not satisfy associativity up to structural equality, but up
--   to observational equivalence.  XXX @sconcat@ is specialized to
--   put all the trees under a single parent.
instance (Action d u, Semigroup u) => Semigroup (DUBLTree d u b l) where
  Empty <> t = t
  t <> Empty = t
  t1 <> t2   = branchGen [t1,t2]
  sconcat    = branchGen . NEL.toList

-- | @DUBLTree@s form a monoid where @mappend@ corresponds to adjoining
--   two trees under a common parent root. Note that this does not
--   satisfy associativity up to structural equality, but up to
--   observational equivalence. XXX @mconcat@ is specialized to put all
--   the trees under a single parent.
instance (Action d u, Semigroup u) => Monoid (DUBLTree d u b l) where
  mempty            = Empty
  mappend           = (<>)
  mconcat           = branchGen

-- | Construct a leaf node from a @u@ annotations along with a leaf
--   datum.
leaf :: u -> l -> DUBLTree d u b l
leaf = Leaf

-- | Construct a branch node with an explicit @d@ annotation and @b@ datum.
branch :: (Action d u, Semigroup u)
       => Option d -> Maybe b -> [DUBLTree d u b l] -> DUBLTree d u b l
branch d b ts = Branch d (mconcat . map getU $ ts) b ts

-- | Construct a generic branch node, with a default (identity) @d@
--   annotation and no @b@ datum.
branchGen :: (Action d u, Semigroup u) => [DUBLTree d u b l] -> DUBLTree d u b l
branchGen = branch oNothing Nothing

-- | Get the @u@ annotation at the root, or @Nothing@ if the tree is empty.
getU :: (Action d u) => DUBLTree d u b l -> Option u
getU Empty                            = oNothing
getU (Leaf u _)                       = oJust u
getU (Branch (Option Nothing) u _ _)  = u
getU (Branch (Option (Just d)) u _ _) = act d <$> u

-- | Get a particular component from a the @u@ annotation at the root.
--   This method is provided for convenience, since its context only
--   requires an action of @d@ on @u'@, rather than on @u@ in its
--   entirety.
getU' :: (Monoid u', Action d (u' ::: Nil), u :>: u') => DUBLTree d u b l -> u'
getU' Empty                            = mempty
getU' (Leaf u _)                       = get u
getU' (Branch (Option Nothing)  u _ _) = fromMaybe mempty . getOption $ get <$> u
getU' (Branch (Option (Just d)) (Option Nothing) _ _)  = mempty
getU' (Branch (Option (Just d)) (Option (Just u)) _ _) = hd $ act d (get u ::: Nil)
  where hd (u' ::: Nil) = u'
        hd (Missing _)  = error "Impossible case in DUBLTree.getU' (hd)"

-- | Add a @d@ annotation to the root, combining it (on the left) with
--   any pre-existing @d@ annotation, and transforming all @u@
--   annotations by the action of @d@.
applyD :: (Action d u, Semigroup d)
       => d -> DUBLTree d u b l -> DUBLTree d u b l
applyD d (Branch d' u b ts) = Branch (oJust d <> d') u b ts
applyD d t                  = Branch (oJust d) (getU t) Nothing [t]

-- | Add a @u@ annotation to the root, combining it (on the left) with
--   the existing @u@ annotation.
applyUpre :: (Semigroup u, Action d u) => u -> DUBLTree d u b l -> DUBLTree d u b l
applyUpre u' Empty                            = Branch oNothing (oJust u') Nothing []
applyUpre u' (Leaf u l)                       = Leaf (u' <> u) l
applyUpre u' (Branch (Option Nothing) u b ts) = Branch oNothing (oJust u' <> u) b ts
applyUpre u' b                                = Branch oNothing (oJust u' <> getU b) Nothing [b]

-- | Add a @u@ annotation to the root, combining it (on the right) with
--   the existing @u@ annotation.
applyUpost u' Empty                            = Branch oNothing (oJust u') Nothing []
applyUpost u' (Leaf u l)                       = Leaf (u <> u') l
applyUpost u' (Branch (Option Nothing) u b ts) = Branch oNothing (u <> oJust u') b ts
applyUpost u' b                                = Branch oNothing (getU b <> oJust u') Nothing [b]

-- | Map a function over all the @u@ annotations.  The function must
--   be a monoid homomorphism, and must commute with the action of @d@
--   on @u@.  That is, to use @mapU f@ safely it must be the case that
--   @f (act d u) == act d (f u)@.
mapU :: (u -> u') -> DUBLTree d u b l -> DUBLTree d u' b l
mapU f Empty             = Empty
mapU f (Leaf u l)        = Leaf (f u) l
mapU f (Branch d u b ts) = Branch d (f <$> u) b (map (mapU f) ts)

-- | A fold for DUBLTrees.
foldDUBL :: (Semigroup d, Action d u)
      => (Option d -> u -> l -> r)
         -- ^ Function for processing leaf nodes. Given the 'mconcat'
         --   of all d annotations above this node, the u annotation
         --   at this node, and the leaf datum.

      -> (Option d -> Option u -> Maybe b -> [Maybe r] -> r)
         -- ^ Function for processing internal nodes.  Given the d and
         --   u annotations at this node, the b datum, and the
         --   recursive results.
      -> DUBLTree d u b l -> Maybe r
foldDUBL = foldDUBL' oNothing     -- Pass along accumulated d value
  where foldDUBL' _ _  _ Empty = Nothing
        foldDUBL' dacc lf _ (Leaf u l)
          = Just $ lf dacc (act dacc u) l
        foldDUBL' dacc lf br (Branch d u b ts)
          = Just $ br d (act dacc' <$> u) b (map (foldDUBL' dacc' lf br) ts)
         where dacc' = dacc <> d

-- | A specialized fold provided for convenience: flatten a tree into
--   a list of leaves along with their @d@ annotations.
flatten :: (Semigroup d, Action d u) => DUBLTree d u b l -> [(l, Option d)]
flatten = fromMaybe [] . foldDUBL (\d _ l -> [(l,d)]) (\_ _ _ -> concat . catMaybes)