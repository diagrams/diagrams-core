{-# LANGUAGE DeriveFunctor
           , TypeOperators
           , FlexibleContexts
  #-}

-- XXX add comments/header etc.
module Graphics.Rendering.Diagrams.UDTree where

import Data.Monoid
import Control.Arrow

import Graphics.Rendering.Diagrams.Monoids
import Graphics.Rendering.Diagrams.MList
import Graphics.Rendering.Diagrams.Util

import qualified Data.Set as S

-- Abstractly, a UDTree is a rose tree with two types of monoidal
-- annotations.  Each leaf contains a piece of data and an annotation
-- of type u.  Each internal node holds an annotation of type d.  At
-- every internal node, we can also access the mconcat (from left to
-- right) of all the u annotations below it.  Likewise, at every leaf
-- node, we can access the mconcat (from top to bottom) of all the d
-- annotations above it along the path from the root.

-- XXX write something about action of d annotations on u.

-- XXX cached u values at nodes can have "extra stuff" appended to them.

data UDTree u d a
  = Leaf u a
  | Branch u [d] [UDTree u d a]
  deriving (Functor)

-- | @UDTree@s form a monoid where @mappend@ corresponds to adjoining
--   two trees under a common parent root.  Note that this technically
--   does not satisfy associativity, but it does with respect to
--   'flatten' which is what we really care about.  @mconcat@ is
--   specialized to put all the trees under a single parent.
instance (Action d u, Monoid u, Monoid d) => Monoid (UDTree u d a) where
  mempty          = Branch mempty mempty []
  t1 `mappend` t2 = branch [t1,t2]
  mconcat         = branch

-- | Construct a leaf node.
leaf :: u -> a -> UDTree u d a
leaf = Leaf

-- | Construct a branch node with an explicit @d@ annotation.
branchD :: (Action d u, Monoid u) => d -> [UDTree u d a] -> UDTree u d a
branchD d ts = Branch (mconcat . map getU $ ts) [d] ts

-- | Construct a branch node with a default (identity) @d@ annotation.
branch :: (Action d u, Monoid u, Monoid d) => [UDTree u d a] -> UDTree u d a
branch ts = Branch (mconcat . map getU $ ts) [] ts

-- | Get the @u@ value summarizing all leaf @u@ annotations (from left
--   to right).
getU :: Action d u => UDTree u d a -> u
getU (Leaf u _)      = u
getU (Branch u ds _) = foldr act u ds

-- | Get a particular value from a @u@ value with several components.
getU' :: (Action d (u' ::: Nil), u :>: u') => UDTree u d a -> u'
getU' (Leaf u _)      = get u
getU' (Branch u ds _) = hd $ foldr act (get u ::: Nil) ds
  where hd (u' ::: Nil) = u'

-- | Add a @d@ annotation to the root, combining it (on the left) with
--   any pre-existing @d@ annotation.
applyD :: Action d u => d -> UDTree u d a -> UDTree u d a
applyD d l@(Leaf {})      = Branch (getU l) [d] [l]
applyD d (Branch u ds ts) = Branch u (d : ds) ts

-- | Add a @u@ annotation to the root, combining it (on the left) with
--   the existing @u@ annotation.
applyU :: (Monoid u, Action d u) => u -> UDTree u d a -> UDTree u d a
applyU u' (Leaf u a) = Leaf (u' <> u) a
applyU u' b          = Branch (u' <> getU b) [] [b]

-- | Map a function over all the @u@ annotations.  The function must
--   be a monoid homomorphism, and must commute with the action of @d@
--   on @u@.  That is, to use @mapU f@ safely it must be the case that
--   @f (act d u) == act d (f u)@.
mapU :: (u -> u') -> UDTree u d a -> UDTree u' d a
mapU f (Leaf u a)       = Leaf (f u) a
mapU f (Branch u ds ts) = Branch (f u) ds (map (mapU f) ts)

-- | A fold for UDTrees.
foldUD :: (Monoid r, Monoid d, Action d u)
      => (u -> d -> a -> r)  -- ^ Function for processing leaf nodes.
                             --   Given the u annotation at this node, the
                             --   mconcat of all d annotations above, and the
                             --   leaf value.
      -> (u -> d -> r -> r)  -- ^ Function for processing internal nodes.
                             --   Given the mconcat of all u annotations below,
                             --   the d annotation at this node, and the mconcat
                             --   of the recursive results.
      -> UDTree u d a -> r
foldUD = foldUD' mempty     -- Pass along accumulated d value
  where foldUD' d l _ (Leaf u a)
          = l (act d u) d a
        foldUD' d l b (Branch u ds ts)
          = b (act (d <> d') u) d' (mconcat $ map (foldUD' (d <> d') l b) ts)
         where d' = mconcat ds

-- | Flatten a tree into a list of leaves along with their @d@ annotations.
flatten :: (Monoid d, Action d u) => UDTree u d a -> [(a,d)]
flatten = foldUD (\_ d a -> [(a,d)]) (\_ _ r -> r)