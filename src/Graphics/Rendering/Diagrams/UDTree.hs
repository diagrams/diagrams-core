{-# LANGUAGE DeriveFunctor #-}

-- XXX add comments/header etc.
module Graphics.Rendering.Diagrams.UDTree where

import Data.Monoid
import Control.Arrow

import qualified Data.Set as S

-- Abstractly, a UDTree is a rose tree with two types of monoidal
-- annotations.  Each leaf contains a piece of data and an annotation
-- of type u.  Each internal node holds an annotation of type d.  At
-- every internal node, we can also access the mconcat (from left to
-- right) of all the u annotations below it.  Likewise, at every leaf
-- node, we can access the mconcat (from top to bottom) of all the d
-- annotations above it along the path from the root.
--
-- We may also map a function f over all the u or d annotations, as
-- long as f is a monoid homomorphism, that is, f (u1 <> u2) = f u1 <>
-- f u2.


-- The implementation is straightforward, except for the fact that we
-- cache u values at internal nodes.

data UDTree u d a
  = Leaf u a
  | Branch u d [UDTree u d a]
  deriving (Functor)

-- | Construct a leaf node.
leaf :: u -> a -> UDTree u d a
leaf = Leaf

-- | Construct a branch node with an explicit @d@ annotation.
branchD :: Monoid u => d -> [UDTree u d a] -> UDTree u d a
branchD d ts = Branch (mconcat . map getU $ ts) d ts

-- | Construct a branch node with a default (identity) @d@ annotation.
branch :: (Monoid u, Monoid d) => [UDTree u d a] -> UDTree u d a
branch = branchD mempty

-- | Get the @u@ value summarizing all leaf @u@ annotations (from left
--   to right).
getU :: Monoid u => UDTree u d a -> u
getU (Leaf u _)     = u
getU (Branch u _ _) = u

-- | Add a @d@ annotation to the root, combining it (on the left) with
--   any pre-existing @d@ annotation.
applyD :: (Monoid d, Monoid u) => d -> UDTree u d a -> UDTree u d a
applyD d l@(Leaf {}) = branchD d [l]
applyD d (Branch u d' ts) = Branch u (d `mappend` d') ts

-- | Map a function over all the @u@ annotations.  The function must
--   be a monoid homomorphism.
mapU :: (u -> u) -> UDTree u d a -> UDTree u d a
mapU f (Leaf u a)      = Leaf (f u) a
mapU f (Branch u d ts) = Branch (f u) d (map (mapU f) ts)

-- | Map a function over all the @d@ annotations.  The function must
--   be a monoid homomorphism.
mapD :: (d -> d) -> UDTree u d a -> UDTree u d a
mapD f l@(Leaf _ _)  = l
mapD f (Branch u d ts) = Branch u (f d) (map (mapD f) ts)

-- | A fold for UDTrees.
foldUD :: (Monoid r, Monoid u, Monoid d)
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
  where foldUD' d l _ (Leaf u a)       = l u d a
        foldUD' d l b (Branch u d' ts)
          = b u d' (mconcat $ map (foldUD' (d `mappend` d') l b) ts)

-- | Flatten a tree into a list of leaves along with their @d@ annotations.
flatten :: (Monoid u, Monoid d) => UDTree u d a -> [(a,d)]
flatten = foldUD (\_ d a -> [(a,d)]) (\_ _ r -> r)