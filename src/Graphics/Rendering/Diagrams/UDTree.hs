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


-- First, a straightforward, inefficient reference implementation.

data UDTree u d a
  = Leaf u a
  | Branch d [UDTree u d a]

leaf :: u -> a -> UDTree u d a
leaf = Leaf

branchD :: d -> [UDTree u d a] -> UDTree u d a
branchD = Branch

branch :: Monoid d => [UDTree u d a] -> UDTree u d a
branch = branchD mempty

getU :: Monoid u => UDTree u d a -> u
getU (Leaf u a) = u
getU (Branch _ ts) = mconcat (map getU ts)

applyD :: Monoid d => d -> UDTree u d a -> UDTree u d a
applyD d l@(Leaf {}) = branchD d [l]
applyD d (Branch d' ts) = Branch (d `mappend` d') ts

-- | Map a function over all the @u@ annotations.  The function must
--   be a monoid homomorphism.
mapU :: (u -> u) -> UDTree u d a -> UDTree u d a
mapU f (Leaf u a)    = Leaf (f u) a
mapU f (Branch d ts) = Branch d (map (mapU f) ts)

-- | Map a function over all the @d@ annotations.  The function must
--   be a monoid homomorphism.
mapD :: (d -> d) -> UDTree u d a -> UDTree u d a
mapD f l@(Leaf _ _)  = l
mapD f (Branch d ts) = Branch (f d) (map (mapD f) ts)

-- | A fold for UDTrees.
foldUD :: (Monoid r, Monoid u, Monoid d)
      => (u -> d -> r -> r)  -- ^ Function for processing internal nodes.
                             --   Given the mconcat of all u annotations below,
                             --   the d annotation at this node, and the mconcat
                             --   of the recursive results.
      -> (u -> d -> a -> r)  -- ^ Function for processing leaf nodes.
                             --   Given the u annotation at this node, the
                             --   mconcat of all d annotations above, and the
                             --   leaf value.
      -> UDTree u d a -> r
foldUD = foldUD' mempty     -- Pass along accumulated d value
  where foldUD' d _ l (Leaf u a)       = l u d a
        foldUD' d b l t@(Branch d' ts) = b (getU t) d'
                                           (mconcat $ map (foldUD' (d `mappend` d') b l) ts)

-- | Flatten a tree into a list of leaves along with their @d@ annotations.
flatten :: (Monoid u, Monoid d) => UDTree u d a -> [(a,d)]
flatten = foldUD (\_ _ r -> r) (\_ d a -> [(a,d)])


-- A more sophisticated implementation.
{-
data UDTree u d a
  = Leaf u a                       -- ^ A piece of data and u
                                   --   annotation.
  | Branch u [UDTree u d a]        -- ^ A branch point, with a cached
                                   --   @u@ value.
  | UMap (u -> u) (UDTree u d a)   -- ^ A delayed @u@ homomorphism.
  | DMap (d -> d) (UDTree u d a)   -- ^ A delayed @d@ homomorphism.
  | DNode d u (UDTree u d a)       -- ^ A specified @d@ value, with
                                   --   cached @u@ value.

-- | Get the @u@ annotation at the root of a tree.
getU :: Monoid u => UDTree u d a -> u
getU (Leaf u _)    = u
getU (Branch  u _) = u
getU (UMap _ t)    = getU t
getU (DMap _ t)    = getU t
getU (DNode _ u _) = u

-- | Create a branch node with the given children, whose @d@
--   annotation is @mempty@.
branch :: Monoid u => [UDTree u d a] -> UDTree u d a
branch ts = Branch (mconcat . map getU $ ts) ts

-- | Apply a new @d@ annotation at the root, combining it (on the
--   left) with the existing @d@ annotation.
applyD :: Monoid u => d -> UDTree u d a -> UDTree u d a
applyD d t = DNode d (getU t) t

-- | Map a function over all the @u@ annotations.  The function must
--   be a monoid homomorphism.
mapU :: (u -> u) -> UDTree u d a -> UDTree u d a
mapU = UMap

-- | Map a function over all the @d@ annotations.  The function must
--   be a monoid homomorphism.
mapD :: (d -> d) -> UDTree u d a -> UDTree u d a
mapD f t = DMap f t

t = applyD (Product 5) $ branch [Leaf (Sum 2) 'x', Leaf (Sum 4) 'y', applyD (Product 2) $ Leaf (Sum 5) 'z']

t2 = branch [mapU (const $ Sum 6) t, Leaf (Sum 1) 'q']

foldUD :: (Monoid r, Monoid d)
      => (u -> d -> r -> r)
      -> (u -> d -> a -> r)
      -> UDTree u d a -> r
foldUD = foldUD' mempty id id

foldUD' :: (Monoid r, Monoid d)
       => d -> (u -> u) -> (d -> d)
       -> (u -> d -> r -> r) -> (u -> d -> a -> r) -> UDTree u d a -> r
foldUD' d fu fd b l (Leaf u a)     = l (fu u) (fd d) a
foldUD' d fu fd b l (Branch u ts)  = b (fu u) (fd d) (mconcat $ map (foldUD' d fu fd b l) ts)
foldUD' d fu fd b l (UMap f t)     = foldUD' d (fu . f) fd b l t
foldUD' d fu fd b l (DMap f t)     = foldUD' d fu (fd . f) b l t
foldUD' d fu fd b l (DNode d' _ t) = foldUD' (d `mappend` fd d') fu fd b l t
-- XXX wrong! Have to pass along current d value as well... =(

-- need to start over with a super-simple but inefficient
-- implementation, and then prove it equivalent to a better one.


-- Can we play the same sort of trick with a monotonic (u -> Bool)
-- function as with fingertrees?  Will the functions we want to use be
-- monotonic?  Maybe not, the main thing we want to search for is
-- names, and we'll probably want to return multiple subtrees.
--
-- data Result = Found
--             | KeepGoing
--             | Stop
--
-- findU :: (u -> Result) -> UDTree u d a -> [UDTree u d a]
-- findU test t@(Leaf u a) =
--   case test u of
--     Found -> [t]
--     _     -> []
-- findU test t@(Branch u ts) =
--   case test u of
--     Found     -> [t]
--     KeepGoing -> concatMap (findU test) ts
--     Stop      -> []
-- findU test t@(UNode f t') = findU (test . f) t'
-- findU test t@(DNode d u t') =
--   case test u of
--     Found     -> [t]
--     KeepGoing -> findU test t'
--     Stop      -> []
-}