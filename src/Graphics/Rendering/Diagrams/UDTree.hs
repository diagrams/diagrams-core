module Graphics.Rendering.Diagrams.UDTree where

import Data.Monoid
import Control.Arrow

import qualified Data.Set as S

-- Abstractly, a UDTree is a rose tree with data at the leaves and two
-- monoidal annotations at every internal node, one of type u and one
-- of type d, such that
--
--   * every u annotation is equal to the mconcat of all u annotations
--     _below_ it in the tree (in order from left to right), and
--
--   * every d annotation is equal to the mconcat of all d annotations
--     _above_ it in the tree (in order from top to bottom).
--
-- When placing a new parent node over a list of subtrees, you may
-- provide a d annotation for it (otherwise it will be taken to be
-- mempty); all the d annotations on the rest of the tree will be
-- updated appropriately.
--
-- You may also map a function f over all the u or d annotations, as
-- long as f is a monoid homomorphism, that is, f (u1 <> u2) = f u1 <>
-- f u2.


-- For efficiency, we do not actually store d annotations at every
-- node, and we delay applying homomorphisms until actually descending
-- into the tree.

data UDTree u d a = Leaf a                         -- ^ A piece of data.
                  | Branch u [UDTree u d a]        -- ^ A branch point, with a cached
                                                   --   @u@ value.
                  | UMap (u -> u) (UDTree u d a)   -- ^ A delayed @u@ homomorphism.
                  | DMap (d -> d) u (UDTree u d a) -- ^ A delayed @d@ homomorphism,
                                                   --   with cached @u@ value copied
                                                   --   from below.
                  | DNode d u (UDTree u d a)       -- ^ A specified @d@ value,
                                                   --   with cached @u@ value.


-- | Get the @u@ annotation at the root of a tree.
getU :: Monoid u => UDTree u d a -> u
getU (Leaf    _)   = mempty
getU (Branch  u _) = u
getU (UMap  f t)   = f (getU t)
getU (DMap _ u _)  = u
getU (DNode _ u _) = u

-- | Create a branch node with the given children, whose @d@
--   annotation is @mempty@ and @u@ annotation is computed
--   automatically from the annotations on the children.
branch :: Monoid u => [UDTree u d a] -> UDTree u d a
branch ts = Branch (mconcat . map getU $ ts) ts

-- | Apply a new @d@ annotation at the root, combining it (on the
--   left) with the existing @d@ annotation.
applyD :: d -> UDTree u d a -> UDTree u d a
applyD d t = DNode d (getU t) t

-- | Map a function over all the @u@ annotations.  The function must
--   be a monoid homomorphism.
mapU :: (u -> u) -> UDTree u d a -> UDTree u d a
mapU f t = UMap f t

-- | Map a function over all the @d@ annotations.  The function must
--   be a monoid homomorphism.
mapD :: (d -> d) -> UDTree u d a -> UDTree u d a
mapD f t = DMap f t

-- | Flatten a tree into a list of leaves along with their @d@ annotations.
-- flatten :: Monoid d => UDTree u d a -> [(a,d)]
-- flatten (Leaf a) = [(a, mempty)]
-- flatten (Branch _ ts) = concatMap flatten ts
-- flatten (UMap _ t) = flatten t
-- flatten (DMap f _ t) = (map . second)
--   (map . second) (d `mappend`) (flatten t)

t = setD (Product 5) $ branch [Leaf (Sum 2) 'x', Leaf (Sum 4) 'y', setD (Product 2) $ Leaf (Sum 5) 'z']

t2 = branch [modifyU (const $ Sum 6) t, Leaf (Sum 1) 'q']


foldUD :: (Monoid r, Monoid d)
      => (u -> d -> d -> r -> r) -> UDTree u d a -> r
foldD = foldD' mempty

foldD' :: (Monoid r, Monoid d)
       => d -> (d -> d -> r -> r) -> (d -> a -> r) -> UDTree u d a -> r
foldD' d _ l (Leaf _ a)     = l d a
foldD' d n l (Branch _ ts)  = mconcat $ map (foldD' d n l) ts
foldD' d n l (UNode _ t)    = foldD' d n l t
foldD' d n l (DNode d' _ t) = n d d' (foldD' (d `mappend` d') n l t)

data Result = Found
            | KeepGoing
            | Stop

-- Can we play the same sort of trick with a monotonic (u -> Bool)
-- function as with fingertrees?  Will the functions we want to use be
-- monotonic?  Maybe not, the main thing we want to search for is
-- names, and we'll probably want to return multiple subtrees.
findU :: (u -> Result) -> UDTree u d a -> [UDTree u d a]
findU test t@(Leaf u a) =
  case test u of
    Found -> [t]
    _     -> []
findU test t@(Branch u ts) =
  case test u of
    Found     -> [t]
    KeepGoing -> concatMap (findU test) ts
    Stop      -> []
findU test t@(UNode f t') = findU (test . f) t'
findU test t@(DNode d u t') =
  case test u of
    Found     -> [t]
    KeepGoing -> findU test t'
    Stop      -> []
