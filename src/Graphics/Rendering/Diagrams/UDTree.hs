module Graphics.Rendering.Diagrams.UDTree where

import Data.Monoid
import Control.Arrow

import qualified Data.Set as S

data UDTree u d a = Leaf u a
                  | Branch u [UDTree u d a]
                  | UNode (u -> u) (UDTree u d a)
                  | DNode d u (UDTree u d a)

getU :: UDTree u d a -> u
getU (Leaf    u _) = u
getU (Branch  u _) = u
getU (UNode   f t) = f (getU t)
getU (DNode _ u _) = u

branch :: Monoid u => [UDTree u d a] -> UDTree u d a
branch ts = Branch (mconcat . map getU $ ts) ts

setD :: d -> UDTree u d a -> UDTree u d a
setD d t = DNode d (getU t) t

modifyU :: (u -> u) -> UDTree u d a -> UDTree u d a
modifyU f t = UNode f t

mapU :: (u -> u) -> UDTree u d a -> UDTree u d a
mapU f (Leaf u a) = Leaf (f u) a
mapU f (Branch u ts) = Branch (f u) (map (mapU f) ts)
mapU f (UNode g t) = UNode (f . g) t
mapU f (DNode d u t) = DNode d (f u) (mapU f t)

flatten :: Monoid d => UDTree u d a -> [(a,d)]
flatten (Leaf _ a) = [(a, mempty)]
flatten (Branch _ ts) = concatMap flatten ts
flatten (UNode _ t) = flatten t
flatten (DNode d _ t) = (map . second) (d `mappend`) (flatten t)

t = setD (Product 5) $ branch [Leaf (Sum 2) 'x', Leaf (Sum 4) 'y', setD (Product 2) $ Leaf (Sum 5) 'z']

t2 = branch [modifyU (const $ Sum 6) t, Leaf (Sum 1) 'q']

-- Some sort of fold for processing this?  Essentially we want to be
-- able to do something at each DNode and at each Leaf.  But we may
-- want to do something on the way through the DNode AND on the way
-- "back out", to restore something etc.  Ah, but that's OK, because
-- with the fold your function is passed the result from folding the
-- subtree, so you can append stuff both before and after it.

-- Maybe at each DNode you want
-- to be given the current accumulated d value as well as the d value
-- at the node itself, and for leaves you are given the accumulated d
-- value.

foldD :: (Monoid r, Monoid d)
      => (d -> d -> r -> r) -> (d -> a -> r) -> UDTree u d a -> r
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
