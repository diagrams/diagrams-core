{-# LANGUAGE TypeFamilies #-}

module Graphics.Rendering.Diagrams.V where

import Data.Set
import Data.Map

------------------------------------------------------------
-- Vector spaces -------------------------------------------
------------------------------------------------------------

-- | Many sorts of objects have an associated vector space in which
--   they live.  The type function @V@ maps from objects to their
--   associated vector space.
type family V a :: *

type instance V [a]       = V a
type instance V (Set a)   = V a
type instance V (Map k a) = V a
