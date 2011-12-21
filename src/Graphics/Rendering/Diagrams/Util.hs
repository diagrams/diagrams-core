{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Util
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Various internal utilities for the diagrams project.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Util
       (
         -- * Vectors

         withLength

       ) where

import Data.VectorSpace

-- | Produce a vector with the specified length in the same direction
--   as the given vector.
withLength :: (InnerSpace v, Floating (Scalar v)) => Scalar v -> v -> v
withLength l v = (l / magnitude v) *^ v