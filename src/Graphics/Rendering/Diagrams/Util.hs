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
       ( (<>)

       ) where

import Data.Monoid

-- | A useful infix operator synonym for 'mappend'.  Hopefully it will
--   eventually be added to the standard libraries and can be deleted
--   from here.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

infixl 6 <>