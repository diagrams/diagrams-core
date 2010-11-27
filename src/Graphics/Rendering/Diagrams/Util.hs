-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Util
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
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