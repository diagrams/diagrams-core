{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- Should probably move Wrapped instance upstream
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Points
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A type for /points/ (as distinct from vectors).
--
-----------------------------------------------------------------------------

module Diagrams.Core.Points
       ( -- * Points

         Point(..), origin, (*.)

       ) where

import Control.Lens (Wrapped(..), iso)

-- We just import from Data.AffineSpace.Point (defined in the
-- vector-space-points package) and re-export.  We also define an
-- instance of V for Point here.
import Data.AffineSpace.Point

import Diagrams.Core.V

type instance V (Point v) = v

instance Wrapped v v' (Point v) (Point v') where
    wrapped = iso P unPoint
