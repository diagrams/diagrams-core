{-# LANGUAGE FlexibleContexts
           , UndecidableInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Juxtapose
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Things which can be placed \"next to\" other things, for some
-- appropriate notion of \"next to\".
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Juxtapose
       ( Juxtaposable(..), juxtaposeDefault
       ) where

import Graphics.Rendering.Diagrams.V
import Graphics.Rendering.Diagrams.Envelope
import Graphics.Rendering.Diagrams.HasOrigin

import qualified Data.Map as M

import Data.VectorSpace

-- | Class of things which can be placed \"next to\" other things, for some
--   appropriate notion of \"next to\".
class Juxtaposable a where

  -- | @juxtapose v a1 a2@ positions @a2@ next to @a1@ in the
  --   direction of @v@.  In particular, place @a2@ so that @v@ points
  --   from the local origin of @a1@ towards the old local origin of
  --   @a2@; @a1@'s local origin becomes @a2@'s new local origin.  The
  --   result is just a translated version of @a2@.  (In particular,
  --   this operation does not /combine/ @a1@ and @a2@ in any way.)
  juxtapose :: V a -> a -> a -> a

-- | Default implementation of 'juxtapose' for things which are
--   instances of 'Enveloped' and 'HasOrigin'.
juxtaposeDefault :: (Enveloped a, HasOrigin a) => V a -> a -> a -> a
juxtaposeDefault v a1 a2 = moveOriginBy (v1 ^+^ v2) a2
  where v1 = negateV (envelopeV v a1)
        v2 = envelopeV (negateV v) a2

instance (InnerSpace v, OrderedField (Scalar v)) => Juxtaposable (Envelope v) where
  juxtapose = juxtaposeDefault

instance (Enveloped b, HasOrigin b) => Juxtaposable [b] where
  juxtapose = juxtaposeDefault

instance (Enveloped b, HasOrigin b) => Juxtaposable (M.Map k b) where
  juxtapose = juxtaposeDefault