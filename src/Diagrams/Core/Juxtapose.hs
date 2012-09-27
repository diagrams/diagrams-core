{-# LANGUAGE FlexibleContexts
           , UndecidableInstances
           , TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Juxtapose
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Things which can be placed \"next to\" other things, for some
-- appropriate notion of \"next to\".
--
-----------------------------------------------------------------------------

module Diagrams.Core.Juxtapose
       ( Juxtaposable(..), juxtaposeDefault
       ) where

import           Data.Functor ((<$>))
import qualified Data.Map as M
import qualified Data.Set as S

import           Data.VectorSpace

import           Diagrams.Core.Envelope
import           Diagrams.Core.HasOrigin
import           Diagrams.Core.V

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
--   instances of 'Enveloped' and 'HasOrigin'.  If either envelope is
--   empty, the second object is returned unchanged.
juxtaposeDefault :: (Enveloped a, HasOrigin a) => V a -> a -> a -> a
juxtaposeDefault v a1 a2 =
  case (mv1, mv2) of
    (Just v1, Just v2) -> moveOriginBy (v1 ^+^ v2) a2
    _                  -> a2
  where mv1 = negateV <$> envelopeVMay v a1
        mv2 = envelopeVMay (negateV v) a2

instance (InnerSpace v, OrderedField (Scalar v)) => Juxtaposable (Envelope v) where
  juxtapose = juxtaposeDefault

instance (Enveloped a, HasOrigin a, Enveloped b, HasOrigin b, V a ~ V b)
         => Juxtaposable (a,b) where
  juxtapose = juxtaposeDefault

instance (Enveloped b, HasOrigin b) => Juxtaposable [b] where
  juxtapose = juxtaposeDefault

instance (Enveloped b, HasOrigin b) => Juxtaposable (M.Map k b) where
  juxtapose = juxtaposeDefault

instance (Enveloped b, HasOrigin b, Ord b) => Juxtaposable (S.Set b) where
  juxtapose = juxtaposeDefault