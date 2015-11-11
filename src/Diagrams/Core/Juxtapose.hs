{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
       ( Juxtaposable(..), juxtaposeDefault, juxtaposeContextual
       ) where

import           Control.Applicative
import qualified Data.Map                as M
import           Data.Semigroup
import qualified Data.Set                as S

import           Diagrams.Core.Context
import           Diagrams.Core.Envelope
import           Diagrams.Core.Measure
import           Diagrams.Core.HasOrigin
import           Diagrams.Core.V

import           Linear.Metric
import           Linear.Vector

-- | Class of things which can be placed \"next to\" other things, for some
--   appropriate notion of \"next to\".
class Juxtaposable a where

  -- | @juxtapose v a1 a2@ positions @a2@ next to @a1@ in the
  --   direction of @v@.  In particular, place @a2@ so that @v@ points
  --   from the local origin of @a1@ towards the old local origin of
  --   @a2@; @a1@'s local origin becomes @a2@'s new local origin.  The
  --   result is just a translated version of @a2@.  (In particular,
  --   this operation does not /combine/ @a1@ and @a2@ in any way.)
  juxtapose :: Vn a -> a -> a -> Contextual (V a) (N a) a

-- | Default implementation of 'juxtapose' for things which are
--   instances of 'Enveloped' and 'HasOrigin'.  If either envelope is
--   empty, the second object is returned unchanged.
juxtaposeDefault :: (Enveloped a, HasOrigin a) => Vn a -> a -> a -> Contextual (V a) (N a) a
juxtaposeDefault v a1 a2 = do
  mv1 <- (fmap . fmap) negated (envelopeVMay v a1)
  mv2 <- envelopeVMay (negated v) a2
  case (mv1, mv2) of
    (Just v1, Just v2) -> return $ moveOriginBy (v1 ^+^ v2) a2
    _                  -> return a2

juxtaposeContextual :: (Juxtaposable a, HasOrigin a, Semigroup a)
                    => Vn a -> Contextual (V a) (N a) a -> Contextual (V a) (N a) a
                            -> Contextual (V a) (N a) a
juxtaposeContextual v c1 c2 = contextual $ \ctx -> 
  runContextual (juxtapose v (runContextual c1 ctx) 
                             (runContextual c2 ctx)) ctx

instance (Metric v, OrderedField n) => Juxtaposable (Envelope v n) where
  juxtapose = juxtaposeDefault

instance (Enveloped a, HasOrigin a, Enveloped b, HasOrigin b, V a ~ V b, N a ~ N b)
         => Juxtaposable (a,b) where
  juxtapose = juxtaposeDefault

instance (Enveloped b, HasOrigin b) => Juxtaposable [b] where
  juxtapose = juxtaposeDefault

instance (Enveloped b, HasOrigin b) => Juxtaposable (M.Map k b) where
  juxtapose = juxtaposeDefault

instance (Enveloped b, HasOrigin b, Ord b) => Juxtaposable (S.Set b) where
  juxtapose = juxtaposeDefault

instance Juxtaposable a => Juxtaposable (b -> a) where
  juxtapose v f1 f2 = contextual $ \ctx b -> runContextual (juxtapose v (f1 b) (f2 b)) ctx

instance Juxtaposable a => Juxtaposable (Measured n a) where
  juxtapose = juxtapose

