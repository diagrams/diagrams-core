{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Context
-- Copyright   :  (c) 2014 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- XXX comment me
--
-----------------------------------------------------------------------------

module Diagrams.Core.Context
  ( Contextual (..)
--, Context
  , contextual
  , runContextual
  )
  where

import           Diagrams.Core.Names
import           Diagrams.Core.Style
import           Diagrams.Core.Transform
import           Diagrams.Core.V

import           Control.Applicative
import           Control.Lens            (Rewrapped, Wrapped (..), iso, over,
                                          view)
import           Control.Monad.Reader
import           Data.Monoid.MList
import           Data.Semigroup

{-
-- | The (monoidal) context in which a diagram is interpreted.
--   Contexts can be thought of as accumulating along each path to a
--   leaf:
--
--   * styles (see "Diagrams.Core.Style")
--
--   * names (see "Diagrams.Core.Names")
type Context v = Style v
             ::: Name
             ::: ()
-}

--------------------------------------------------
-- Context monad

newtype Contextual c a = Contextual (Reader c a)
  deriving (Functor, Applicative, Monad, MonadReader c)

instance Wrapped (Contextual c a) where
  type Unwrapped (Contextual c a) = c -> a
  _Wrapped' = iso (\(Contextual r) -> runReader r) (Contextual . reader)

-- | Smart constructor for 'Contextual' values.
--
--   Note @contextual = review _Wrapped'@.
contextual :: (c -> a) -> Contextual c a
contextual = Contextual . reader

runContextual :: Contextual c a -> (c -> a)
runContextual = view _Wrapped'

instance Rewrapped (Contextual c a) (Contextual c' a')

type instance V (Contextual c a) = V a

instance Semigroup a => Semigroup (Contextual c a) where
  Contextual r1 <> Contextual r2 = Contextual . reader $ \ctx -> runReader r1 ctx <> runReader r2 ctx

instance (Semigroup a, Monoid a) => Monoid (Contextual c a) where
  mappend = (<>)
  mempty  = Contextual . reader $ const mempty

instance Transformable a => Transformable (Contextual c a) where
  transform = over _Wrapped' . fmap . transform
