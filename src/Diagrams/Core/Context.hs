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
  ( Context
  , Contextual (..)
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

-- | The (monoidal) context in which a diagram is interpreted.
--   Contexts can be thought of as accumulating along each path to a
--   leaf:
--
--   * styles (see "Diagrams.Core.Style")
--
--   * names (see "Diagrams.Core.Names")
type Context v n = Style v n
               ::: Name
               ::: (n, n) -- ^ global to output, normalized to output.
               ::: ()

--------------------------------------------------
-- Context monad

newtype Contextual v n a = Contextual (Reader (Context v n) a)
  deriving (Functor, Applicative, Monad, MonadReader (Context v n))

instance Wrapped (Contextual v n a) where
  type Unwrapped (Contextual v n a) = Context v n -> a
  _Wrapped' = iso (\(Contextual r) -> runReader r) (Contextual . reader)

-- | Smart constructor for 'Contextual' values.
--
--   Note @contextual = review _Wrapped'@.
contextual :: (Context v n -> a) -> Contextual v n a
contextual = Contextual . reader

runContextual :: Contextual v n a -> (Context v n -> a)
runContextual = view _Wrapped'

instance Rewrapped (Contextual v n a) (Contextual v' n' a')

type instance V (Contextual v n a) = V a
type instance N (Contextual v n a) = N a

instance Semigroup a => Semigroup (Contextual v n a) where
  Contextual r1 <> Contextual r2 = Contextual . reader $ \ctx -> runReader r1 ctx <> runReader r2 ctx

instance (Semigroup a, Monoid a) => Monoid (Contextual v n a) where
  mappend = (<>)
  mempty  = Contextual . reader $ const mempty

instance Transformable a => Transformable (Contextual v n a) where
  transform = over _Wrapped' . fmap . transform

