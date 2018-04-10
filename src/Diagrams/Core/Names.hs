{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
  -- for Data.Semigroup import, which becomes redundant under GHC 8.4

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Names
-- Copyright   :  (c) 2011-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines a type of names which can be used for referring
-- to subdiagrams, and related types.
--
-----------------------------------------------------------------------------

module Diagrams.Core.Names
  (-- * Names
   -- ** Atomic names
    AName(..)
  , _AName

   -- ** Names
  , Name(..)
  , IsName(..)
  , (.>)
  , eachName

   -- ** Qualifiable
  , Qualifiable(..)

  ) where

import           Control.Lens            hiding ((.>))
import qualified Data.Map                as M
import           Data.Semigroup
import qualified Data.Set                as S
import           Data.Typeable

import           Diagrams.Core.Transform
import           Diagrams.Core.Measure

------------------------------------------------------------
--  Names  -------------------------------------------------
------------------------------------------------------------

-- | Class for those types which can be used as names.  They must
--   support 'Typeable' (to facilitate extracting them from
--   existential wrappers), 'Ord' (for comparison and efficient
--   storage) and 'Show'.
--
--   To make an instance of 'IsName', you need not define any methods,
--   just declare it.
--
--   WARNING: it is not recommended to use
--   @GeneralizedNewtypeDeriving@ in conjunction with @IsName@, since
--   in that case the underlying type and the @newtype@ will be
--   considered equivalent when comparing names.  For example:
--
--   @
--     newtype WordN = WordN Int deriving (Show, Ord, Eq, Typeable, IsName)
--   @
--
--   is unlikely to work as intended, since @(1 :: Int)@ and @(WordN 1)@
--   will be considered equal as names.  Instead, use
--
--   @
--     newtype WordN = WordN Int deriving (Show, Ord, Eq, Typeable, IsName)
--     instance IsName WordN
--   @
class (Typeable a, Ord a, Show a) => IsName a where
  toName :: a -> Name
  toName = Name . (:[]) . AName

instance IsName ()
instance IsName Bool
instance IsName Char
instance IsName Int
instance IsName Float
instance IsName Double
instance IsName Integer
instance IsName a => IsName [a]
instance IsName a => IsName (Maybe a)
instance (IsName a, IsName b) => IsName (a,b)
instance (IsName a, IsName b, IsName c) => IsName (a,b,c)

-- | Atomic names.  @AName@ is just an existential wrapper around
--   things which are 'Typeable', 'Ord' and 'Show'.
data AName where
  AName :: (Typeable a, Ord a, Show a) => a -> AName
  deriving Typeable

instance IsName AName where
  toName = Name . (:[])

instance Eq AName where
  AName a1 == AName a2 =
    case cast a2 of
      Nothing  -> False
      Just a2' -> a1 == a2'

instance Ord AName where
  AName a1 `compare` AName a2 =
    case cast a2 of
      Just a2' -> a1 `compare` a2'
      Nothing  -> typeOf a1 `compare` typeOf a2

instance Show AName where
  showsPrec d (AName a) = showParen (d > 10) $
    showString "AName " . showsPrec 11 a

-- | Prism onto 'AName'.
_AName :: (Typeable a, Ord a, Show a) => Prism' AName a
_AName = prism' AName (\(AName a) -> cast a)

-- | A (qualified) name is a (possibly empty) sequence of atomic names.
newtype Name = Name [AName]
  deriving (Eq, Ord, Semigroup, Monoid, Typeable)

instance Rewrapped Name Name
instance Wrapped Name where
  type Unwrapped Name = [AName]
  _Wrapped' = iso (\(Name ns) -> ns) Name

instance Each Name Name AName AName where
  each = _Wrapped . traversed
  {-# INLINE each #-}

-- | Traversal over each name in a 'Name' that matches the target type.
--
-- @
-- >>> toListOf eachName ('a' .> False .> 'b') :: String
-- "ab"
-- >>> 'a' .> True .> 'b' & eachName %~ not
-- 'a' .> False .> 'b'
-- @
--
-- Note that the type of the name is very important.
--
-- @
-- >>> sumOf eachName ((1::Int) .> (2 :: Integer) .> (3 :: Int)) :: Int
-- 4
-- >>> sumOf eachName ((1::Int) .> (2 :: Integer) .> (3 :: Int)) :: Integer
-- 2
-- @
eachName :: (Typeable a, Ord a, Show a) => Traversal' Name a
eachName = each . _AName

instance Show Name where
  showsPrec d (Name xs) = case xs of
    []     -> showParen (d > 10) $ showString "toName []"
    [n]    -> showParen (d > 10) $ showString "toName " . showsName 11 n
    (n:ns) -> showParen (d > 5)  $ showsName 6 n . go ns
      where
        go (y:ys) = showString " .> " . showsName 6 y . go ys
        go _      = id
    where showsName dd (AName a) = showsPrec dd a

instance IsName Name where
  toName = id

-- | Convenient operator for writing qualified names with atomic
--   components of different types.  Instead of writing @toName a1 \<\>
--   toName a2 \<\> toName a3@ you can just write @a1 .> a2 .> a3@.
(.>) :: (IsName a1, IsName a2) => a1 -> a2 -> Name
a1 .> a2 = toName a1 <> toName a2

-- | Instances of 'Qualifiable' are things which can be qualified by
--   prefixing them with a name.
class Qualifiable q where
  -- | Qualify with the given name.
  (.>>) :: IsName a => a -> q -> q

-- | Of course, names can be qualified using @(.>)@.
instance Qualifiable Name where
  (.>>) = (.>)

instance Qualifiable a => Qualifiable (TransInv a) where
  (.>>) n = over (_Unwrapping' TransInv) (n .>>)

instance (Qualifiable a, Qualifiable b) => Qualifiable (a,b) where
  n .>> (a,b) = (n .>> a, n .>> b)

instance (Qualifiable a, Qualifiable b, Qualifiable c) => Qualifiable (a,b,c) where
  n .>> (a,b,c) = (n .>> a, n .>> b, n .>> c)

instance Qualifiable a => Qualifiable [a] where
  n .>> as = map (n .>>) as

instance (Ord a, Qualifiable a) => Qualifiable (S.Set a) where
  n .>> s = S.map (n .>>) s

instance Qualifiable a => Qualifiable (M.Map k a) where
  n .>> m = fmap (n .>>) m

instance Qualifiable a => Qualifiable (b -> a) where
  n .>> f = (n .>>) . f

instance Qualifiable a => Qualifiable (Measured n a) where
  n .>> m = fmap (n .>>) m

infixr 5 .>>
infixr 5 .>
