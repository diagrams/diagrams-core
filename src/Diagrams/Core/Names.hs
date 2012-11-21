{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverlappingInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Names
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines a type of names which can be used for referring
-- to locations within diagrams, and related types.
--
-----------------------------------------------------------------------------

module Diagrams.Core.Names
       (-- * Names
        -- ** Atomic names
         AName(..)

        -- ** Names
       , Name(..), IsName(..), (.>)

        -- ** Qualifiable
       , Qualifiable(..)

       ) where

import Data.List      ( intercalate )
import Data.Semigroup
import Data.Typeable

------------------------------------------------------------
--  Names  -------------------------------------------------
------------------------------------------------------------

-- | Class for those types which can be used as names.  They must
--   support 'Typeable' (to facilitate extracting them from
--   existential wrappers), 'Ord' (for comparison and efficient
--   storage) and 'Show'.
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
instance IsName String
instance IsName a => IsName [a]
instance (IsName a, IsName b) => IsName (a,b)
instance (IsName a, IsName b, IsName c) => IsName (a,b,c)

-- | Atomic names.  @AName@ is just an existential wrapper around
--   things which are 'Typeable', 'Ord' and 'Show'.
data AName where
  AName :: (Typeable a, Ord a, Show a) => a -> AName
  deriving (Typeable)

instance IsName AName where
  toName = Name . (:[])

instance Eq AName where
  (AName a1) == (AName a2) =
    case cast a2 of
      Nothing  -> False
      Just a2' -> a1 == a2'

instance Ord AName where
  (AName a1) `compare` (AName a2) =
    case cast a2 of
      Nothing  -> show (typeOf a1) `compare` show (typeOf a2)
      Just a2' -> a1 `compare` a2'

instance Show AName where
  show (AName a) = show a

-- | A (qualified) name is a (possibly empty) sequence of atomic names.
newtype Name = Name [AName]
  deriving (Eq, Ord, Semigroup, Monoid, Typeable)

instance Show Name where
  show (Name ns) = intercalate " .> " $ map show ns

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
  (|>) :: IsName a => a -> q -> q

-- | Of course, names can be qualified using @(.>)@.
instance Qualifiable Name where
  (|>) = (.>)

infixr 5 |>
infixr 5 .>