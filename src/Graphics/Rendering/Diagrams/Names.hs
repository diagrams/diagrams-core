{-# LANGUAGE TypeSynonymInstances
           , FlexibleInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Names
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- An embedded domain-specific language for describing and rendering
-- diagrams.  This module defines a type of names, for referring to
-- locations within diagrams, and related types.
--
-- Note that end users should rarely (if ever) need to import this
-- module directly; instead, import "Graphics.Rendering.Diagrams",
-- which re-exports most of the functionality from this module.
-- Library developers may occasionally wish to import this module
-- directly if they need direct access to something not re-exported by
-- "Graphics.Rendering.Diagrams".
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Names
       (-- * Names

         AName(..), Name(..), IsName(..)

       , Qualifiable(..)

         -- * Name sets

       , NameSet(..)

         -- ** Constructing name sets
       , fromNames
       , rememberAs
       ) where

import Graphics.Rendering.Diagrams.Points

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Arrow ((***))

------------------------------------------------------------
--  Names  -------------------------------------------------
------------------------------------------------------------

-- | An atomic name is either a number or a string.  Numeric names are
--   provided for convenience in naming lists of things, such as a row
--   of ten squares, or the vertices of a path.
data AName = IName Int
           | SName String
  deriving Ord

-- XXX is this really what we want?  Given these Eq and Show
-- instances, is it worth even having the IName/SName distinction at
-- all?  Or do we want the derived Eq instance?

instance Eq AName where
  IName i1 == IName i2 = i1 == i2
  SName s1 == SName s2 = s1 == s2
  IName i  == SName s  = show i == s
  SName s  == IName i  = s == show i

instance Show AName where
  show (IName i) = show i
  show (SName s) = s

-- | A (qualified) name is a nonempty sequence of atomic names.
--   Atomic names can be either numbers or arbitrary strings.  Numeric
--   names are provided for convenience in naming lists of things,
--   such as a row of ten squares, or the vertices of a path.
newtype Name = Name [AName]
  deriving (Eq, Ord)

instance Show Name where
  show (Name ns) = intercalate "." $ map show ns

-- | Instaces of 'IsName' are things which can be converted to names.
class IsName n where
  toName :: n -> Name

instance IsName String where
  toName = Name . (:[]) . SName

instance IsName Int where
  toName = Name . (:[]) . IName

-- | Instances of 'Qualifiable' are things which can be qualified by
--   prefixing them with a name.
class Qualifiable a where
  (|>) :: IsName n => n -> a -> a
  -- ^ Qualify with the given name.

-- | Of course, names themselves are qualifiable.
instance Qualifiable Name where
  n1 |> (Name ns2) = Name $ ns1 ++ ns2
    where (Name ns1) = toName n1

------------------------------------------------------------
--  Name sets  ---------------------------------------------
------------------------------------------------------------

-- | A 'NameSet' is a map from names to points, possibly with
--   multiple points associated with each name.
newtype NameSet v = NameSet (M.Map Name [Point v])

-- | 'NameSet's form a monoid with the empty map as the identity, and
--   map union as the binary operation.  No information is ever lost:
--   if two maps have the same name in their domain, the resulting map
--   will associate that name to the union of the two sets of vectors
--   associated with that name.
instance Monoid (NameSet v) where
  mempty = NameSet M.empty
  (NameSet s1) `mappend` (NameSet s2) = NameSet $ M.unionWith (++) s1 s2

-- | 'NameSet's are qualifiable: if @ns@ is a 'NameSet', then @n |>
--   ns@ is the same 'NameSet' except with every name qualified by
--   @n@.
instance Qualifiable (NameSet v) where
  n |> (NameSet names) = NameSet $ M.mapKeys (n |>) names

-- | Construct a 'NameSet' from a list of (name, point) pairs.
fromNames :: IsName n => [(n, Point v)] -> NameSet v
fromNames = NameSet . M.fromList . map (toName *** (:[]))

-- | Give a name to a point.
rememberAs :: Name -> Point v -> NameSet v -> NameSet v
rememberAs n p s@(NameSet names) = NameSet $ M.insertWith (++) n [p] names