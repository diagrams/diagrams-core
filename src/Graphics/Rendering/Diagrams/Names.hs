{-# LANGUAGE TypeSynonymInstances
           , FlexibleInstances
           , TypeFamilies
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverlappingInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Names
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines a type of names which can be used for referring
-- to locations within diagrams, and related types.
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

         -- ** Searching within name sets
       , lookupN
       ) where

import Graphics.Rendering.Diagrams.V
import Graphics.Rendering.Diagrams.Monoids
import Graphics.Rendering.Diagrams.HasOrigin
import Graphics.Rendering.Diagrams.Points

import Data.VectorSpace

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Monoid
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

-- | A (qualified) name is a (possibly empty) sequence of atomic names.
--   Atomic names can be either numbers or arbitrary strings.  Numeric
--   names are provided for convenience in naming lists of things,
--   such as a row of ten squares, or the vertices of a path.
newtype Name = Name [AName]
  deriving (Eq, Ord, Monoid)

instance Show Name where
  show (Name ns) = intercalate "." $ map show ns

-- | Instaces of 'IsName' are things which can be converted to names.
class IsName n where
  toName :: n -> Name

instance IsName String where
  toName = Name . (:[]) . SName

instance IsName Int where
  toName = Name . (:[]) . IName

instance IsName Name where
  toName = id

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
-- Note, in some sense it would be nicer to use Sets of points instead
-- of a list, but then we would have to put Ord constraints on v
-- everywhere. =P

type instance V (NameSet v) = v

-- | 'NameSet's form a monoid with the empty map as the identity, and
--   map union as the binary operation.  No information is ever lost:
--   if two maps have the same name in their domain, the resulting map
--   will associate that name to the union of the two sets of vectors
--   associated with that name.
instance Monoid (NameSet v) where
  mempty = NameSet M.empty
  (NameSet s1) `mappend` (NameSet s2) = NameSet $ M.unionWith (++) s1 s2

instance VectorSpace v => HasOrigin (NameSet v) where
  moveOriginTo p (NameSet m) = NameSet $ M.map (map (moveOriginTo p)) m

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
rememberAs n p (NameSet names) = NameSet $ M.insertWith (++) n [p] names

-- | A name acts on a name set by qualifying it.
instance Action Name (NameSet v) where
  act = (|>)

-- | Names don't act on anything else.
instance Action Name a


-- Searching in name sets.

-- | Look for the given name in a name set, returning a list of points
--   associated with that name.
lookupN :: IsName n => n -> NameSet v -> Maybe [Point v]
lookupN n (NameSet m) = M.lookup (toName n) m