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

         -- * Name maps

       , NameMap(..)

         -- ** Constructing name maps
       , fromNames
       , rememberAs

         -- ** Searching within name maps
       , lookupN
       ) where

import Graphics.Rendering.Diagrams.V
import Graphics.Rendering.Diagrams.Monoids
import Graphics.Rendering.Diagrams.HasOrigin
import Graphics.Rendering.Diagrams.Points

import Data.VectorSpace

import Data.List (intercalate, isSuffixOf)
import qualified Data.Map as M
import Data.Monoid
import Control.Arrow ((***))
import Control.Monad (mplus)

------------------------------------------------------------
--  Names  -------------------------------------------------
------------------------------------------------------------

-- | An atomic name is either a number or a string.  Numeric names are
--   provided for convenience in naming lists of things, such as a row
--   of ten squares, or the vertices of a path.
data AName = IName Int
           | SName String
  deriving Ord

-- | Note that equality on names does not distinguish between integers
--   and their @String@ representations.
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
  -- | Qualify with the given name.
  (|>) :: IsName n => n -> a -> a

-- | Names can be qualified by prefixing them with other names.
instance Qualifiable Name where
  n1 |> n2 = toName n1 `mappend` n2

------------------------------------------------------------
--  Name maps  ---------------------------------------------
------------------------------------------------------------

-- | A 'NameMap' is a map from names to points, possibly with
--   multiple points associated with each name.
newtype NameMap v = NameMap (M.Map Name [Point v])
-- Note, in some sense it would be nicer to use Sets of points instead
-- of a list, but then we would have to put Ord constraints on v
-- everywhere. =P

type instance V (NameMap v) = v

-- | 'NameMap's form a monoid with the empty map as the identity, and
--   map union as the binary operation.  No information is ever lost:
--   if two maps have the same name in their domain, the resulting map
--   will associate that name to the union of the two sets of points
--   associated with that name.
instance Monoid (NameMap v) where
  mempty = NameMap M.empty
  (NameMap s1) `mappend` (NameMap s2) = NameMap $ M.unionWith (++) s1 s2

instance VectorSpace v => HasOrigin (NameMap v) where
  moveOriginTo p (NameMap m) = NameMap $ M.map (map (moveOriginTo p)) m

-- | 'NameMap's are qualifiable: if @ns@ is a 'NameMap', then @n |>
--   ns@ is the same 'NameMap' except with every name qualified by
--   @n@.
instance Qualifiable (NameMap v) where
  n |> (NameMap names) = NameMap $ M.mapKeys (n |>) names

-- | Construct a 'NameMap' from a list of (name, point) pairs.
fromNames :: IsName n => [(n, Point v)] -> NameMap v
fromNames = NameMap . M.fromList . map (toName *** (:[]))

-- | Give a name to a point.
rememberAs :: Name -> Point v -> NameMap v -> NameMap v
rememberAs n p (NameMap names) = NameMap $ M.insertWith (++) n [p] names

-- | A name acts on a name map by qualifying every name in it.
instance Action Name (NameMap v) where
  act = (|>)

-- | Names don't act on anything else.
instance Action Name a


-- Searching in name maps.

-- | Look for the given name in a name map, returning a list of points
--   associated with that name.  If no names match the given name
--   exactly, return all the points associated with names of which the
--   given name is a suffix.
lookupN :: IsName n => n -> NameMap v -> Maybe [Point v]
lookupN n (NameMap m)
  = M.lookup n' m `mplus`
    (flatten . filter ((n' `nameSuffixOf`) . fst) . M.assocs $ m)
  where n' = toName n
        (Name n1) `nameSuffixOf` (Name n2) = n1 `isSuffixOf` n2
        flatten [] = Nothing
        flatten xs = Just . concat . map snd $ xs