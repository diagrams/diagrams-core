{-# LANGUAGE TypeSynonymInstances
           , FlexibleInstances
           , FlexibleContexts
           , TypeFamilies
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverlappingInstances
           , TupleSections
           , GADTs
           , DeriveDataTypeable
           , UndecidableInstances
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
        -- ** Atomic names
         Atomic(..)
       , AName(..)

        -- ** Names
       , Name(..), toName

        -- ** Qualifiable
       , Qualifiable(..), (.>), (||>)

         -- * Name maps

       , NameMap(..)

         -- ** Constructing name maps
       , fromNames, fromNamesB
       , rememberAs

         -- ** Searching within name maps
       , lookupN
       ) where

import Graphics.Rendering.Diagrams.V
import Graphics.Rendering.Diagrams.Monoids
import Graphics.Rendering.Diagrams.HasOrigin
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.Bounds
import Graphics.Rendering.Diagrams.Transform

import Data.VectorSpace

import Data.List (intercalate, isSuffixOf)
import qualified Data.Map as M
import Data.Monoid
import Control.Arrow ((***), second)
import Control.Monad (mplus)

import Data.Typeable

------------------------------------------------------------
--  Names  -------------------------------------------------
------------------------------------------------------------

-- | @Atomic@ types are those which can be used as names.  They must
--   support 'Typeable' (to facilitate extracting them from
--   existential wrappers), 'Ord' (for comparison and efficient
--   storage) and 'Show'.
class (Typeable a, Ord a, Show a) => Atomic a where
  toAName :: a -> AName
  toAName = AName

instance Atomic ()
instance Atomic Bool
instance Atomic Char
instance Atomic Int
instance Atomic Float
instance Atomic Double
instance Atomic Integer
instance Atomic String
instance Atomic a => Atomic [a]
instance (Atomic a, Atomic b) => Atomic (a,b)
instance (Atomic a, Atomic b, Atomic c) => Atomic (a,b,c)

-- | Atomic names.  @AName@ is just an existential wrapper around
--   'Atomic' values.
data AName where
  AName :: Atomic a => a -> AName
  deriving (Typeable)

instance Atomic AName where
  toAName = id

instance Eq AName where
  (AName a1) == (AName a2) =
    case cast a2 of
      Nothing  -> False
      Just a2' -> a1 == a2'

instance Ord AName where
  (AName a1) `compare` (AName a2) =
    case cast a2 of
      Nothing  -> (show $ typeOf a1) `compare` (show $ typeOf a2)
      Just a2' -> a1 `compare` a2'

instance Show AName where
  show (AName a) = show a

-- | A (qualified) name is a (possibly empty) sequence of atomic names.
newtype Name = Name [AName]
  deriving (Eq, Ord, Monoid)

instance Show Name where
  show (Name ns) = intercalate " |> " $ map show ns

-- | Convert an atomic name to a name.
toName :: Atomic a => a -> Name
toName = Name . (:[]) . toAName

-- | Instances of 'Qualifiable' are things which can be qualified by
--   prefixing them with an atomic name.
class Qualifiable q where
  -- | Qualify with the given name.
  (|>) :: Atomic a => a -> q -> q

-- | Of course, names can be qualified.
instance Qualifiable Name where
  a |> (Name as) = Name (toAName a : as)

-- | Convenient operator for writing complete names in the form @a1 |>
--   a2 |> a3 .> a4@.  In particular, @a1 .> a2@ is equivalent to
--   @a1 |> toName a2@.
(.>) :: (Atomic a1, Atomic a2) => a1 -> a2 -> Name
a1 .> a2 = a1 |> toName a2

infixr 2 |>
infixr 2 .>

-- | Qualify by an entire qualified name.  @(a1 |> a2 .> a3) ||> q@ is
--   equivalent to @a1 |> a2 |> a3 |> q@.
(||>) :: Qualifiable q => Name -> q -> q
Name as ||> q = foldr (|>) q as

------------------------------------------------------------
--  Name maps  ---------------------------------------------
------------------------------------------------------------

-- | A 'NameMap' is a map associating names to pairs of points (local
--   origins) and bounding functions.  There can be multiple (point,
--   bounding function) pairs associated with each name.
newtype NameMap v = NameMap (M.Map Name [(Point v, TransInv (Bounds v))])
  deriving (Show)

-- Note, in some sense it would be nicer to use Sets instead of a
-- list, but then we would have to put Ord constraints on v
-- everywhere. =P

-- Note also that we wrap the bounds with TransInv.  This is because
-- the base point of each bounding function should be thought of as
-- the paired Point, *not* as the origin of the current vector space.
-- In other words, the point gets translated "for both of them".

type instance V (NameMap v) = v

-- | 'NameMap's form a monoid with the empty map as the identity, and
--   map union as the binary operation.  No information is ever lost:
--   if two maps have the same name in their domain, the resulting map
--   will associate that name to the concatenation of the information
--   associated with that name.
instance Monoid (NameMap v) where
  mempty = NameMap M.empty
  (NameMap s1) `mappend` (NameMap s2) = NameMap $ M.unionWith (++) s1 s2

instance (AdditiveGroup (Scalar v), Fractional (Scalar v), InnerSpace v)
      => HasOrigin (NameMap v) where
  moveOriginTo p (NameMap m) = NameMap $ M.map (map (moveOriginTo p *** moveOriginTo p)) m

instance (AdditiveGroup (Scalar v), InnerSpace v, Floating (Scalar v), HasLinearMap v)
  => Transformable (NameMap v) where
  transform t (NameMap ns) = NameMap $ M.map (map (papply t *** transform t)) ns

-- | 'NameMap's are qualifiable: if @ns@ is a 'NameMap', then @a |>
--   ns@ is the same 'NameMap' except with every name qualified by
--   @a@.
instance Qualifiable (NameMap v) where
  a |> (NameMap names) = NameMap $ M.mapKeys (a |>) names

-- | Construct a 'NameMap' from a list of (name, point) pairs.  The
--   bounding functions will be empty.
fromNames :: (AdditiveGroup (Scalar v), Ord (Scalar v), Atomic a)
          => [(a, Point v)] -> NameMap v
fromNames = NameMap . M.fromList . map (toName *** ((:[]) . (,mempty)))

-- | Construct a 'NameMap' from a list of associations between names
--   and (point, bounds) pairs.
fromNamesB :: Atomic a => [(a, (Point v, Bounds v))] -> NameMap v
fromNamesB = NameMap . M.fromList . map (toName *** (return . second TransInv))

-- | Give a name to a point and bounding function.
rememberAs :: Name -> Point v -> Bounds v -> NameMap v -> NameMap v
rememberAs n p b (NameMap names) = NameMap $ M.insertWith (++) n [(p,TransInv b)] names

-- | A name acts on a name map by qualifying every name in it.
instance Action Name (NameMap v) where
  act = (||>)

-- | Names don't act on anything else.
instance Action Name a


-- Searching in name maps.

-- | Look for the given name in a name map, returning a list of points
--   and bounding regions associated with that name.  If no names
--   match the given name exactly, return all the points associated
--   with names of which the given name is a suffix.
lookupN :: Name -> NameMap v -> Maybe [(Point v, Bounds v)]
lookupN n (NameMap m)
  = (fmap . map . second) unTransInv
    (M.lookup n m `mplus`
    (flatten . filter ((n `nameSuffixOf`) . fst) . M.assocs $ m))
  where (Name n1) `nameSuffixOf` (Name n2) = n1 `isSuffixOf` n2
        flatten [] = Nothing
        flatten xs = Just . concatMap snd $ xs