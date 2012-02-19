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
         AName(..)

        -- ** Names
       , Name(..), IsName(..), (.>)

        -- ** Qualifiable
       , Qualifiable(..)

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
import Graphics.Rendering.Diagrams.Envelope
import Graphics.Rendering.Diagrams.Transform

import Data.VectorSpace

import Data.List (intercalate, isSuffixOf)
import qualified Data.Map as M
import Data.Semigroup
import Control.Arrow ((***))
import Control.Monad (mplus)

import Control.Newtype

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

------------------------------------------------------------
--  Name maps  ---------------------------------------------
------------------------------------------------------------

-- | A 'NameMap' is a map associating names to located envelopes,
--   /i.e./ envelopes with concrete locations for their base
--   points.  There can be multiple associations for any given name.
newtype NameMap v = NameMap (M.Map Name [LocatedEnvelope v])
  deriving (Show)

instance Newtype (NameMap v) (M.Map Name [LocatedEnvelope v]) where
  pack = NameMap
  unpack (NameMap m) = m

-- Note, in some sense it would be nicer to use Sets instead of a
-- list, but then we would have to put Ord constraints on v
-- everywhere. =P

-- Note also that we wrap the envelope with TransInv.  This is because
-- the base point of each envelope should be thought of as the paired
-- Point, *not* as the origin of the current vector space.  In other
-- words, the point gets translated "for both of them".

type instance V (NameMap v) = v

instance Semigroup (NameMap v) where
  NameMap s1 <> NameMap s2 = NameMap $ M.unionWith (++) s1 s2

-- | 'NameMap's form a monoid with the empty map as the identity, and
--   map union as the binary operation.  No information is ever lost:
--   if two maps have the same name in their domain, the resulting map
--   will associate that name to the concatenation of the information
--   associated with that name.
instance Monoid (NameMap v) where
  mempty = NameMap M.empty
  mappend = (<>)

instance (AdditiveGroup (Scalar v), Fractional (Scalar v), InnerSpace v)
      => HasOrigin (NameMap v) where
  moveOriginTo = over NameMap . moveOriginTo

instance (AdditiveGroup (Scalar v), InnerSpace v, Floating (Scalar v), HasLinearMap v)
  => Transformable (NameMap v) where
  transform = over NameMap . transform

-- | 'NameMap's are qualifiable: if @ns@ is a 'NameMap', then @a |>
--   ns@ is the same 'NameMap' except with every name qualified by
--   @a@.
instance Qualifiable (NameMap v) where
  a |> (NameMap names) = NameMap $ M.mapKeys (a |>) names

-- | Construct a 'NameMap' from a list of (name, point) pairs.
fromNames :: (InnerSpace v, AdditiveGroup (Scalar v), Ord (Scalar v), Floating (Scalar v), IsName a)
          => [(a, Point v)] -> NameMap v
fromNames = NameMap . M.fromListWith (++) 
          . map (toName *** ((:[]) . (\p -> locateEnvelope p (getEnvelope p))))

-- | Construct a 'NameMap' from a list of associations between names
--   and located envelopes.
fromNamesB :: IsName a => [(a, LocatedEnvelope v)] -> NameMap v
fromNamesB = NameMap . M.fromListWith (++) . map (toName *** (:[]))

-- | Give a name to a located envelope.
rememberAs :: IsName a => a -> LocatedEnvelope v -> NameMap v -> NameMap v
rememberAs n b = over NameMap $ M.insertWith (++) (toName n) [b]

-- | A name acts on a name map by qualifying every name in it.
instance Action Name (NameMap v) where
  act = (|>)

-- | Names don't act on anything else.
instance Action Name a


-- Searching in name maps.

-- | Look for the given name in a name map, returning a list of
--   located envelopes associated with that name.  If no names match
--   the given name exactly, return all the points associated with
--   names of which the given name is a suffix.
lookupN :: IsName n => n -> NameMap v -> Maybe [LocatedEnvelope v]
lookupN a (NameMap m)
  = M.lookup n m `mplus`
    (flatten . filter ((n `nameSuffixOf`) . fst) . M.assocs $ m)
  where (Name n1) `nameSuffixOf` (Name n2) = n1 `isSuffixOf` n2
        flatten [] = Nothing
        flatten xs = Just . concatMap snd $ xs
        n = toName a