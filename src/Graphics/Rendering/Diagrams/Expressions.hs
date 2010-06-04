{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Expressions
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- An embedded domain-specific language for describing and rendering
-- diagrams.  This module provides names and expressions for referring
-- to locations within diagrams.
--
-- Note that end users should rarely (if ever) need to import this
-- module directly; instead, import "Graphics.Rendering.Diagrams",
-- which re-exports most of the functionality from this module.
-- Library developers may occasionally wish to import this module
-- directly if they need direct access to something not re-exported by
-- "Graphics.Rendering.Diagrams".
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Expressions
       ( -- * Names

         AName(..), Name(..), IsName(..)

       , Qualifiable(..)

         -- * Linear expressions

       , LExpr(..), evalLExpr

         -- * Name sets

       , NameSet(..)
       , fromNames

         -- ** Primitive 'NameSet' operations

       , rememberAs
       ) where

import Data.VectorSpace

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
--  Linear expressions  ------------------------------------
------------------------------------------------------------

data LExpr v = Var Name
             | Const v
             | LExpr v :+: LExpr v
             | Scalar v :*: LExpr v

evalLExpr :: VectorSpace v => LExpr v -> NameSet v -> v
  -- XXX should have some sort of warning here if multiple matches found?
evalLExpr (Var n) (NameSet names) = fromMaybe zeroV (M.lookup n names >>= listToMaybe)
evalLExpr (Const v) _   = v
evalLExpr (e1 :+: e2) names = evalLExpr e1 names ^+^ evalLExpr e2 names
evalLExpr (s :*: e) names   = s *^ evalLExpr e names

------------------------------------------------------------
--  Name sets  ---------------------------------------------
------------------------------------------------------------

newtype NameSet v = NameSet (M.Map Name [v])

instance Monoid (NameSet v) where
  mempty = NameSet M.empty
  (NameSet s1) `mappend` (NameSet s2) = NameSet $ M.unionWith (++) s1 s2

fromNames :: IsName n => [(n, v)] -> NameSet v
fromNames = NameSet . M.fromList . map (toName *** (:[]))

qualify :: AName -> NameSet v -> NameSet v
qualify n (NameSet names) = NameSet $ M.mapKeys (n:) names

rememberAs :: VectorSpace v => Name -> LExpr v -> NameSet v -> NameSet v
rememberAs n e s@(NameSet names) = NameSet $ M.insertWith (++) n [evalLExpr e s] names