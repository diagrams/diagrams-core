-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Expressions
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- XXX comment me
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Expressions
       ( -- * Names

         AName, Name, nm

         -- * Linear expressions

       , LExpr(..), evalLExpr

         -- * Name sets

       , NameSet(..)

         -- ** Primitive 'NameSet' operations

       , qualify
       , rememberAs
       ) where

import Data.VectorSpace

import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromMaybe, listToMaybe)

------------------------------------------------------------
--  Names  -------------------------------------------------
------------------------------------------------------------

data AName = IName Int
           | SName String
  deriving (Eq, Ord, Show)

type Name = [AName]

nm :: String -> Name
nm = (:[]) . SName

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

qualify :: AName -> NameSet v -> NameSet v
qualify n (NameSet names) = NameSet $ M.mapKeys (n:) names

rememberAs :: VectorSpace v => Name -> LExpr v -> NameSet v -> NameSet v
rememberAs n e s@(NameSet names) = NameSet $ M.insertWith (++) n [evalLExpr e s] names