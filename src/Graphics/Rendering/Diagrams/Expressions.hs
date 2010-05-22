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

module Graphics.Rendering.Diagrams.Expressions where

import Data.VectorSpace

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data AName = IName Int
           | SName String
  deriving (Eq, Ord, Show)

type Name = [AName]

nm :: String -> Name
nm = (:[]) . SName

type NameSet v = M.Map Name v

qualify :: AName -> NameSet v -> NameSet v
qualify n = M.mapKeys (n:)

data LExpr v = Var Name
             | Const v
             | LExpr v :+: LExpr v
             | Scalar v :*: LExpr v

evalLExpr :: VectorSpace v => LExpr v -> NameSet v -> v
evalLExpr (Var n) names = fromMaybe zeroV (M.lookup n names)
evalLExpr (Const v) _   = v
evalLExpr (e1 :+: e2) names = evalLExpr e1 names ^+^ evalLExpr e2 names
evalLExpr (s :*: e) names   = s *^ evalLExpr e names

rememberAs :: VectorSpace v => Name -> LExpr v -> NameSet v -> NameSet v
rememberAs n e names = M.insert n (evalLExpr e names) names