{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleContexts, EmptyDataDecls #-}

import Data.AdditiveGroup
import Data.VectorSpace
import qualified Data.Map as M

import Data.Maybe

---------------------------------------------
-- Backend/Primitive stuff

class Backend b where
  type BackendSpace b :: *
  type Render b :: * -> *
  type Options b :: *
  render :: Options b -> Render b () -> IO ()

class (Backend b) => Renderable p b where
  renderPrim :: p -> Render b ()

data Prim b where
  Prim :: Renderable p b => p -> Prim b

------------------------------------------------
-- Bounds

type Bounds v = v -> Scalar v

------------------------------------------------
-- Names and expressions

data AName = IName Int
           | SName String
  deriving (Eq, Ord, Show)

type Name = [AName]

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

-------------------------------------------------
-- Diagrams

data F   -- Frozen
data U   -- Unfrozen

data Diagram f b = Diagram { prims  :: [Prim b]
                           , bounds :: Bounds (BackendSpace b)
                           , names  :: NameSet (BackendSpace b)
                           }

rebase :: (v ~ BackendSpace b, VectorSpace v) => LExpr v -> Diagram f b -> Diagram f b
rebase e d = Diagram { prims  = map undefined (prims d)
                     , bounds = rebaseBounds (evalLExpr e (names d)) (bounds d)
                     , names  = M.map undefined (names d)
                     }

rebaseBounds :: VectorSpace v => v -> Bounds v -> Bounds v
rebaseBounds = undefined