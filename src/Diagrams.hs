{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , GADTs
           , FlexibleContexts
           , FlexibleInstances
           , TypeOperators
           , GeneralizedNewtypeDeriving
           , UndecidableInstances #-}

import Data.AdditiveGroup
import Data.VectorSpace
import Data.LinearMap
import Data.Basis
import Data.MemoTrie
import qualified Data.Map as M

import Data.Maybe
import Data.Monoid

import System.IO

---------------------------------------------
-- Backend/Primitive stuff

data RenderOption =
    OutputFile FilePath
  -- XXX other things here, like size etc.
  | Other String String

class Backend b where
  type BackendSpace b :: *
  type Render b :: * -> *
  render :: [RenderOption] -> Render b () -> IO ()

class (HasBasis v, HasTrie (Basis v)) => Transformable v t where
  transform :: Affine v -> t -> t

class (Backend b, Transformable (BackendSpace b) p) => Renderable p b where
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

data Diagram b = Diagram { prims  :: [Prim b]
                         , bounds :: Bounds (BackendSpace b)
                         , names  :: NameSet (BackendSpace b)
                         }

rebase :: (v ~ BackendSpace b, InnerSpace v, AdditiveGroup (Scalar v), Fractional (Scalar v))
       => LExpr v -> Diagram b -> Diagram b
rebase e d = Diagram { prims  = map undefined (prims d)    -- XXX
                     , bounds = rebaseBounds u (bounds d)
                     , names  = M.map (^-^ u) (names d)
                     }
  where u = evalLExpr e (names d)

rebaseBounds :: (InnerSpace v, AdditiveGroup (Scalar v), Fractional (Scalar v))
             => v -> Bounds v -> Bounds v
rebaseBounds u f v = f v ^-^ ((u ^/ (v <.> v)) <.> v)

------------------------------------------------
-- Affine transformations

-- An affine transformation consists of a linear transformation and a
-- translation.
data Affine v = Affine (v :-* v) v

-- Affine transformations are closed under composition.
instance (HasBasis v, HasTrie (Basis v), VectorSpace v) => Monoid (Affine v) where
  mempty = Affine idL zeroV
  mappend (Affine a2 b2) (Affine a1 b1) = Affine (a2 *.* a1) (lapply a2 b1 ^+^ b2)

-- Apply an affine transformation.
aapply :: (HasBasis v, HasTrie (Basis v)) => Affine v -> v -> v
aapply (Affine a b) v = lapply a v ^+^ b