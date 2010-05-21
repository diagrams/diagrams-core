{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , GADTs
           , FlexibleContexts
           , FlexibleInstances
           , TypeOperators
           , GeneralizedNewtypeDeriving
           , UndecidableInstances
           , TypeSynonymInstances #-}

module Diagrams where

import Data.AdditiveGroup
import Data.VectorSpace
import Data.LinearMap
import Data.Basis
import Data.MemoTrie
import qualified Data.Map as M

import Data.Maybe
import Data.Monoid
import Control.Applicative (Applicative(..), (*>), (<$>))

import System.IO

---------------------------------------------
-- Backend/Primitive stuff

data RenderOption =
    OutputFile FilePath
  -- XXX other things here, like size etc.
  | Other String String

class (HasBasis (BSpace b), HasTrie (Basis (BSpace b))) => Backend b where
  type BSpace b :: *
  type Render b :: * -> *
  runRender :: [RenderOption] -> Render b () -> IO ()

class (HasBasis (TSpace t), HasTrie (Basis (TSpace t))) => Transformable t where
  type TSpace t :: *
  transform :: Affine (TSpace t) -> t -> t

class (Backend b, Transformable t) => Renderable t b where
  render :: b -> t -> Render b ()

data Prim b where
  Prim :: (BSpace b ~ TSpace t, Renderable t b) => t -> Prim b

renderPrim :: b -> Prim b -> Render b ()
renderPrim b (Prim t) = render b t

instance Backend b => Transformable (Prim b) where
  type TSpace (Prim b) = BSpace b
  transform v (Prim p) = Prim (transform v p)

instance Backend b => Renderable (Prim b) b where
  render b (Prim p) = render b p

------------------------------------------------
-- Bounds

type Bounds v = v -> Scalar v

------------------------------------------------
-- Names and expressions

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

-------------------------------------------------
-- Diagrams

data Diagram b = Diagram { prims  :: [Prim b]
                         , bounds :: Bounds (BSpace b)
                         , names  :: NameSet (BSpace b)
                         }

rebase :: ( Backend b, v ~ BSpace b
          , InnerSpace v, HasBasis v, HasTrie (Basis v)
          , AdditiveGroup (Scalar v), Fractional (Scalar v))
       => LExpr v -> Diagram b -> Diagram b
rebase e d = Diagram { prims  = map (transform (translation (negateV u)))
                                    (prims d)
                     , bounds = rebaseBounds u (bounds d)
                     , names  = M.map (^-^ u) (names d)
                     }
  where u = evalLExpr e (names d)

rebaseBounds :: (InnerSpace v, AdditiveGroup (Scalar v), Fractional (Scalar v))
             => v -> Bounds v -> Bounds v
rebaseBounds u f v = f v ^-^ ((u ^/ (v <.> v)) <.> v)

instance ( Backend b
         , HasTrie (Basis (BSpace b))
         , HasBasis (BSpace b)) =>
         Transformable (Diagram b) where
  type TSpace (Diagram b) = BSpace b
  transform t (Diagram ps bs ns) = Diagram (map (transform t) ps)
                                           (\v -> undefined)    -- XXX
                                           (M.map (aapply t) ns)

instance (Backend b, Applicative (Render b)) => Renderable (Diagram b) b where
  render b (Diagram ps _ _) = mapA_ (render b) ps

mapA_ :: Applicative f => (a -> f b) -> [a] -> f ()
mapA_ f []     = pure ()
mapA_ f (x:xs) = f x *> mapA_ f xs

------------------------------------------------
-- Affine transformations

-- An affine transformation consists of a linear transformation and a
-- translation.
data Affine v = Affine (v :-* v) v

-- Affine transformations are closed under composition.
instance (HasBasis v, HasTrie (Basis v), VectorSpace v) => Monoid (Affine v) where
  mempty  = Affine idL zeroV
  mappend (Affine a2 b2) (Affine a1 b1) = Affine (a2 *.* a1) (lapply a2 b1 ^+^ b2)

-- Apply an affine transformation.
aapply :: (HasBasis v, HasTrie (Basis v)) => Affine v -> v -> v
aapply (Affine a b) v = lapply a v ^+^ b

translation :: (HasBasis v, HasTrie (Basis v)) => v -> Affine v
translation = Affine idL

------------------------------------------------
-- Composing diagrams

atop :: Ord (Scalar (BSpace b)) => Diagram b -> Diagram b -> Diagram b
atop (Diagram ps1 bs1 ns1) (Diagram ps2 bs2 ns2) =
  Diagram (ps1 ++ ps2)
          (\v -> max (bs1 v) (bs2 v))  -- XXX make this nicer?
          (M.union ns1 ns2)

beside :: ( Backend b
          , v ~ BSpace b
          , HasBasis v
          , HasTrie (Basis v)
          , InnerSpace v
          , AdditiveGroup (Scalar v)
          , Fractional (Scalar v)
          , Ord (Scalar v))
       => v -> Diagram b -> Diagram b -> Diagram b
beside v d1 d2 = rebase (Const (bounds d1 v *^ v)) d1
          `atop` rebase (Const (bounds d2 (negateV v) *^ negateV v)) d2

------------------------------------------------
-- Paths

data Segment v = Bezier v v v
  deriving Show

straight :: (VectorSpace v, Fractional (Scalar v)) => v -> Segment v
straight v = Bezier (v ^/ 3) (2 *^ (v ^/ 3)) v

bezier :: v -> v -> v -> Segment v
bezier = Bezier

pointAt :: (VectorSpace v, Num (Scalar v)) => Scalar v -> Segment v -> v
pointAt t (Bezier c1 c2 x2) = (3 * (1-t)^2 * t) *^ c1
                          ^+^ (3 * (1-t) * t^2) *^ c2
                          ^+^ t^3 *^ x2

{- (1-t)^2 t c1 + (1-t) t^2 c2 + t^3 x2

   Can we compute the projection of B(t) onto a given vector v?

   u.v = |u||v| cos th

   |proj_v u| = cos th * |u|
              = (u.v/|v|)

   so B_v(t) = (B(t).v/|v|)

   Then take the derivative of this wrt. t, get a quadratic, solve.

   B_v(t) = (1/|v|) *     -- note this does not affect max/min, can solve for t first
            (1-t)^2 t (c1.v) + (1-t) t^2 (c2.v) + t^3 (x2.v)
          = t^3 ((c1 - c2 + x2).v) + t^2 ((-2c1 + c2).v) + t (c1.v)

   B_v'(t) = t^2 (3(c1 - c2 + x2).v) + t (2(-2c1 + c2).v) + c1.v

   Set equal to zero, use quadratic formula.
-}

data QFSolution d = NoRealSol | DoubleSol d | RealSols d d

quadForm :: (Floating d, Ord d) => d -> d -> d -> QFSolution d
quadForm a b c
  | d < 0     = NoRealSol
  | d == 0    = DoubleSol (-b/(2*a))
  | otherwise = RealSols ((-b + sqrt d)/(2*a)) ((-b - sqrt d)/(2*a))
 where d = b*b - 4*a*c

qfList :: (Floating d, Ord d) => d -> d -> d -> [d]
qfList a b c = case quadForm a b c of
                 NoRealSol -> []
                 DoubleSol d -> [d]
                 RealSols d1 d2 -> [d1,d2]

segmentBounds :: (InnerSpace v, Ord (Scalar v), Floating (Scalar v))
              => Segment v -> Bounds v
segmentBounds s@(Bezier c1 c2 x2) v =
  maximum .
  map (\t -> (pointAt t s <.> v) / magnitude v) $
  [0,1] ++
  qfList (3 * ((c1 ^-^ c2 ^+^ x2) <.> v))
         (2 * (((-2) *^ c1 ^+^ c2) <.> v))
         (c1 <.> v)