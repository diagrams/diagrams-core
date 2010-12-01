{-# LANGUAGE TypeOperators
           , FlexibleInstances
           , TypeSynonymInstances
           , TypeFamilies #-}

import Data.Ratio
import Control.Monad
import Data.Monoid

import Data.Basis
import Data.LinearMap
import Data.AdditiveGroup
import Data.VectorSpace

import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Util

import Test.QuickCheck

type R2 = (Rational, Rational)
type L2 = R2 :-: R2
type M2 = (R2, R2)

idm :: M2
idm = ((1,0),(0,1))

det :: M2 -> Rational
det ((a,b)
    ,(c,d)) = a*d - b*c

minv :: M2 -> M2
minv m@((a,b)
       ,(c,d)) | dt == 0   = error $ "Non-invertible matrix: " ++ show m
               | otherwise = ((d/dt, -b/dt), (-c/dt, a/dt))
  where dt = det m

mtrans :: M2 -> M2
mtrans ((a,b),(c,d)) = ((a,c),(b,d))

mm :: M2 -> M2 -> M2
mm ((a1,b1),(c1,d1)) ((a2,b2),(c2,d2))
  = ((a1*a2 + b1*c2, a1*b2 + b1*d2), (c1*a2 + d1*c2, c1*b2 + d1*d2))

prop_minv m = det m /= 0 ==> mm m (minv m) == idm

m2f :: M2 -> (R2 -> R2)
m2f ((a,b),(c,d)) (x,y) = (a*x + b*y, c*x + d*y)

f2m :: (R2 -> R2) -> M2
f2m f = mtrans (f (1,0), f (0,1))

prop_f2m_m2f m = f2m (m2f m) == m

m2l :: M2 -> L2
m2l m = m2f m <-> m2f (minv m)

instance AdditiveGroup Rational where {zeroV=0; (^+^) = (+); negateV = negate}

instance VectorSpace Rational where
  type Scalar Rational = Rational
  (*^) = (*)

instance HasBasis Rational where
  type Basis Rational = ()
  basisValue ()       = 1
  decompose s         = [((),s)]
  decompose' s        = const s

instance Arbitrary L2 where
  arbitrary = do
    m <- arbitrary
    if det m == 0
       then return (id <-> id)
       else return (m2f m <-> m2f (minv m))

instance Show L2 where
  show (f :-: g) = "[" ++ show a ++ " " ++ show c ++ "]\n[" ++ show b ++ " " ++ show d ++ "]"
    where (a,c) = lapply f (1,0)
          (b,d) = lapply f (0,1)

instance Eq L2 where
  f == g = lapp f (1,0) == lapp g (1,0) && lapp f (0,1) == lapp g (0,1)

prop_linv_L :: L2 -> Bool
prop_linv_L l = (linv l <> l) == mempty

prop_linv_R :: L2 -> Bool
prop_linv_R l = (l <> linv l) == mempty

instance Arbitrary (Transformation R2) where
  arbitrary = do
    m <- arbitrary
    v <- arbitrary
    return $ Transformation (m2l m) (m2l (mtrans m)) v