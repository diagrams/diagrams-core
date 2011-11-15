{-# LANGUAGE TypeOperators
           , FlexibleInstances
           , TypeSynonymInstances
           , TypeFamilies #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Ratio
import Control.Monad
import Data.Monoid
import Control.Applicative

import Data.Basis
import Data.LinearMap
import Data.AdditiveGroup
import Data.VectorSpace

import Graphics.Rendering.Diagrams.Core
import Graphics.Rendering.Diagrams.V
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Util

import Test.QuickCheck

------------------------------------------------------------
--  Supporting infrastructure  -----------------------------
------------------------------------------------------------

-- Some types for testing.

-- | Use the vector space Q^2 so we can have exact equality testing.
type Q2 = (Rational, Rational)
-- | Invertible linear maps over Q^2.
type I2 = Q2 :-: Q2
-- | 2x2 matrices of rationals.
data M2 = M2 Q2 Q2
  deriving (Eq, Show)

type instance V Q2 = Q2

instance Transformable Q2 where
  transform = apply

-- | Kludgy way to generate random nonsingular matrices.
instance Arbitrary M2 where
  arbitrary = do
    [a,b,c,d] <- replicateM 4 arbitrary
    return $ process a b c d
   where process a b c d
           | a*d - b*c == 0  = process a (b+1) (c+1) d  -- this is an awful hack
           | otherwise       = M2 (a,b) (c,d)

-- | The identity matrix.
idm :: M2
idm = M2 (1,0) (0,1)

-- | Compute the determinant of a matrix.
det :: M2 -> Rational
det (M2 (a,b)
        (c,d)) = a*d - b*c

-- | Matrix inversion.
minv :: M2 -> M2
minv m@(M2 (a,b)
           (c,d)) | dt == 0   = error $ "Non-invertible matrix: " ++ show m
                  | otherwise = M2 (d/dt, -b/dt) (-c/dt, a/dt)
  where dt = det m

-- | Matrix transpose.
mtrans :: M2 -> M2
mtrans (M2 (a,b) (c,d)) = M2 (a,c) (b,d)

-- | Matrix multiplication.
mm :: M2 -> M2 -> M2
mm (M2 (a1,b1) (c1,d1)) (M2 (a2,b2) (c2,d2))
  = M2 (a1*a2 + b1*c2, a1*b2 + b1*d2) (c1*a2 + d1*c2, c1*b2 + d1*d2)

-- | Convert a matrix to a (linear) function.
m2f :: M2 -> (Q2 -> Q2)
m2f (M2 (a,b) (c,d)) (x,y) = (a*x + b*y, c*x + d*y)

-- | Convert a linear function to a matrix.
f2m :: (Q2 -> Q2) -> M2
f2m f = mtrans $ M2 (f (1,0)) (f (0,1))

-- | Convert a matrix to an invertible linear map.
m2l :: M2 -> I2
m2l m = m2f m <-> m2f (minv m)

-- | Convert a linear map to a matrix.
l2m :: (Q2 :-* Q2) -> M2
l2m l = mtrans $ M2 (lapply l (1,0)) (lapply l (0,1))

instance AdditiveGroup Rational where {zeroV=0; (^+^) = (+); negateV = negate}

instance VectorSpace Rational where
  type Scalar Rational = Rational
  (*^) = (*)

instance HasBasis Rational where
  type Basis Rational = ()
  basisValue ()       = 1
  decompose s         = [((),s)]
  decompose' s        = const s

instance Arbitrary I2 where
  arbitrary = do
    m <- arbitrary
    if det m == 0
       then return (id <-> id)
       else return (m2f m <-> m2f (minv m))

instance Show I2 where
  show (f :-: g) = "[" ++ show a ++ " " ++ show c ++ "]\n[" ++ show b ++ " " ++ show d ++ "]"
    where (a,c) = lapply f (1,0)
          (b,d) = lapply f (0,1)

instance Eq I2 where
  f == g = lapp f (1,0) == lapp g (1,0) && lapp f (0,1) == lapp g (0,1)

instance Arbitrary (Transformation Q2) where
  arbitrary = do
    m <- arbitrary
    if det m == 0 then arbitrary
      else do
        v <- arbitrary
        return $ Transformation (m2l m) (m2l (mtrans m)) v

instance Show (Transformation Q2) where
  show (Transformation t tt v) = unlines [ show t, show tt, show v ]

i2m :: I2 -> (M2, M2)
i2m (l1 :-: l2) = (l2m l1, l2m l2)

iValid :: I2 -> Bool
iValid i = minv m1 == m2
  where (m1, m2) = i2m i

tValid :: Transformation Q2 -> Bool
tValid (Transformation t t' _) = iValid t && iValid t'
                              && mtrans m == mt && mtrans m' == mt'
  where (m ,m' ) = i2m t
        (mt,mt') = i2m t'

-- Transformations on Q2 as free monoid on a set of primitive generators

data PrimTransf = ScaleT (NonZero Rational) (NonZero Rational)
                | RotateT Double
                | ReflectT
                | TranslateT Rational Rational
  deriving (Show)

instance Arbitrary PrimTransf where
  arbitrary = oneof [ ScaleT <$> arbitrary <*> arbitrary
                    , RotateT <$> arbitrary
                    , return ReflectT
                    , TranslateT <$> arbitrary <*> arbitrary
                    ]

type FreeTransf = [PrimTransf]

------------------------------------------------------------
--  Properties  --------------------------------------------
------------------------------------------------------------

-- **** Linear function properties

-- f2m and m2f are inverse.
prop_f2m_m2f m = f2m (m2f m) == m

-- linv gives a left inverse.
prop_linv_L :: I2 -> Bool
prop_linv_L l = (linv l <> l) == mempty

-- linv gives a right inverse.
prop_linv_R :: I2 -> Bool
prop_linv_R l = (l <> linv l) == mempty

-- **** Transformation properties

-- Matrix inversion works.
prop_minv m = det m /= 0 ==> mm m (minv m) == idm

-- Random transformations are valid.
prop_tValid t = tValid t

-- Inversion gives a valid transformation.
prop_inv_valid t = tValid (inv t)

-- Translations are valid.
prop_trans_valid v = tValid (translation v)

-- Translations have no effect on vectors.
prop_trans_vec_invariant :: Q2 -> Q2 -> Bool
prop_trans_vec_invariant v w = translate v w == w

-- Translations do what they should to points.
prop_trans_point :: Q2 -> Q2 -> Bool
prop_trans_point v w = translate v (P w) == (P w')
  where (v1,v2) = v
        (x,y)   = w
        w'      = (x+v1, y+v2)

-- Scalings are valid.
prop_scale_valid s = s /= 0 ==> tValid (scaling s)

-- Scales do what they should.
prop_scale_scales :: Rational -> Q2 -> Property
prop_scale_scales s v = s /= 0 ==> scale s v == v'
  where (x,y) = v
        v' = (s*x, s*y)

------------------------------------------------------------
--  Collecting test results  -------------------------------
------------------------------------------------------------

tests = [ testGroup "Linear functions"
          [ testProperty "Matrix/function conversion" prop_f2m_m2f
          , testProperty "Linear left inverse"        prop_linv_L
          , testProperty "Linear right inverse"       prop_linv_R
          ]

        , testGroup "Transformations"
          [ testProperty "Matrix inversion"        prop_minv
          , testProperty "Transformation validity" prop_tValid
          , testProperty "Inversion validity"      prop_inv_valid
          , testProperty "Translation validity"    prop_trans_valid
          , testProperty "Translation invariance"  prop_trans_vec_invariant
          , testProperty "Point translation"       prop_trans_point
          , testProperty "Scale validity"          prop_scale_valid
          , testProperty "Scaling"                 prop_scale_scales
          ]
        ]

main = defaultMain tests