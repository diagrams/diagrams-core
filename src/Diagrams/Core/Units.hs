{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Diagrams.Core.Units where

import           Diagrams.Core.Transform
import           Diagrams.Core.V

import           Control.Lens            (Prism', prism')
import           Data.Data
import           Data.VectorSpace

------------------------------------------------------------
--  Physical Units  ----------------------------------------
------------------------------------------------------------

data Physical d = Inches d | Pixels d
  deriving (Read, Show, Eq, Ord, Typeable, Data)

scalePhysical :: Num d => d -> Physical d -> Physical d
scalePhysical d (Inches d') = Inches (d * d')
scalePhysical d (Pixels d') = Pixels (d * d')

-- | The first argument is the conversion factor from inches, /i.e./
--   what do you multiply inches by to obtain the unit in question?
--   For example, 'mm' is defined as 'fromInches 25.4', since there
--   are 25.4 millimeters in one inch.
fromInches :: Fractional d => d -> Prism' (Physical d) d
fromInches f = prism' (Inches . (*f)) (\phys -> case phys of
                                                  Inches d -> Just (d/f)
                                                  _        -> Nothing)

-- | Inches.
inches :: Fractional d => Prism' (Physical d) d
inches = fromInches 1

-- | Millimeters.
mm :: Fractional d => Prism' (Physical d) d
mm = fromInches 25.4

-- | Centimeters.
cm :: Fractional d => Prism' (Physical d) d
cm = fromInches 2.54

-- | Points.  1 pt = 1/72 in.
pt :: Fractional d => Prism' (Physical d) d
pt = fromInches (1/72)

-- | Picas.  1 pc = 1/6 in.
pc :: Fractional d => Prism' (Physical d) d
pc = fromInches (1/6)

-- | Pixels.
px :: Prism' (Physical d) d
px = prism' Pixels (\phys -> case phys of
                               Pixels p -> Just p
                               _        -> Nothing)

------------------------------------------------------------
--  Measurement Units  -------------------------------------
------------------------------------------------------------
-- | Type of measurement units for attributes.
data Measure v
    = Output (Physical (Scalar v))
    | Normalized (Scalar v)
    | Local      (Scalar v)
    | Global     (Scalar v)

    | MinM (Measure v) (Measure v)
    | MaxM (Measure v) (Measure v)
    | ZeroM
    | NegateM (Measure v)
    | PlusM (Measure v) (Measure v)
    | ScaleM (Scalar v) (Measure v)
  deriving (Typeable)

deriving instance (Eq (Scalar v)) => Eq (Measure v)
deriving instance (Ord (Scalar v)) => Ord (Measure v)
deriving instance (Show (Scalar v)) => Show (Measure v)
deriving instance (Typeable v, Data v, Data (Scalar v)) => Data (Measure v)

-- | Compute the larger of two 'Measure's.  Useful for setting lower
--   bounds.
atLeast :: Measure v -> Measure v -> Measure v
atLeast = MaxM

-- | Compute the smaller of two 'Measure's.  Useful for setting upper
--   bounds.
atMost :: Measure v -> Measure v -> Measure v
atMost = MinM

instance AdditiveGroup (Measure v) where
  zeroV = ZeroM
  negateV (NegateM m) = m
  negateV m = NegateM m
  ZeroM ^+^ m = m
  m ^+^ ZeroM = m
  m1 ^+^ m2 = PlusM m1 m2

instance VectorSpace (Measure v) where
  type Scalar (Measure v) = Scalar v
  s *^ m = ScaleM s m

type instance V (Measure v) = v

instance (HasLinearMap v, Floating (Scalar v)) => Transformable (Measure v) where
  transform tr (Local x) = Local (avgScale tr * x)
  transform tr (MinM m1 m2) = MinM (transform tr m1) (transform tr m2)
  transform tr (MaxM m1 m2) = MaxM (transform tr m1) (transform tr m2)
  transform tr (NegateM m') = NegateM (transform tr m')
  transform tr (PlusM m1 m2) = PlusM (transform tr m1) (transform tr m2)
  transform tr (ScaleM s m') = ScaleM s (transform tr m')
  transform _ y = y

-- | Retrieve the 'Output' value of a 'Measure v' or throw an
--   exception.  Only 'Ouput' measures should be left in the 'RTree'
--   passed to the backend.  Returns either a 'Double' representing a
--   number of pixels, or a 'Physical' value representing a physical
--   length.  It is up to a backend to decide how to convert between
--   these, if necessary; typical scenarios might involve using some
--   standard, default pixel resolution, or requiring the user to
--   specify a resolution value manually.
fromOutput :: Measure v -> Physical (Scalar v)
fromOutput (Output o)     = o
fromOutput (Normalized _) = fromOutputErr "Normalized"
fromOutput (Local _)      = fromOutputErr "Local"
fromOutput (Global _)     = fromOutputErr "Global"
fromOutput (MinM _ _)     = fromOutputErr "MinM"
fromOutput (MaxM _ _)     = fromOutputErr "MaxM"
fromOutput (ZeroM)        = fromOutputErr "ZeroM"
fromOutput (NegateM _)    = fromOutputErr "NegateM"
fromOutput (PlusM _ _)    = fromOutputErr "PlusM"
fromOutput (ScaleM _ _)   = fromOutputErr "ScaleM"

fromOutputErr :: String -> a
fromOutputErr s = error $ "fromOutput: Cannot pass " ++ s ++ " to backends, must be Output."

--   Eventually we may use a GADT like:
--
--     data Measure o v where
--       Output     :: Scalar v -> Measure O v
--       Normalized :: Scalar v -> Measure A v
--       Global     :: Scalar v -> Measure A v
--       Local      :: Scale v  -> Measure A v
--
--   to check this at compile time. But for now we throw a runtime error.
--
--   [BAY 4 April 2014] I tried switching to such a GADT.  One tricky
--   bit is that you have to use Output :: Scalar v -> Measure o v,
--   not Measure O v: the reason is that operations like addition have
--   to take two values of the same type, so in order to be able to
--   add Output to something else, Output must be able to have an A
--   annotation.  That all works fine.  The problem is with gmapAttrs,
--   which has to preserve type: so we can't generically convert from
--   Measure A to Measure O.
