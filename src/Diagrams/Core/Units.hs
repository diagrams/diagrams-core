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

import           Control.Lens            (Iso', iso)
import           Data.Data
import           Data.VectorSpace

------------------------------------------------------------
--  Physical Units  ----------------------------------------
------------------------------------------------------------

newtype Physical = Inches Double
  deriving (Read, Show, Eq, Ord, Enum, AdditiveGroup, Typeable, Data)

instance VectorSpace Physical where
  type Scalar Physical = Double
  s *^ Inches m = Inches (s * m)

-- | The first argument is the conversion factor from inches, /i.e./
--   what do you multiply inches by to obtain the unit in question?
--   For example, 'mm' is defined as 'fromInches 25.4', since there
--   are 25.4 millimeters in one inch.
fromInches :: Double -> Iso' Physical Double
fromInches f = iso (\(Inches m) -> m/f) (Inches . (*f))

-- | Inches.
inches :: Iso' Physical Double
inches = fromInches 1

-- | Millimeters.
mm :: Iso' Physical Double
mm = fromInches 25.4

-- | Centimeters.
cm :: Iso' Physical Double
cm = fromInches 2.54

-- | Points.  1 pt = 1/72 in.
pt :: Iso' Physical Double
pt = fromInches (1/72)

-- | Picas.  1 pc = 1/6 in.
pc :: Iso' Physical Double
pc = fromInches (1/6)

------------------------------------------------------------
--  Measurement Units  -------------------------------------
------------------------------------------------------------
-- | Type of measurement units for attributes.
data Measure v = OutputPx   Double
               | OutputPhys Physical
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
  transform _ y = y

-- | Retrieve the 'Output' value of a 'Measure v' or throw an
--   exception.  Only 'Ouput' measures should be left in the 'RTree'
--   passed to the backend.  Returns either a 'Double' representing a
--   number of pixels, or a 'Physical' value representing a physical
--   length.  It is up to a backend to decide how to convert between
--   these, if necessary; typical scenarios might involve using some
--   standard, default pixel resolution, or requiring the user to
--   specify a resolution value manually.
fromOutput :: Measure v -> Either Double Physical
fromOutput (OutputPx o)   = Left o
fromOutput (OutputPhys o) = Right o
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
--   > data Measure o v where
--   > Output :: Scalar v -> Measure O v
--   > Normalized :: Scalar v -> Measure A v
--   > Global :: Scalar v -> Measure A v
--   > Local :: Scale v -> Measure A v
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
