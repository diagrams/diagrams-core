{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Compile
-- Copyright   :  (c) 2013 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module provides tools for compiling @QDiagrams@ into a more
-- convenient and optimized tree form, suitable for use by backends.
--
-----------------------------------------------------------------------------

module Diagrams.Core.Compile
  ( -- * Tools for backends
    RNode(..)
  , RTree
  , toRTree

    -- * Backend API

  , renderDia
  , renderDiaT

    -- * Internals

  , styleToOutput
  , toOutput
  )
  where

import           Data.Data
import qualified Data.List.NonEmpty        as NEL
import           Data.Maybe                (fromMaybe)
import           Data.Monoid.Coproduct
import           Data.Monoid.MList
import           Data.Monoid.WithSemigroup (Monoid')
import           Data.Semigroup
import           Data.Tree
import           Data.VectorSpace
import           Diagrams.Core.Envelope    (OrderedField, diameter)
import           Diagrams.Core.Style
import           Diagrams.Core.Transform
import           Diagrams.Core.Types

-- | Apply a style transformation on 'RStyle' nodes; the identity for
--   other 'RNode's.
onRStyle :: (Style v -> Style v) -> (RNode b v a -> RNode b v a)
onRStyle f (RStyle s) = RStyle (f s)
onRStyle _ n          = n

-- | Convert all 'Measure' values to 'Output' units.  The arguments
--   are, respectively, the scaling factor from global units to output
--   units, and from normalized units to output units.  It is assumed
--   that local units are identical to output units (which will be the
--   case if all transformations have been fully pushed down and
--   applied). Normalized units are based on a logical diagram size of
--   1 x 1.
styleToOutput
  :: forall v. (Data v, Data (Scalar v), Num (Scalar v), Ord (Scalar v), Fractional (Scalar v))
  => Scalar v -> Scalar v -> Style v -> Style v
styleToOutput globalToOutput normToOutput =
  gmapAttrs (toOutput globalToOutput normToOutput :: Measure v -> Measure v)

-- | Convert an aribrary 'Measure' to 'Output' units.
toOutput :: forall v. (Data v, Data (Scalar v), Num (Scalar v), Ord (Scalar v), Fractional (Scalar v))
  => Scalar v -> Scalar v -> Measure v -> Measure v
toOutput g n m =
  case m of
     m'@(Output _) -> m'
     Local s       -> Output s
     Global s      -> Output (g * s)
     Normalized s  -> Output (n * s)

     MinM m1 m2    -> outBin min (toOutput g n m1) (toOutput g n m2)
     MaxM m1 m2    -> outBin max (toOutput g n m1) (toOutput g n m2)
     ZeroM         -> Output 0
     NegateM m'    -> outUn negate (toOutput g n m')
     PlusM m1 m2   -> outBin (+) (toOutput g n m1) (toOutput g n m2)
     ScaleM s m'   -> outUn (s*) (toOutput g n m')
  where
    outUn  op (Output o1)             = Output (op o1)
    outUn  _  _ = error "outUn: The sky is falling!"
    outBin op (Output o1) (Output o2) = Output (o1 `op` o2)
    outBin _ _ _ = error "outBin: Both skies are falling!"


--------------------------------------------------

-- | Render a diagram, returning also the transformation which was
--   used to convert the diagram from its (\"global\") coordinate
--   system into the output coordinate system.  The inverse of this
--   transformation can be used, for example, to convert output/screen
--   coordinates back into diagram coordinates.  See also 'adjustDia'.
renderDiaT
  :: ( Backend b v
     , HasLinearMap v, InnerSpace v, Data v
     , OrderedField (Scalar v), Data (Scalar v)
     , Monoid' m
     )
  => b -> Options b v -> QDiagram b v m -> (Transformation v, Result b v)
renderDiaT b opts d = (g2o, renderRTree b opts' . toRTree g2o $ d')
  where (opts', g2o, d') = adjustDia b opts d

-- | Render a diagram.
renderDia
  :: ( Backend b v
     , InnerSpace v, Data v
     , OrderedField (Scalar v), Data (Scalar v)
     , Monoid' m
     )
          => b -> Options b v -> QDiagram b v m -> Result b v
renderDia b opts d = snd (renderDiaT b opts d)
