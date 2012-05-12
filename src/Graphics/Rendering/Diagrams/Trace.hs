{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Trace
-- Copyright   :  (c) 2012 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- "Graphics.Rendering.Diagrams" defines the core library of primitives
-- forming the basis of an embedded domain-specific language for
-- describing and rendering diagrams.
--
-- The @Trace@ module defines a data type and type class for
-- \"traces\", aka functional boundaries, essentially corresponding to
-- embedding a raytracer with each diagram.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Trace
       ( -- * Traces
         Trace(..)

       , inTrace
       , mkTrace

         -- * Traced class

       , Traced(..)

         -- * Computing with traces

       , traceV, traceP
       , maxTraceV, maxTraceP

       ) where

import           Control.Applicative
import qualified Data.Map as M
import           Data.Semigroup
import qualified Data.Set as S

import           Data.AffineSpace
import           Data.VectorSpace

import           Graphics.Rendering.Diagrams.HasOrigin
import           Graphics.Rendering.Diagrams.Points
import           Graphics.Rendering.Diagrams.Transform
import           Graphics.Rendering.Diagrams.V

------------------------------------------------------------
--  Adjoining positive infinity  ---------------------------
------------------------------------------------------------

data Inf a = Finite a | PosInfty
  deriving (Eq, Ord)

instance Ord a => Semigroup (Inf a) where
  (<>) = min

instance Ord a => Monoid (Inf a) where
  mempty = PosInfty
  mappend = (<>)

------------------------------------------------------------
--  Trace  -------------------------------------------------
------------------------------------------------------------

-- | Every diagram comes equipped with a *trace*.  Intuitively, the
--   trace for a diagram is like a raytracer: given a line
--   (represented as a base point + direction), the trace computes the
--   distance from the base point along the line to the first
--   intersection with the diagram.  The distance can be negative if
--   the intersection is in the opposite direction from the base
--   point, or infinite if the ray never intersects the diagram.
--   Note: to obtain the distance to the *furthest* intersection
--   instead of the *closest*, just negate the direction vector and
--   then negate the result.
--
--   Note that the output should actually be interpreted not as an
--   absolute distance, but as a multiplier relative to the input
--   vector.  That is, if the input vector is @v@ and the returned
--   scalar is @s@, the distance from the base point to the
--   intersection is given by @s *^ magnitude v@.

newtype Trace v = Trace { appTrace :: Point v -> v -> Inf (Scalar v) }

inTrace :: ((Point v -> v -> Inf (Scalar v)) -> (Point v -> v -> Inf (Scalar v)))
        -> Trace v -> Trace v
inTrace f = Trace . f . appTrace

mkTrace :: (Point v -> v -> Inf (Scalar v)) -> Trace v
mkTrace = Trace

-- | Traces form a semigroup with pointwise minimum as composition.
--   Hence, if @t1@ is the trace for diagram @d1@, and
--   @e2@ is the trace for @d2@, then @e1 \`mappend\` e2@
--   is the trace for @d1 \`atop\` d2@.
deriving instance Ord (Scalar v) => Semigroup (Trace v)

-- | The identity for the 'Monoid' instance is the constantly infinite
--   trace.
deriving instance Ord (Scalar v) => Monoid (Trace v)

type instance V (Trace v) = v

instance (VectorSpace v) => HasOrigin (Trace v) where
  moveOriginTo (P u) = inTrace $ \f p -> f (p .-^ u)

instance Show (Trace v) where
  show _ = "<trace>"

------------------------------------------------------------
--  Transforming traces  -----------------------------------
------------------------------------------------------------

instance HasLinearMap v => Transformable (Trace v) where
  transform t = inTrace $ \f p v -> f (papply (inv t) p) (apply (inv t) v)

------------------------------------------------------------
--  Traced class  ------------------------------------------
------------------------------------------------------------

-- | @Traced@ abstracts over things which have a trace.
class (Ord (Scalar (V a)), VectorSpace (V a)) => Traced a where

  -- | Compute the trace of an object.
  getTrace :: a -> Trace (V a)

instance (Ord (Scalar v), VectorSpace v) => Traced (Trace v) where
  getTrace = id

-- | The trace of a single point is the empty trace, /i.e./ the one
--   which returns positive infinity for every query.  Arguably it
--   should return a finite distance for vectors aimed directly at the
--   given point and infinity for everything else, but due to
--   floating-point inaccuracy this is problematic.  Note that the
--   envelope for a single point is *not* the empty envelope (see
--   "Graphics.Rendering.Diagrams.Envelope").
instance (Ord (Scalar v), VectorSpace v) => Traced (Point v) where
  getTrace p = mempty

instance (Traced a, Traced b, V a ~ V b) => Traced (a,b) where
  getTrace (x,y) = getTrace x <> getTrace y

instance (Traced b) => Traced [b] where
  getTrace = mconcat . map getTrace

instance (Traced b) => Traced (M.Map k b) where
  getTrace = mconcat . map getTrace . M.elems

instance (Traced b) => Traced (S.Set b) where
  getTrace = mconcat . map getTrace . S.elems

------------------------------------------------------------
--  Computing with traces  ---------------------------------
------------------------------------------------------------

-- | Compute the vector from the given point to the boundary of the
--   given object in the given direction, or @Nothing@ if there is no
--   intersection.
traceV :: Traced a => Point (V a) -> V a -> a -> Maybe (V a)
traceV p v a = case appTrace (getTrace a) p v of
                 Finite s -> Just (s *^ v)
                 PosInfty -> Nothing

-- | Given a base point and direction, compute the closest point on
--   the boundary of the given object, or @Nothing@ if there is no
--   intersection in the given direction.
traceP :: Traced a => Point (V a) -> V a -> a -> Maybe (Point (V a))
traceP p v a = (p .+^) <$> traceV p v a

-- | Like 'traceV', but computes a vector to the *furthest* point on
--   the boundary instead of the closest.
maxTraceV :: Traced a => Point (V a) -> V a -> a -> Maybe (V a)
maxTraceV p = traceV p . negateV

-- | Like 'traceP', but computes the *furthest* point on the boundary
--   instead of the closest.
maxTraceP :: Traced a => Point (V a) -> V a -> a -> Maybe (Point (V a))
maxTraceP p v a = (p .+^) <$> maxTraceV p v a