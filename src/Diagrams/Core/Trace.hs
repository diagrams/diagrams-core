{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Trace
-- Copyright   :  (c) 2012 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- @diagrams-core@ defines the core library of primitives
-- forming the basis of an embedded domain-specific language for
-- describing and rendering diagrams.
--
-- The @Trace@ module defines a data type and type class for
-- \"traces\", aka functional boundaries, essentially corresponding to
-- embedding a raytracer with each diagram.
--
-----------------------------------------------------------------------------

module Diagrams.Core.Trace
       ( -- * SortedList
         SortedList(SortedList)

         -- * Traces
       , Trace(Trace)

       , appTrace
       , mkTrace

         -- * Traced class

       , Traced(..)

         -- * Computing with traces

       , traceV, traceP
       , maxTraceV, maxTraceP
       , getRayTrace
       , rayTraceV, rayTraceP
       , maxRayTraceV, maxRayTraceP

       ) where

import           Control.Applicative
import           Control.Lens
import qualified Data.Map                as M
import           Data.Semigroup
import qualified Data.Set                as S

import           Data.AffineSpace
import           Data.VectorSpace

import           Diagrams.Core.HasOrigin
import           Diagrams.Core.Points
import           Diagrams.Core.Transform
import           Diagrams.Core.V

------------------------------------------------------------
--  SortedList  -------------------------------------------------
------------------------------------------------------------

newtype SortedList a = SortedList [a]

instance Wrapped [a] [a] (SortedList a) (SortedList a)
  where wrapped = iso SortedList $ \(SortedList x) -> x

instance Ord a => Semigroup (SortedList a) where
  sl0 <> sl1 = SortedList $ merge (view unwrapped sl0) (view unwrapped sl1)
    where
      merge xs []         = xs
      merge [] ys         = ys
      merge (x:xs) (y:ys) =
        if x <= y
          then x : merge xs (y:ys)
          else y : merge (x:xs) ys

instance Ord a => Monoid (SortedList a) where
  mappend = (<>)
  mempty = SortedList []

------------------------------------------------------------
--  Trace  -------------------------------------------------
------------------------------------------------------------

-- | Every diagram comes equipped with a /trace/.  Intuitively, the
--   trace for a diagram is like a raytracer: given a line
--   (represented as a base point and a direction), the trace computes
--   the distance from the base point along the line to the first
--   intersection with the diagram.  The distance can be negative if
--   the intersection is in the opposite direction from the base
--   point, or infinite if the ray never intersects the diagram.
--   Note: to obtain the distance to the /furthest/ intersection
--   instead of the /closest/, just negate the direction vector and
--   then negate the result.
--
--   Note that the output should actually be interpreted not as an
--   absolute distance, but as a multiplier relative to the input
--   vector.  That is, if the input vector is @v@ and the returned
--   scalar is @s@, the distance from the base point to the
--   intersection is given by @s * magnitude v@.

newtype Trace v = Trace { appTrace :: Point v -> v -> SortedList (Scalar v) }

instance (Scalar v ~ s, Scalar v' ~ s', s ~ s') =>
         Wrapped
         (Point v -> v -> SortedList s)
         (Point v' -> v' -> SortedList s')
         (Trace v) (Trace v')
         where wrapped = iso Trace appTrace

mkTrace :: (Point v -> v -> SortedList (Scalar v)) -> Trace v
mkTrace = Trace

-- | Traces form a semigroup with pointwise minimum as composition.
--   Hence, if @t1@ is the trace for diagram @d1@, and
--   @e2@ is the trace for @d2@, then @e1 \`mappend\` e2@
--   is the trace for @d1 \`atop\` d2@.

--instance Ord (Scalar v) => Semigroup (Trace v) where
--  ts1 <> ts2 = mkTrace tr
--    where
--      tr p v = foldr insert (appTrace ts1 p v) (appTrace ts2 p v)

deriving instance (Ord (Scalar v)) =>Semigroup (Trace v)


-- | The identity for the 'Monoid' instance is the constantly infinite
--   trace.
deriving instance (Ord (Scalar v)) => Monoid (Trace v)
--instance Ord (Scalar v) => Monoid (Trace v) where
--  mappend = (<>)
--  mempty = mkTrace $ \_ _ -> []

type instance V (Trace v) = v

instance (VectorSpace v) => HasOrigin (Trace v) where
  moveOriginTo (P u) = unwrapping Trace %~ \f p -> f (p .+^ u)

instance Show (Trace v) where
  show _ = "<trace>"

------------------------------------------------------------
--  Transforming traces  -----------------------------------
------------------------------------------------------------

instance HasLinearMap v => Transformable (Trace v) where
  transform t = unwrapped %~ \f p v -> f (papply (inv t) p) (apply (inv t) v)

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
--   envelope for a single point is /not/ the empty envelope (see
--   "Diagrams.Core.Envelope").
instance (Ord (Scalar v), VectorSpace v) => Traced (Point v) where
  getTrace = const mempty

instance Traced t => Traced (TransInv t) where
  getTrace = getTrace . view unwrapped

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
--   given object in either the given direction or the opposite direction.
--   Return @Nothing@ if there is no intersection.
traceV :: Traced a => Point (V a) -> V a -> a -> Maybe (V a)
traceV p v a = case view unwrapped $ ((getTrace a)^.unwrapping Trace) p v of
                 (s:_) -> Just (s *^ v)
                 []    -> Nothing

-- | Given a base point and direction, compute the closest point on
--   the boundary of the given object in the direction or its opposite.
--   Return @Nothing@ if there is no intersection in either direction.
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

-- | Get the trace of an object along in the direction of the given vector
--   but not in the opposite direction like `getTrace`. I.e. only return
--   positive traces.
getRayTrace :: (Traced a, Num (Scalar (V a))) => a -> Trace (V a)
getRayTrace a = Trace $ \p v ->
                  SortedList $ filter (>= 0) (view unwrapped
                             $ (((getTrace a)^.unwrapping Trace) p v))

-- | Compute the vector from the given point to the boundary of the
--   given object in the given direction, or @Nothing@ if there is no
--   intersection. Only positive scale muliples of the direction are returned
rayTraceV :: (Traced a, Num (Scalar (V a)))
           => Point (V a) -> V a -> a -> Maybe (V a)
rayTraceV p v a = case view unwrapped $ ((getRayTrace a)^.unwrapping Trace) p v of
                 (s:_) -> Just (s *^ v)
                 []    -> Nothing

-- | Given a base point and direction, compute the closest point on
--   the boundary of the given object, or @Nothing@ if there is no
--   intersection in the given direction.
rayTraceP :: (Traced a, Num (Scalar (V a)))
           => Point (V a) -> V a -> a -> Maybe (Point (V a))
rayTraceP p v a = (p .+^) <$> rayTraceV p v a

-- | Like 'rayTraceV', but computes a vector to the *furthest* point on
--   the boundary instead of the closest.
maxRayTraceV :: (Traced a, Num (Scalar (V a)))
              => Point (V a) -> V a -> a -> Maybe (V a)
maxRayTraceV p v a =
  case view unwrapped $ ((getRayTrace a)^.unwrapping Trace) p v of
    [] -> Nothing
    xs -> Just ((last xs) *^ v)

-- | Like 'rayTraceP', but computes the *furthest* point on the boundary
--   instead of the closest.
maxRayTraceP :: (Traced a, Num (Scalar (V a)))
              => Point (V a) -> V a -> a -> Maybe (Point (V a))
maxRayTraceP p v a = (p .+^) <$> maxRayTraceV p v a