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
         SortedList
       , mkSortedList, getSortedList, onSortedList, unsafeOnSortedList

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
import           Data.List               (sort)
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
--  SortedList  --------------------------------------------
------------------------------------------------------------

-- Traces return sorted lists of intersections, so we define a newtype
-- wrapper to represent sorted lists.

-- | A newtype wrapper around a list which maintains the invariant
--   that the list is sorted.  The constructor is not exported; use
--   the smart constructor 'mkSortedList' (which sorts the given list)
--   instead.
newtype SortedList a = SortedList [a]

-- | A smart constructor for the 'SortedList' type, which sorts the
--   input to ensure the 'SortedList' invariant.
mkSortedList :: Ord a => [a] -> SortedList a
mkSortedList = SortedList . sort

-- | Project the (guaranteed sorted) list out of a 'SortedList'
--   wrapper.
getSortedList :: SortedList a -> [a]
getSortedList (SortedList as) = as

-- | Apply a list function to a 'SortedList'.  The function need not
--   result in a sorted list; the result will be sorted before being
--   rewrapped as a 'SortedList'.
onSortedList :: Ord b => ([a] -> [b]) -> (SortedList a -> SortedList b)
onSortedList f = unsafeOnSortedList (sort . f)

-- | Apply an /order-preserving/ list function to a 'SortedList'.  No
--   sorts or checks are done.
unsafeOnSortedList :: ([a] -> [b]) -> (SortedList a -> SortedList b)
unsafeOnSortedList f (SortedList as) = SortedList (f as)

-- | Merge two sorted lists.  The result is the sorted list containing
--   all the elements of both input lists (with duplicates).
merge :: Ord a => SortedList a -> SortedList a -> SortedList a
merge (SortedList as) (SortedList bs) = SortedList (merge' as bs)
  where
    merge' xs []         = xs
    merge' [] ys         = ys
    merge' (x:xs) (y:ys) =
      if x <= y
        then x : merge' xs (y:ys)
        else y : merge' (x:xs) ys

-- | 'SortedList' forms a semigroup with 'merge' as composition.
instance Ord a => Semigroup (SortedList a) where
  (<>) = merge

-- | 'SortedList' forms a monoid with 'merge' and the empty list.
instance Ord a => Monoid (SortedList a) where
  mappend = (<>)
  mempty = SortedList []

------------------------------------------------------------
--  Trace  -------------------------------------------------
------------------------------------------------------------

-- | Every diagram comes equipped with a /trace/.  Intuitively, the
--   trace for a diagram is like a raytracer: given a line
--   (represented as a base point and a direction vector), the trace
--   computes a sorted list of signed distances from the base point to
--   all intersections of the line with the boundary of the
--   diagram.
--
--   Note that the outputs are not absolute distances, but multipliers
--   relative to the input vector.  That is, if the base point is @p@
--   and direction vector is @v@, and one of the output scalars is
--   @s@, then there is an intersection at the point @p .+^ (s *^ v)@.

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

deriving instance (Ord (Scalar v)) => Semigroup (Trace v)

deriving instance (Ord (Scalar v)) => Monoid (Trace v)

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
--   which returns no intersection points for every query.  Arguably
--   it should return a single finite distance for vectors aimed
--   directly at the given point, but due to floating-point inaccuracy
--   this is problematic.  Note that the envelope for a single point
--   is /not/ the empty envelope (see "Diagrams.Core.Envelope").
instance (Ord (Scalar v), VectorSpace v) => Traced (Point v) where
  getTrace = const mempty

instance Traced t => Traced (TransInv t) where
  getTrace = getTrace . op TransInv

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
traceV p v a = case getSortedList $ op Trace (getTrace a) p v of
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
getRayTrace a = Trace $ \p v -> unsafeOnSortedList (dropWhile (<0)) $ appTrace (getTrace a) p v

-- | Compute the vector from the given point to the boundary of the
--   given object in the given direction, or @Nothing@ if there is no
--   intersection. Only positive scale muliples of the direction are returned
rayTraceV :: (Traced a, Num (Scalar (V a)))
           => Point (V a) -> V a -> a -> Maybe (V a)
rayTraceV p v a = case getSortedList $ op Trace (getRayTrace a) p v of
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
  case getSortedList $ op Trace (getRayTrace a) p v of
    [] -> Nothing
    xs -> Just ((last xs) *^ v)

-- | Like 'rayTraceP', but computes the *furthest* point on the boundary
--   instead of the closest.
maxRayTraceP :: (Traced a, Num (Scalar (V a)))
              => Point (V a) -> V a -> a -> Maybe (Point (V a))
maxRayTraceP p v a = (p .+^) <$> maxRayTraceV p v a