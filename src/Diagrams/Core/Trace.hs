{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
  -- for Data.Semigroup import, which becomes redundant under GHC 8.4

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Trace
-- Copyright   :  (c) 2012-2015 diagrams-core team (see LICENSE)
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

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Lens
import           Data.List               (sort)
import qualified Data.Map                as M
import           Data.Semigroup
import qualified Data.Set                as S

import           Diagrams.Core.HasOrigin
import           Diagrams.Core.Transform
import           Diagrams.Core.V

import           Linear.Affine
import           Linear.Vector


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
onSortedList :: Ord b => ([a] -> [b]) -> SortedList a -> SortedList b
onSortedList f = unsafeOnSortedList (sort . f)

-- | Apply an /order-preserving/ list function to a 'SortedList'.  No
--   sorts or checks are done.
unsafeOnSortedList :: ([a] -> [b]) -> SortedList a -> SortedList b
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

-- > traceEx = mkTraceDia def

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
--
--   <<diagrams/src_Diagrams_Core_Trace_traceEx.svg#diagram=traceEx&width=200>>

newtype Trace v n = Trace { appTrace :: Point v n -> v n -> SortedList n }

instance Wrapped (Trace v n) where
  type Unwrapped (Trace v n) = Point v n -> v n -> SortedList n
  _Wrapped' = iso appTrace Trace

instance Rewrapped (Trace v n) (Trace v' n')

mkTrace :: (Point v n -> v n -> SortedList n) -> Trace v n
mkTrace = Trace

-- | Traces form a semigroup with pointwise minimum as composition.
--   Hence, if @t1@ is the trace for diagram @d1@, and
--   @e2@ is the trace for @d2@, then @e1 \`mappend\` e2@
--   is the trace for @d1 \`atop\` d2@.

deriving instance (Ord n) => Semigroup (Trace v n)

deriving instance (Ord n) => Monoid (Trace v n)

type instance V (Trace v n) = v
type instance N (Trace v n) = n

instance (Additive v, Num n) => HasOrigin (Trace v n) where
  moveOriginTo (P u) = _Wrapping' Trace %~ \f p -> f (p .+^ u)

instance Show (Trace v n) where
  show _ = "<trace>"

------------------------------------------------------------
--  Transforming traces  -----------------------------------
------------------------------------------------------------

instance (Additive v, Num n) => Transformable (Trace v n) where
  transform t = _Wrapped %~ \f p v -> f (papply (inv t) p) (apply (inv t) v)

------------------------------------------------------------
--  Traced class  ------------------------------------------
------------------------------------------------------------

-- | @Traced@ abstracts over things which have a trace.
class (Additive (V a), Ord (N a)) => Traced a where

  -- | Compute the trace of an object.
  getTrace :: a -> Trace (V a) (N a)

instance (Additive v, Ord n) => Traced (Trace v n) where
  getTrace = id

-- | The trace of a single point is the empty trace, /i.e./ the one
--   which returns no intersection points for every query.  Arguably
--   it should return a single finite distance for vectors aimed
--   directly at the given point, but due to floating-point inaccuracy
--   this is problematic.  Note that the envelope for a single point
--   is /not/ the empty envelope (see "Diagrams.Core.Envelope").
instance (Additive v, Ord n) => Traced (Point v n) where
  getTrace = const mempty

instance Traced t => Traced (TransInv t) where
  getTrace = getTrace . op TransInv

instance (Traced a, Traced b, SameSpace a b) => Traced (a,b) where
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

-- | Compute the vector from the given point @p@ to the \"smallest\"
--   boundary intersection along the given vector @v@.  The
--   \"smallest\" boundary intersection is defined as the one given by
--   @p .+^ (s *^ v)@ for the smallest (most negative) value of
--   @s@. Return @Nothing@ if there is no intersection.  See also
--   'traceP'.
--
--   See also 'rayTraceV' which uses the smallest /positive/
--   intersection, which is often more intuitive behavior.
--
--   <<diagrams/src_Diagrams_Core_Trace_traceVEx.svg#diagram=traceVEx&width=600>>
traceV :: (n ~ N a, Num n, Traced a) => Point (V a) n -> V a n -> a -> Maybe (V a n)
traceV p v a = case getSortedList $ op Trace (getTrace a) p v of
                 (s:_) -> Just (s *^ v)
                 []    -> Nothing

-- > traceVEx = mkTraceDiasABC def { drawV = True, sFilter = take 1 }


-- | Compute the \"smallest\" boundary point along the line determined
--   by the given point @p@ and vector @v@.  The \"smallest\" boundary
--   point is defined as the one given by @p .+^ (s *^ v)@ for
--   the smallest (most negative) value of @s@. Return @Nothing@ if
--   there is no such boundary point.  See also 'traceV'.
--
--   See also 'rayTraceP' which uses the smallest /positive/
--   intersection, which is often more intuitive behavior.
--
--   <<diagrams/src_Diagrams_Core_Trace_tracePEx.svg#diagram=tracePEx&width=600>>
traceP :: (n ~ N a, Traced a, Num n) => Point (V a) n -> V a n -> a -> Maybe (Point (V a) n)
traceP p v a = (p .+^) <$> traceV p v a

-- > tracePEx = mkTraceDiasABC def { sFilter = take 1 }


-- | Like 'traceV', but computes a vector to the \"largest\" boundary
--   point instead of the smallest. (Note, however, the \"largest\"
--   boundary point may still be in the opposite direction from the
--   given vector, if all the boundary points are, as in the third
--   example shown below.)
--
--   <<diagrams/src_Diagrams_Core_Trace_maxTraceVEx.svg#diagram=maxTraceVEx&width=600>>
maxTraceV :: (n ~ N a, Num n, Traced a) => Point (V a) n -> V a n -> a -> Maybe (V a n)
maxTraceV p = traceV p . negated

-- > maxTraceVEx = mkTraceDiasABC def { drawV = True, sFilter = dropAllBut1 }


-- | Like 'traceP', but computes the \"largest\" boundary point
--   instead of the smallest. (Note, however, the \"largest\" boundary
--   point may still be in the opposite direction from the given
--   vector, if all the boundary points are.)
--
--   <<diagrams/src_Diagrams_Core_Trace_maxTracePEx.svg#diagram=maxTracePEx&width=600>>
maxTraceP :: (n ~ N a, Num n, Traced a) => Point (V a) n -> V a n -> a -> Maybe (Point (V a) n)
maxTraceP p v a = (p .+^) <$> maxTraceV p v a

-- > maxTracePEx = mkTraceDiasABC def { sFilter = dropAllBut1 }


-- | Get a modified 'Trace' for an object which only returns positive
--   boundary points, /i.e./ those boundary points given by a positive
--   scalar multiple of the direction vector.  Note, this property
--   will be destroyed if the resulting 'Trace' is translated at all.
getRayTrace :: (n ~ N a, Traced a, Num n) => a -> Trace (V a) n
getRayTrace a = Trace $ \p v -> unsafeOnSortedList (dropWhile (<0)) $ appTrace (getTrace a) p v

-- | Compute the vector from the given point to the closest boundary
--   point of the given object in the given direction, or @Nothing@ if
--   there is no such boundary point (as in the third example
--   below). Note that unlike 'traceV', only /positive/ boundary
--   points are considered, /i.e./ boundary points corresponding to a
--   positive scalar multiple of the direction vector.  This is
--   intuitively the \"usual\" behavior of a raytracer, which only
--   considers intersections \"in front of\" the camera.  Compare the
--   second example diagram below with the second example shown for
--   'traceV'.
--
--   <<diagrams/src_Diagrams_Core_Trace_rayTraceVEx.svg#diagram=rayTraceVEx&width=600>>
rayTraceV :: (n ~ N a, Traced a, Num n)
           => Point (V a) n -> V a n -> a -> Maybe (V a n)
rayTraceV p v a = case getSortedList $ op Trace (getRayTrace a) p v of
                 (s:_) -> Just (s *^ v)
                 []    -> Nothing

-- > rayTraceVEx = mkTraceDiasABC def { drawV = True, sFilter = take 1 . filter (>0) }


-- | Compute the boundary point on an object which is closest to the
--   given base point in the given direction, or @Nothing@ if there is
--   no such boundary point. Note that unlike 'traceP', only /positive/
--   boundary points are considered, /i.e./ boundary points
--   corresponding to a positive scalar multiple of the direction
--   vector.  This is intuitively the \"usual\" behavior of a raytracer,
--   which only considers intersection points \"in front of\" the
--   camera.
--
--   <<diagrams/src_Diagrams_Core_Trace_rayTracePEx.svg#diagram=rayTracePEx&width=600>>
rayTraceP :: (n ~ N a, Traced a, Num n)
           => Point (V a) n -> V a n -> a -> Maybe (Point (V a) n)
rayTraceP p v a = (p .+^) <$> rayTraceV p v a

-- > rayTracePEx = mkTraceDiasABC def { sFilter = take 1 . filter (>0) }


-- | Like 'rayTraceV', but computes a vector to the \"largest\"
--   boundary point instead of the smallest.  Considers only
--   /positive/ boundary points.
--
--   <<diagrams/src_Diagrams_Core_Trace_maxRayTraceVEx.svg#diagram=maxRayTraceVEx&width=600>>
maxRayTraceV :: (n ~ N a, Traced a, Num n)
              => Point (V a) n -> V a n -> a -> Maybe (V a n)
maxRayTraceV p v a =
  case getSortedList $ op Trace (getRayTrace a) p v of
    [] -> Nothing
    xs -> Just (last xs *^ v)

-- > maxRayTraceVEx = mkTraceDiasABC def { drawV = True, sFilter = dropAllBut1 . filter (>0) }


-- | Like 'rayTraceP', but computes the \"largest\" boundary point
--   instead of the smallest.  Considers only /positive/ boundary
--   points.
--
--   <<diagrams/src_Diagrams_Core_Trace_maxRayTracePEx.svg#diagram=maxRayTracePEx&width=600>>
maxRayTraceP :: (n ~ N a, Traced a, Num n)
              => Point (V a) n -> V a n -> a -> Maybe (Point (V a) n)
maxRayTraceP p v a = (p .+^) <$> maxRayTraceV p v a

-- > maxRayTracePEx = mkTraceDiasABC def { sFilter = dropAllBut1 . filter (>0) }


------------------------------------------------------------
-- Drawing trace diagrams
------------------------------------------------------------

-- > import Data.Default.Class
-- > import Control.Lens ((^.))
-- > import Data.Maybe (fromMaybe)
-- >
-- > thingyT :: Trail V2 Double
-- > thingyT =
-- >   fromOffsets
-- >     [ 3 *^ unitX, 3 *^ unitY, 2 *^ unit_X, 1 *^ unit_Y
-- >     , 1 *^ unitX, 1 *^ unit_Y, 2 *^ unit_X, 1 *^ unit_Y ]
-- >
-- > thingy = strokeTrail thingyT
-- >
-- > data TraceDiaOpts
-- >   = TDO { traceShape :: Diagram B
-- >         , basePt     :: P2 Double
-- >         , dirV       :: V2 Double
-- >         , sFilter    :: [Double] -> [Double]
-- >         , drawV      :: Bool
-- >         }
-- >
-- > instance Default TraceDiaOpts where
-- >   def = TDO { traceShape = thingy
-- >             , basePt     = pointB
-- >             , dirV       = 0.3 ^& 0.5
-- >             , sFilter    = id
-- >             , drawV      = False
-- >             }
-- >
-- > pointA = 1 ^& (-1.5)
-- > pointB = 1 ^& 1.2
-- > pointC = 2.5 ^& 3.5
-- >
-- > dot' = circle 0.05 # lw none
-- >
-- > mkTraceDia :: TraceDiaOpts -> Diagram B
-- > mkTraceDia tdo = mconcat
-- >   [ mconcat $ map (place (dot' # fc red)) pts
-- >   , if drawV tdo then resultArrow else mempty
-- >   , arrowAt (basePt tdo) (dirV tdo) # lc blue
-- >   , dot' # fc blue # moveTo (basePt tdo)
-- >   , traceLine (basePt tdo) maxPosPt
-- >   , traceLine (basePt tdo) minNegPt
-- >   , traceShape tdo
-- >   ]
-- >   # centerXY # pad 1.1
-- >   where
-- >     ss  = sFilter tdo . getSortedList
-- >         $ appTrace (traceShape tdo ^. trace) (basePt tdo) (dirV tdo)
-- >     pts = map mkPt ss
-- >     mkPt s = basePt tdo .+^ (s *^ dirV tdo)
-- >     maxPosPt = (mkPt <$>) . safeLast $ filter (>0) ss
-- >     minNegPt = (mkPt <$>) . safeHead $ filter (<0) ss
-- >     minPt = (mkPt <$>) . safeHead $ ss
-- >     resultArrow = fromMaybe mempty (arrowBetween (basePt tdo) <$> minPt)
-- >       # lc green
-- >
-- > safeLast [] = Nothing
-- > safeLast xs = Just $ last xs
-- > safeHead [] = Nothing
-- > safeHead (x:_) = Just x
-- > dropAllBut1 [] = []
-- > dropAllBut1 xs = [last xs]
-- >
-- > traceLine _ Nothing = mempty
-- > traceLine p (Just q) = (p ~~ q) # dashingG [0.1,0.1] 0
-- >
-- > mkTraceDias :: [TraceDiaOpts] -> Diagram B
-- > mkTraceDias = hcat' (with & sep .~ 1) . map mkTraceDia
-- >
-- > mkTraceDiasABC :: TraceDiaOpts -> Diagram B
-- > mkTraceDiasABC tdo = mkTraceDias (map (\p -> tdo { basePt = p }) [pointA, pointB, pointC])
