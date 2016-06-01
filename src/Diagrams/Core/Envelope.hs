{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Envelope
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- diagrams-core defines the core library of primitives forming the
-- basis of an embedded domain-specific language for describing and
-- rendering diagrams.
--
-- The @Diagrams.Core.Envelope@ module defines a data type and type class for
-- \"envelopes\", aka functional bounding regions.
--
-----------------------------------------------------------------------------

module Diagrams.Core.Envelope
       ( -- * Envelopes
         Envelope(..)

       , appEnvelope
       , onEnvelope
       , mkEnvelope
       , pointEnvelope

       , Enveloped(..)

         -- * Utility functions
       , diameter
       , radius
       , extent
       , size
       , envelopeVMay
       , envelopeV
       , envelopePMay
       , envelopeP
       , envelopeSMay
       , envelopeS

         -- * Miscellaneous
       , OrderedField
       ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative     ((<$>))
#endif
import           Control.Lens            (Rewrapped, Wrapped (..), iso, mapped,
                                          op, over, (&), (.~), _Wrapping')
import           Data.Functor.Rep
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Semigroup
import qualified Data.Set                as S

import           Diagrams.Core.HasOrigin
import           Diagrams.Core.Points
import           Diagrams.Core.Transform
import           Diagrams.Core.V

import           Linear.Metric
import           Linear.Vector


------------------------------------------------------------
--  Envelopes  ---------------------------------------------
------------------------------------------------------------

-- | Every diagram comes equipped with an /envelope/.  What is an envelope?
--
--   Consider first the idea of a /bounding box/. A bounding box
--   expresses the distance to a bounding plane in every direction
--   parallel to an axis.  That is, a bounding box can be thought of
--   as the intersection of a collection of half-planes, two
--   perpendicular to each axis.
--
--   More generally, the intersection of half-planes in /every/
--   direction would give a tight \"bounding region\", or convex hull.
--   However, representing such a thing intensionally would be
--   impossible; hence bounding boxes are often used as an
--   approximation.
--
--   An envelope is an /extensional/ representation of such a
--   \"bounding region\".  Instead of storing some sort of direct
--   representation, we store a /function/ which takes a direction as
--   input and gives a distance to a bounding half-plane as output.
--   The important point is that envelopes can be composed, and
--   transformed by any affine transformation.
--
--   Formally, given a vector @v@, the envelope computes a scalar @s@ such
--   that
--
--     * for every point @u@ inside the diagram,
--       if the projection of @(u - origin)@ onto @v@ is @s' *^ v@, then @s' <= s@.
--
--     * @s@ is the smallest such scalar.
--
--   There is also a special \"empty envelope\".
--
--   The idea for envelopes came from
--   Sebastian Setzer; see
--   <http://byorgey.wordpress.com/2009/10/28/collecting-attributes/#comment-2030>.  See also Brent Yorgey, /Monoids: Theme and Variations/, published in the 2012 Haskell Symposium: <http://www.cis.upenn.edu/~byorgey/pub/monoid-pearl.pdf>; video: <http://www.youtube.com/watch?v=X-8NCkD2vOw>.
newtype Envelope v n = Envelope (Option (v n -> Max n))

instance Wrapped (Envelope v n) where
  type Unwrapped (Envelope v n) = Option (v n -> Max n)
  _Wrapped' = iso (\(Envelope e) -> e) Envelope

instance Rewrapped (Envelope v n) (Envelope v' n')

appEnvelope :: Envelope v n -> Maybe (v n -> n)
appEnvelope (Envelope (Option e)) = (getMax .) <$> e

onEnvelope :: ((v n -> n) -> v n -> n) -> Envelope v n -> Envelope v n
onEnvelope t = over (_Wrapping' Envelope . mapped) ((Max .) . t . (getMax .))

mkEnvelope :: (v n -> n) -> Envelope v n
mkEnvelope = Envelope . Option . Just . (Max .)

-- | Create an envelope for the given point.
pointEnvelope :: (Fractional n, Metric v) => Point v n -> Envelope v n
pointEnvelope p = moveTo p (mkEnvelope $ const 0)

-- | Envelopes form a semigroup with pointwise maximum as composition.
--   Hence, if @e1@ is the envelope for diagram @d1@, and
--   @e2@ is the envelope for @d2@, then @e1 \`mappend\` e2@
--   is the envelope for @d1 \`atop\` d2@.
deriving instance Ord n => Semigroup (Envelope v n)

-- | The special empty envelope is the identity for the
--   'Monoid' instance.
deriving instance Ord n => Monoid (Envelope v n)


--   XXX add some diagrams here to illustrate!  Note that Haddock supports
--   inline images, using a \<\<url\>\> syntax.

type instance V (Envelope v n) = v
type instance N (Envelope v n) = n

-- | The local origin of an envelope is the point with respect to
--   which bounding queries are made, /i.e./ the point from which the
--   input vectors are taken to originate.
instance (Metric v, Fractional n) => HasOrigin (Envelope v n) where
  moveOriginTo (P u) = onEnvelope $ \f v -> f v - ((u ^/ (v `dot` v)) `dot` v)

instance Show (Envelope v n) where
  show _ = "<envelope>"

------------------------------------------------------------
--  Transforming envelopes  --------------------------------
------------------------------------------------------------

instance (Metric v, Floating n) => Transformable (Envelope v n) where
  transform t = moveOriginTo (P . negated . transl $ t) . onEnvelope g
    where
      -- XXX add lots of comments explaining this!
      g f v = f v' / (v' `dot` vi)
        where
          v' = signorm $ lapp (transp t) v
          vi = apply (inv t) v

------------------------------------------------------------
--  Enveloped class
------------------------------------------------------------

-- | When dealing with envelopes we often want scalars to be an
--   ordered field (i.e. support all four arithmetic operations and be
--   totally ordered) so we introduce this class as a convenient
--   shorthand.
class (Floating s, Ord s) => OrderedField s
instance (Floating s, Ord s) => OrderedField s

-- | @Enveloped@ abstracts over things which have an envelope.
class (Metric (V a), OrderedField (N a)) => Enveloped a where

  -- | Compute the envelope of an object.  For types with an intrinsic
  --   notion of \"local origin\", the envelope will be based there.
  --   Other types (e.g. 'Trail') may have some other default
  --   reference point at which the envelope will be based; their
  --   instances should document what it is.
  getEnvelope :: a -> Envelope (V a) (N a)

instance (Metric v, OrderedField n) => Enveloped (Envelope v n) where
  getEnvelope = id

instance (OrderedField n, Metric v) => Enveloped (Point v n) where
  getEnvelope p = moveTo p . mkEnvelope $ const 0

instance Enveloped t => Enveloped (TransInv t) where
  getEnvelope = getEnvelope . op TransInv

instance (Enveloped a, Enveloped b, V a ~ V b, N a ~ N b) => Enveloped (a,b) where
  getEnvelope (x,y) = getEnvelope x <> getEnvelope y

instance Enveloped b => Enveloped [b] where
  getEnvelope = mconcat . map getEnvelope

instance Enveloped b => Enveloped (M.Map k b) where
  getEnvelope = mconcat . map getEnvelope . M.elems

instance Enveloped b => Enveloped (S.Set b) where
  getEnvelope = mconcat . map getEnvelope . S.elems

------------------------------------------------------------
--  Computing with envelopes
------------------------------------------------------------

-- | Compute the vector from the local origin to a separating
--   hyperplane in the given direction, or @Nothing@ for the empty
--   envelope.
envelopeVMay :: Enveloped a => Vn a -> a -> Maybe (Vn a)
envelopeVMay v = fmap ((*^ v) . ($ v)) . appEnvelope . getEnvelope

-- | Compute the vector from the local origin to a separating
--   hyperplane in the given direction.  Returns the zero vector for
--   the empty envelope.
envelopeV :: Enveloped a => Vn a -> a -> Vn a
envelopeV v = fromMaybe zero . envelopeVMay v

-- | Compute the point on a separating hyperplane in the given
--   direction, or @Nothing@ for the empty envelope.
envelopePMay :: (V a ~ v, N a ~ n, Enveloped a) => v n -> a -> Maybe (Point v n)
envelopePMay v = fmap P . envelopeVMay v

-- | Compute the point on a separating hyperplane in the given
--   direction.  Returns the origin for the empty envelope.
envelopeP :: (V a ~ v, N a ~ n, Enveloped a) => v n -> a -> Point v n
envelopeP v = P . envelopeV v

-- | Equivalent to the norm of 'envelopeVMay':
--
--   @ envelopeSMay v x == fmap norm (envelopeVMay v x) @
--
--   (other than differences in rounding error)
--
--   Note that the 'envelopeVMay' / 'envelopePMay' functions above should be
--   preferred, as this requires a call to norm.  However, it is more
--   efficient than calling norm on the results of those functions.
envelopeSMay :: (V a ~ v, N a ~ n, Enveloped a) => v n -> a -> Maybe n
envelopeSMay v = fmap ((* norm v) . ($ v)) . appEnvelope . getEnvelope

-- | Equivalent to the norm of 'envelopeV':
--
--   @ envelopeS v x == norm (envelopeV v x) @
--
--   (other than differences in rounding error)
--
--   Note that the 'envelopeV' / 'envelopeP' functions above should be
--   preferred, as this requires a call to norm. However, it is more
--   efficient than calling norm on the results of those functions.
envelopeS :: (V a ~ v, N a ~ n, Enveloped a) => v n -> a -> n
envelopeS v = fromMaybe 0 . envelopeSMay v

-- | Compute the diameter of a enveloped object along a particular
--   vector.  Returns zero for the empty envelope.
diameter :: (V a ~ v, N a ~ n, Enveloped a) => v n -> a -> n
diameter v a = maybe 0 (\(lo,hi) -> (hi - lo) * norm v) (extent v a)

-- | Compute the \"radius\" (1\/2 the diameter) of an enveloped object
--   along a particular vector.
radius :: (V a ~ v, N a ~ n, Enveloped a) => v n -> a -> n
radius v = (0.5*) . diameter v

-- | Compute the range of an enveloped object along a certain
--   direction.  Returns a pair of scalars @(lo,hi)@ such that the
--   object extends from @(lo *^ v)@ to @(hi *^ v)@. Returns @Nothing@
--   for objects with an empty envelope.
extent :: (V a ~ v, N a ~ n, Enveloped a) => v n -> a -> Maybe (n, n)
extent v a = (\f -> (-f (negated v), f v)) <$> (appEnvelope . getEnvelope $ a)

-- | The smallest positive vector that bounds the envelope of an object.
size :: (V a ~ v, N a ~ n, Enveloped a, HasBasis v) => a -> v n
size d = tabulate $ \(E l) -> diameter (zero & l .~ 1) d
