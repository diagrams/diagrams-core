{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
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
--   <http://byorgey.wordpress.com/2009/10/28/collecting-attributes/#comment-2030>.  See also Brent Yorgey, /Monoids: Theme and Variations/, published in the 2012 Haskell Symposium: <http://ozark.hendrix.edu/~yorgey/pub/monoid-pearl.pdf>; video: <http://www.youtube.com/watch?v=X-8NCkD2vOw>.
newtype Envelope v n = Envelope (Option (v n -> Max n))

instance Wrapped (Envelope v n) where
  type Unwrapped (Envelope v n) = Option (v n -> Max n)
  _Wrapped' = iso (\(Envelope e) -> e) Envelope

instance Rewrapped (Envelope v n) (Envelope v' n')

-- | \"Apply\" an envelope by turning it into a function.  @Nothing@
--   is returned iff the envelope is empty.
appEnvelope :: Envelope v n -> Maybe (v n -> n)
appEnvelope (Envelope (Option e)) = (getMax .) <$> e

-- | A convenient way to transform an envelope, by specifying a
--   transformation on the underlying @v n -> n@ function.  The empty
--   envelope is unaffected.
onEnvelope :: ((v n -> n) -> v n -> n) -> Envelope v n -> Envelope v n
onEnvelope t = over (_Wrapping' Envelope . mapped) ((Max .) . t . (getMax .))

-- | Create an envelope from a @v n -> n@ function.
mkEnvelope :: (v n -> n) -> Envelope v n
mkEnvelope = Envelope . Option . Just . (Max .)

-- | Create a point envelope for the given point.  A point envelope
--   has distance zero to a bounding hyperplane in every direction.
--   Note this is /not/ the same as the empty envelope.
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

type instance V (Envelope v n) = v
type instance N (Envelope v n) = n

instance Show (Envelope v n) where
  show _ = "<envelope>"

------------------------------------------------------------
--  Transforming envelopes  --------------------------------
------------------------------------------------------------

-- | The local origin of an envelope is the point with respect to
--   which bounding queries are made, /i.e./ the point from which the
--   input vectors are taken to originate.
instance (Metric v, Fractional n) => HasOrigin (Envelope v n) where
  moveOriginTo (P u) = onEnvelope $ \oldEnv v -> oldEnv v - ((u ^/ (v `dot` v)) `dot` v)
  -- For a detailed explanation of this code, see note
  -- [Transforming Envelopes] below.

instance (Metric v, Floating n) => Transformable (Envelope v n) where
  transform t = moveOriginTo (P . negated . transl $ t) . onEnvelope g
    where

      -- For a detailed explanation of this code, see note
      -- [Transforming Envelopes] below.
      g f v = f v' / (v' `dot` vi)
        where
          v' = signorm $ lapp (transp t) v
          vi = apply (inv t) v

{-

Note [Transforming Envelopes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We are given an envelope for some object, and want to apply an affine
transformation, such that the new envelope will be the envelope for
the transformed object.  The HasOrigin instance handles the
translational component; the rest of the code in the Transformable
instance handles the linear component.

See <<diagrams/EnvHasOrigin.png>>.

To implement moveOriginTo, we need to move the "base point" from which
envelope queries are made.  We are given the old envelope @oldEnv@ (a
function from vectors to scalars), a vector @u@ from the old origin to
the new origin, and a query vector @v@ which we imagine to emanate
from the new origin.  If we query the old envelope with v, it will
find the correct perpendicular hyperplane, but the reported distance
may be wrong (it will only be correct if the origin was moved in a
direction perpendicular to v).  The part that needs to be subtracted
is just the projection of u onto v, which is given by (u.v)/(v.v) *^
v.  In fact envelopes return not a distance or vector, but a scalar
which is taken to be a multiple of the query vector, so the scalar we
need to subtract is just (u.v)/(v.v).

We now consider how to apply a linear transformation to an envelope.
Recall that an envelope is a function that takes a vector and returns
a scaling factor s such that scaling the vector by s will produce a
vector to the minimum separating hyperplane.  (So if given a unit
vector as input, the output will be simply the distance to the minimum
separating hyperplane.)

We are given a linear transformation t and must produce a new envelope
function.  Given an input vector v, the "obvious" thing to do is to
transform v back into the original coordinate system using the inverse
of t, apply the original envelope, and then adjust the resulting
scalar according to how much the transformation scales v.

However, this does not work, since linear transformations do not
preserve angles.  Thus, in particular, given the query vector v and
the perpendicular separating hyperplane H which we wish to find, t^-1
v and t^-1 H are not necessarily perpendicular anymore.  So if we
query the envelope with t^-1 v we will get information about the
distance to some separating hyperplane, which when mapped forward
through t will no longer be perpendicular to v.

However, it turns out that if v and w are perpendicular, then t^-1 v
will be perpendicular to t^T w, that is, the *transpose* of t (when
considered as a matrix) applied to w.  The proof is simple. Recall
that v and w are perpendicular if and only if v . w = v^T w = 0.  Thus,

  (t^-1 v) . (t^T w) = (t^-1 v)^T (t^T w) = v^T t^-T t^T w = v^T w = 0.

Now to explain this code:

      g f v = f v' / (v' `dot` vi)
        where
          v' = signorm $ lapp (transp t) v
          vi = apply (inv t) v

In our case, our new envelope function (transformed by t) will be
given a query vector v, and we suppose v is perpendicular to the
separating hyperplane H.  Instead of querying the old envelope
function f with t^-1 v, we query it with t^T v (after normalizing),
since that vector will be perpendicular to t^-1 H.

Finally, to scale the resulting value correctly, we divide by (t^T v
. t^-1 v); I forget why.  Perhaps I will come back later and complete
this explanation.

-}

------------------------------------------------------------
--  Enveloped class
------------------------------------------------------------

-- | When dealing with envelopes we often want scalars to be an
--   ordered field (i.e. support all four arithmetic operations and be
--   totally ordered) so we introduce this constraint as a convenient
--   shorthand.
type OrderedField s = (Floating s, Ord s)

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

-- | The smallest positive /axis-parallel/ vector that bounds the
--   envelope of an object.
size :: (V a ~ v, N a ~ n, Enveloped a, HasBasis v) => a -> v n
size d = tabulate $ \(E l) -> diameter (zero & l .~ 1) d
