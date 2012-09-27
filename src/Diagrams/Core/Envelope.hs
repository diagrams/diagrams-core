{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , MultiParamTypeClasses
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Envelope
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- "Graphics.Rendering.Diagrams" defines the core library of primitives
-- forming the basis of an embedded domain-specific language for
-- describing and rendering diagrams.
--
-- The @Envelope@ module defines a data type and type class for
-- \"envelopes\", aka functional bounding regions.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Envelope
       ( -- * Envelopes
         Envelope(..)

       , inEnvelope
       , appEnvelope
       , onEnvelope
       , mkEnvelope
       , pointEnvelope

       , Enveloped(..)

         -- * Utility functions
       , diameter
       , radius
       , envelopeVMay, envelopeV, envelopePMay, envelopeP

         -- * Miscellaneous
       , OrderedField
       ) where

import           Control.Applicative ((<$>))
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Semigroup
import qualified Data.Set as S

import           Data.AffineSpace ((.+^), (.-^))
import           Data.VectorSpace

import           Graphics.Rendering.Diagrams.HasOrigin
import           Graphics.Rendering.Diagrams.Points
import           Graphics.Rendering.Diagrams.Transform
import           Graphics.Rendering.Diagrams.V

------------------------------------------------------------
--  Envelopes  ---------------------------------------------
------------------------------------------------------------

-- | Every diagram comes equipped with an /envelope/.
--   Intuitively, the envelope for a diagram tells us the
--   minimum distance we have to go in a given direction to get to a
--   (hyper)plane entirely containing the diagram on one side of
--   it. Formally, given a vector @v@, it returns a scalar @s@ such
--   that
--
--     * for every point @u@ inside the diagram,
--       if the projection of @(u - origin)@ onto @v@ is @s' *^ v@, then @s' <= s@.
--
--     * @s@ is the smallest such scalar.
--
--   This could probably be expressed in terms of a Galois connection;
--   this is left as an exercise for the reader.
--
--   There is also a special \"empty envelope\".
--
--   Essentially, envelopes are a functional/extensional
--   representation of (a conservative approximation to) convex
--   bounding regions.  The idea for this representation came from
--   Sebastian Setzer; see
--   <http://byorgey.wordpress.com/2009/10/28/collecting-attributes/#comment-2030>.
newtype Envelope v = Envelope { unEnvelope :: Option (v -> Max (Scalar v)) }

inEnvelope :: (Option (v -> Max (Scalar v)) -> Option (v -> Max (Scalar v)))
           -> Envelope v -> Envelope v
inEnvelope f = Envelope . f . unEnvelope

appEnvelope :: Envelope v -> Maybe (v -> Scalar v)
appEnvelope (Envelope (Option e)) = (getMax .) <$> e

onEnvelope :: ((v -> Scalar v) -> (v -> Scalar v)) -> Envelope v -> Envelope v
onEnvelope t = (inEnvelope . fmap) ((Max .) . t . (getMax .))

mkEnvelope :: (v -> Scalar v) -> Envelope v
mkEnvelope = Envelope . Option . Just . (Max .)

-- | Create an envelope for the given point.
pointEnvelope :: (AdditiveGroup (Scalar v), Fractional (Scalar v), InnerSpace v)
              => Point v -> Envelope v
pointEnvelope p = moveTo p (mkEnvelope (const zeroV))

-- | Envelopes form a semigroup with pointwise maximum as composition.
--   Hence, if @e1@ is the envelope for diagram @d1@, and
--   @e2@ is the envelope for @d2@, then @e1 \`mappend\` e2@
--   is the envelope for @d1 \`atop\` d2@.
deriving instance Ord (Scalar v) => Semigroup (Envelope v)

-- | The special empty envelope is the identity for the
--   'Monoid' instance.
deriving instance Ord (Scalar v) => Monoid (Envelope v)



--   XXX add some diagrams here to illustrate!  Note that Haddock supports
--   inline images, using a \<\<url\>\> syntax.

type instance V (Envelope v) = v

-- | The local origin of an envelope is the point with respect to
--   which bounding queries are made, /i.e./ the point from which the
--   input vectors are taken to originate.
instance (InnerSpace v, AdditiveGroup (Scalar v), Fractional (Scalar v))
         => HasOrigin (Envelope v) where
  moveOriginTo (P u) = onEnvelope $ \f v -> f v ^-^ ((u ^/ (v <.> v)) <.> v)

instance Show (Envelope v) where
  show _ = "<envelope>"

------------------------------------------------------------
--  Transforming envelopes  --------------------------------
------------------------------------------------------------

-- XXX can we get away with removing this Floating constraint? It's the
--   call to normalized here which is the culprit.
instance ( HasLinearMap v, InnerSpace v
         , Floating (Scalar v), AdditiveGroup (Scalar v) )
    => Transformable (Envelope v) where
  transform t =   -- XXX add lots of comments explaining this!
    moveOriginTo (P . negateV . transl $ t) .
    (onEnvelope $ \f v ->
      let v' = normalized $ lapp (transp t) v
          vi = apply (inv t) v
      in  f v' / (v' <.> vi)
    )

------------------------------------------------------------
--  Enveloped class
------------------------------------------------------------

-- | When dealing with envelopes we often want scalars to be an
--   ordered field (i.e. support all four arithmetic operations and be
--   totally ordered) so we introduce this class as a convenient
--   shorthand.
class (Fractional s, Floating s, Ord s, AdditiveGroup s) => OrderedField s
instance (Fractional s, Floating s, Ord s, AdditiveGroup s) => OrderedField s

-- | @Enveloped@ abstracts over things which have an envelope.
class (InnerSpace (V a), OrderedField (Scalar (V a))) => Enveloped a where

  -- | Compute the envelope of an object.  For types with an intrinsic
  --   notion of \"local origin\", the envelope will be based there.
  --   Other types (e.g. 'Trail') may have some other default
  --   reference point at which the envelope will be based; their
  --   instances should document what it is.
  getEnvelope :: a -> Envelope (V a)

instance (InnerSpace v, OrderedField (Scalar v)) => Enveloped (Envelope v) where
  getEnvelope = id

instance (OrderedField (Scalar v), InnerSpace v) => Enveloped (Point v) where
  getEnvelope p = moveTo p . mkEnvelope $ const zeroV

instance (Enveloped a, Enveloped b, V a ~ V b) => Enveloped (a,b) where
  getEnvelope (x,y) = getEnvelope x <> getEnvelope y

instance (Enveloped b) => Enveloped [b] where
  getEnvelope = mconcat . map getEnvelope

instance (Enveloped b) => Enveloped (M.Map k b) where
  getEnvelope = mconcat . map getEnvelope . M.elems

instance (Enveloped b) => Enveloped (S.Set b) where
  getEnvelope = mconcat . map getEnvelope . S.elems

------------------------------------------------------------
--  Computing with envelopes
------------------------------------------------------------

-- | Compute the vector from the local origin to a separating
--   hyperplane in the given direction, or @Nothing@ for the empty
--   envelope.
envelopeVMay :: Enveloped a => V a -> a -> Maybe (V a)
envelopeVMay v = fmap ((*^ v) . ($ v)) . appEnvelope . getEnvelope

-- | Compute the vector from the local origin to a separating
--   hyperplane in the given direction.  Returns the zero vector for
--   the empty envelope.
envelopeV :: Enveloped a => V a -> a -> V a
envelopeV v = fromMaybe zeroV . envelopeVMay v

-- | Compute the point on a separating hyperplane in the given
--   direction, or @Nothing@ for the empty envelope.
envelopePMay :: Enveloped a => V a -> a -> Maybe (Point (V a))
envelopePMay v = fmap P . envelopeVMay v

-- | Compute the point on a separating hyperplane in the given
--   direction.  Returns the origin for the empty envelope.
envelopeP :: Enveloped a => V a -> a -> Point (V a)
envelopeP v = P . envelopeV v

-- | Compute the diameter of a enveloped object along a particular
--   vector.  Returns zero for the empty envelope.
diameter :: Enveloped a => V a -> a -> Scalar (V a)
diameter v a = magnitude (envelopeV v a ^-^ envelopeV (negateV v) a)

-- | Compute the \"radius\" (1\/2 the diameter) of an enveloped object
--   along a particular vector.
radius :: Enveloped a => V a -> a -> Scalar (V a)
radius v = (0.5*) . diameter v