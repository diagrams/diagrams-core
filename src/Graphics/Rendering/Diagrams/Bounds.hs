{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Bounds
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- "Graphics.Rendering.Diagrams" defines the core library of primitives
-- forming the basis of an embedded domain-specific language for
-- describing and rendering diagrams.
--
-- The @Bounds@ module defines a data type and type class for functional
-- bounding regions.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Bounds
       ( -- * Bounding regions
         Bounds(..)

       , Boundable(..)

         -- * Utility functions
       , diameter
       , radius
       , boundaryV, boundary, boundaryFrom

         -- * Miscellaneous
       , OrderedField
       ) where

import Graphics.Rendering.Diagrams.V
import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.HasOrigin

import Data.VectorSpace
import Data.AffineSpace ((.+^))

import Data.Monoid
import Control.Applicative ((<$>), (<*>))

------------------------------------------------------------
--  Bounds  ------------------------------------------------
------------------------------------------------------------

-- | Every diagram comes equipped with a bounding function.
--   Intuitively, the bounding function for a diagram tells us the
--   minimum distance we have to go in a given direction to get to a
--   (hyper)plane entirely containing the diagram on one side of
--   it. Formally, given a vector @v@, it returns a scalar @s@ such
--   that
--
--     * for every vector @u@ with its endpoint inside the diagram,
--       if the projection of @u@ onto @v@ is @s' *^ v@, then @s' <= s@.
--
--     * @s@ is the smallest such scalar.
--
--   This could probably be expressed in terms of a Galois connection;
--   this is left as an exercise for the reader.
--
--   Essentially, bounding functions are a functional representation
--   of (a conservative approximation to) convex bounding regions.
--   The idea for this representation came from Sebastian Setzer; see
--   <http://byorgey.wordpress.com/2009/10/28/collecting-attributes/#comment-2030>.
newtype Bounds v = Bounds { appBounds :: v -> Scalar v }

--   XXX add some diagrams here to illustrate!  Note that Haddock supports
--   inline images, using a \<\<url\>\> syntax.

type instance V (Bounds v) = v

-- | Bounding functions form a monoid, with the constantly zero
--   function (/i.e./ the empty region) as the identity, and pointwise
--   maximum as composition.  Hence, if @b1@ is the bounding function
--   for diagram @d1@, and @b2@ is the bounding function for @d2@,
--   then @b1 \`mappend\` b2@ is the bounding function for @d1
--   \`atop\` d2@.
instance (Ord (Scalar v), AdditiveGroup (Scalar v)) => Monoid (Bounds v) where
  mempty = Bounds $ const zeroV
  mappend (Bounds b1) (Bounds b2) = Bounds $ max <$> b1 <*> b2

-- | The local origin of a bounding function is the point with
--   respect to which bounding queries are made, i.e. the point from
--   which the input vectors are taken to originate.
instance (InnerSpace v, AdditiveGroup (Scalar v), Fractional (Scalar v))
         => HasOrigin (Bounds v) where
  moveOriginTo (P u) (Bounds f) = Bounds $ \v -> f v ^-^ ((u ^/ (v <.> v)) <.> v)

instance Show (Bounds v) where
  show _ = "<bounds>"

------------------------------------------------------------
--  Transforming bounding regions  -------------------------
------------------------------------------------------------

-- XXX can we get away with removing this Floating constraint? It's the
--   call to normalized here which is the culprit.
instance ( HasLinearMap v, InnerSpace v
         , Floating (Scalar v), AdditiveGroup (Scalar v) )
    => Transformable (Bounds v) where
  transform t (Bounds b) =   -- XXX add lots of comments explaining this!
    moveOriginTo (P . negateV . transl $ t) $
    Bounds $ \v ->
      let v' = normalized $ lapp (transp t) v
          vi = apply (inv t) v
      in  b v' / (v' <.> vi)

------------------------------------------------------------
--  Boundable class
------------------------------------------------------------

-- | When dealing with bounding regions we often want scalars to be an
--   ordered field (i.e. support all four arithmetic operations and be
--   totally ordered) so we introduce this class as a convenient
--   shorthand.
class (Fractional s, Floating s, Ord s, AdditiveGroup s) => OrderedField s
instance (Fractional s, Floating s, Ord s, AdditiveGroup s) => OrderedField s

-- | @Boundable@ abstracts over things which can be bounded.
class (InnerSpace (V b), OrderedField (Scalar (V b))) => Boundable b where

  -- | Given a boundable object, compute a functional bounding region
  --   for it.  For types with an intrinsic notion of \"local
  --   origin\", the bounding function will be based there.  Other
  --   types (e.g. 'Trail') may have some other default reference
  --   point at which the bounding function will be based; their
  --   instances should document what it is.
  getBounds :: b -> Bounds (V b)

instance (InnerSpace v, OrderedField (Scalar v)) => Boundable (Bounds v) where
  getBounds = id

instance (Boundable b) => Boundable [b] where
  getBounds = mconcat . map getBounds

------------------------------------------------------------
--  Computing with bounds
------------------------------------------------------------

-- | Compute the vector from the local origin to a separating
--   hyperplane in the given direction.
boundaryV :: Boundable a => V a -> a -> V a
boundaryV v a = appBounds (getBounds a) v *^ v

-- | Compute the point on the boundary in the given direction.
--   Caution: this point is only valid in the local vector space of
--   the @Boundable@ object.  If you want to compute boundary points
--   of things which are subparts of a larger diagram (and hence
--   embedded within a different vector space), you must use
--   'boundaryFrom' instead.
boundary :: Boundable a => V a -> a -> Point (V a)
boundary v a = P $ boundaryV v a

-- | @boundaryFrom o v a@ computes the point along the boundary of @a@
--   in the direction of @v@, assuming that @a@'s local origin is
--   located at the point @o@ of the vector space we care about.
boundaryFrom :: Boundable a => Point (V a) -> V a -> a -> Point (V a)
boundaryFrom o v a = o .+^ boundaryV v a

-- | Compute the diameter of a boundable object along a particular
--   vector.
diameter :: Boundable a => V a -> a -> Scalar (V a)
diameter v a = f v ^+^ f (negateV v)
  where f = appBounds (getBounds a)

-- | Compute the radius (1\/2 the diameter) of a boundable object
--   along a particular vector.
radius :: Boundable a => V a -> a -> Scalar (V a)
radius v a = 0.5 * diameter v a