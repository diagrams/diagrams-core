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
-- Graphics.Rendering.Diagrams defines the core library of primitives
-- forming the basis of an embedded domain-specific language for
-- describing and rendering diagrams.
--
-- The Bounds module defines a type class and data type for functional
-- bounding regions.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams.Bounds
       ( Boundable(..)
       , Bounds(..)
       , diameter
       , radius

       , OrderedField
       ) where

import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.HasOrigin

import Data.VectorSpace

import Data.Monoid
import Control.Applicative ((<$>), (<*>))

------------------------------------------------------------
--  Bounds  ------------------------------------------------
------------------------------------------------------------

-- | Every diagram comes equipped with a bounding function.
--   Intuitively, the bounding function for a diagram tells us the
--   minimum distance we have to go in any given direction to get to a
--   (hyper)plane entirely containing the diagram on one side of
--   it. Formally, given a vector @v@, it returns a scalar @s@ such
--   that
--
--     * for every vector @u@ with its endpoint inside the diagram,
--       if the projection of @u@ onto @v@ is @s' *^ v@, then @s' <= s@.
--
--     * @s@ is the smallest such scalar.
--
--   Essentially, bounding functions are a functional representation
--   of convex bounding regions.  The idea for this representation
--   came from Sebastian Setzer: see <http://byorgey.wordpress.com/2009/10/28/collecting-attributes/#comment-2030>.
--
--   XXX add some diagrams here to illustrate!  Note that Haddock supports
--   inline images, using a \<\<url\>\> syntax.
newtype Bounds v = Bounds { getBoundFunc :: v -> Scalar v }

-- | Bounding functions form a monoid, with the constantly zero
--   function (/i.e./ the empty region) as the identity, and pointwise
--   maximum as composition.  Hence, if @b1@ is the bounding function
--   for diagram @d1@, and @b2@ is the bounding function for @d2@,
--   then @b1 \`mappend\` b2@ is the bounding function for @d1
--   \`atop\` d2@.
instance (Ord (Scalar v), AdditiveGroup (Scalar v)) => Monoid (Bounds v) where
  mempty = Bounds $ const zeroV
  mappend (Bounds b1) (Bounds b2) = Bounds $ max <$> b1 <*> b2

-- | The local origin of a bounding function is the points with
--   respect to which bounding queries are made, i.e. the point from
--   which the input vectors are taken to originate.
instance (InnerSpace v, AdditiveGroup (Scalar v), Fractional (Scalar v))
         => HasOrigin (Bounds v) where
  type OriginSpace (Bounds v) = v

  moveOriginTo (P u) (Bounds f) = Bounds $ \v -> f v ^-^ ((u ^/ (v <.> v)) <.> v)

------------------------------------------------------------
--  Transforming bounding regions  -------------------------
------------------------------------------------------------

-- XXX can we get away with removing this Floating constraint? It's the
--   call to normalized here which is the culprit.
instance ( HasLinearMap v, InnerSpace v
         , Scalar v ~ s, Floating s, AdditiveGroup s )
    => Transformable (Bounds v) where
  type TSpace (Bounds v) = v
  transform t (Bounds b) =   -- XXX add lots of comments explaining this!
    moveOriginTo (P . negateV . transl $ t) $
    Bounds $ \v ->
      let v' = normalized $ lapp (transp t) v
          vi = apply (inv t) v
      in  b v' / (v' <.> vi)

------------------------------------------------------------
--  Boundable class
------------------------------------------------------------

-- | We often want scalars to be an ordered field (i.e. support
--   addition, subtraction, multiplication, and division, and a total
--   ordering) so we introduce this class as a convenient shorthand.
class (Fractional s, Floating s, Ord s, AdditiveGroup s) => OrderedField s
instance (Fractional s, Floating s, Ord s, AdditiveGroup s) => OrderedField s

-- | @Boundable@ abstracts over things which can be bounded.
class ( InnerSpace (BoundSpace b)
      , OrderedField (Scalar (BoundSpace b))
      ) => Boundable b where
  -- | The vector space in which this boundable thing lives.
  type BoundSpace b :: *

  -- | Given a boundable object, compute a functional bounding region
  --   for it.  For types with an intrinsic notion of \"local
  --   origin\", the bounding function will be based there.  Other
  --   types (e.g. 'Trail') may have some other default reference
  --   point at which the bounding function will be based; their
  --   instances should document what it is.
  bounds :: b -> Bounds (BoundSpace b)

instance (InnerSpace v, OrderedField (Scalar v)) => Boundable (Bounds v) where
  type BoundSpace (Bounds v) = v

  bounds = id

------------------------------------------------------------
--  Computing with bounds
------------------------------------------------------------

-- | Compute the diameter of a boundable object along a particular
--   vector.
diameter :: (Boundable a, v ~ BoundSpace a)
         => v -> a -> Scalar v
diameter v a = f v ^+^ f (negateV v)
  where f = getBoundFunc (bounds a)

-- | Compute the radius (1\/2 the diameter) of a boundable object
--   along a particular vector.
radius :: (Boundable a, v ~ BoundSpace a)
       => v -> a -> Scalar v
radius v a = 0.5 * diameter v a