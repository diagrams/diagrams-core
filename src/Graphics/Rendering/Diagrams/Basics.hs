{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , UndecidableInstances
           , GADTs
           , DeriveFunctor
           , ExistentialQuantification
           , ScopedTypeVariables
           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Basics
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- The core library of primitives forming the basis of an embedded
-- domain-specific language for describing and rendering diagrams.
--
-- Note that end users should rarely (if ever) need to import this
-- module directly; instead, import "Graphics.Rendering.Diagrams",
-- which re-exports most of the functionality from this module.
-- Library developers may occasionally wish to import this module
-- directly if they need access to something not re-exported by
-- "Graphics.Rendering.Diagrams".
--
-----------------------------------------------------------------------------

-- TODO: this module probably wants breaking up into smaller modules...

module Graphics.Rendering.Diagrams.Basics
       ( -- * Backends

         Backend(..)
       , MultiBackend(..)
       , Renderable(..)

         -- * Attributes

       , AttributeClass(..)
       , Attribute(..)
       , unwrapAttr

       , applyAttr

         -- * Styles

       , Style(..), inStyle
       , getAttr, setAttr, addAttr

       , attrToStyle
       , applyStyle

         -- * Primtives

       , Prim(..), prim

         -- * Bounds

       , Bounds(..)

         -- ** Bounds transformation

       , OrientedSubspace(..)
       , orthogonalSpace, orthogonalVec
       , proj
       , vecToList, listToVec

         -- * Diagrams

       , Diagram(..)

         -- ** Primitive operations
         -- $prim
       , rebase
       , atop

       , rebaseBounds
       ) where

import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Expressions

import Data.Typeable

import qualified Numeric.LinearAlgebra.Algorithms as L
import qualified Data.Packed.Vector as LV
import qualified Data.Packed.Matrix as LM

import Data.VectorSpace
import Data.Basis
import Data.MemoTrie

import qualified Data.Map as M
import Data.Monoid
import Control.Arrow (first, second)
import Control.Applicative hiding (Const)
import Data.List (sortBy)
import Data.Ord (comparing)

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- XXX TODO: add lots of actual diagrams to illustrate the
-- documentation!  Haddock supports \<\<inline image urls\>\>.

------------------------------------------------------------
-- Backends  -----------------------------------------------
------------------------------------------------------------

-- | Abstract diagrams are rendered to particular formats by
--   /backends/.  Each backend must be an instance of the 'Backend'
--   class, and comes with an associated vector space and rendering
--   environment.  A backend must provide the four associated types as
--   well as implementations for 'withStyle' and 'doRender'.
class ( HasLinearMap (BSpace b), HasLinearMap (Scalar (BSpace b))
      , Monoid (Render b) )
    => Backend b where
  type BSpace b  :: *           -- The vector space associated with this backend
  type Render b  :: *           -- The type of rendering operations used by this
                                --   backend, which must be a monoid
  type Result b  :: *           -- The result of the rendering operation
  data Options b :: *           -- The rendering options for this backend

  -- | Perform a rendering operation with a local style.
  withStyle      :: b          -- ^ Backend token (needed only for type inference)
                 -> Style      -- ^ Style to use
                 -> Render b   -- ^ Rendering operation to run
                 -> Render b   -- ^ Rendering operation using the style locally

  -- | 'doRender' is used to interpret rendering operations.
  doRender       :: b          -- ^ Backend token (needed only for type inference)
                 -> Options b  -- ^ Backend-specific collection of rendering options
                 -> Render b   -- ^ Rendering operation to perform
                 -> Result b   -- ^ Output of the rendering operation

  -- | Render a diagram.  This has a default implementation in terms
  --   of 'withStyle', 'doRender', and the 'render' operation from the
  --   'Renderable' class ('withStyle' and 'render' are used to render
  --   each primitive, the resulting operations are combined with
  --   'mconcat', and the final operation run with 'doRender') but
  --   backends may override it if desired.
  renderDia :: b -> Options b -> Diagram b -> Result b
  renderDia b opts d = doRender b opts (mconcat $ map renderOne (prims d))
    where renderOne (s,p) = withStyle b s (render b p)

-- Note: as of version 2.8.1, Haddock doesn't seem to support
-- documentation for associated types; hence the comments next to
-- BSpace, Render, etc. above are not Haddock comments.  Making them
-- Haddock comments by adding a carat causes Haddock to choke with a
-- parse error.  Hopefully at some point in the future Haddock will
-- support this, at which time this comment can be deleted and the
-- above comments made into proper Haddock comments.

-- | A class for backends which support rendering multiple diagrams,
--   e.g. to a multi-page pdf or something similar.
class Backend b => MultiBackend b where

  -- | Render multiple diagrams at once.
  renderDias :: b -> Options b -> [Diagram b] -> Result b


-- | The 'Renderable' type class connects backends to primitives which
--   they know how to render.
class (Backend b, Transformable t) => Renderable t b where
  render :: b -> t -> Render b
  -- ^ Given a token representing the backend, a style, and a
  -- transformable object, render it in the appropriate rendering
  -- context.

  -- Note, the token is necessary for type inference: unifying Render
  -- b with something else will never work, since Render is not
  -- necessarily injective.

------------------------------------------------------------
--  Attributes  --------------------------------------------
------------------------------------------------------------

-- The attribute code is inspired by xmonad's Message type, which
-- was in turn based on ideas in /An Extensible Dynamically-Typed
-- Hierarchy of Exceptions/, Simon Marlow, 2006.

-- | Every attribute must be an instance of @AttributeClass@.  The
--   'combine' method indicates how to combine two attributes of the
--   same type; by default the first one is ignored (since child
--   attributes should override parent attributes).  However for some
--   attributes (e.g. for transparent colors) it may make sense to
--   have some more interesting combining behavior.
class Typeable a => AttributeClass a where
  combine :: a -> a -> a
  combine _ a = a

-- TODO: actually use combine below!

-- | An existential wrapper type to hold attributes.
data Attribute = forall a. AttributeClass a => Attribute a

-- | Unwrap an unknown 'Attribute' type, performing a (dynamic)
--   type check on the result.
unwrapAttr :: AttributeClass a => Attribute -> Maybe a
unwrapAttr (Attribute a) = cast a

------------------------------------------------------------
--  Styles  ------------------------------------------------
------------------------------------------------------------

-- | A @Style@ is a collection of attributes, containing at most one
--   attribute of any given type.
newtype Style = Style (M.Map String Attribute)
  -- The String keys are serialized TypeRep values, corresponding to
  -- the type of the stored attribute.

inStyle :: (M.Map String Attribute -> M.Map String Attribute)
        -> Style -> Style
inStyle f (Style s) = Style (f s)

-- | Extract an attribute from a style using the magic of type
--   inference.
getAttr :: forall a. AttributeClass a => Style -> Maybe a
getAttr (Style s) = M.lookup ty s >>= unwrapAttr
  where ty = (show . typeOf $ (undefined :: a))
  -- the unwrapAttr should never fail, since we maintain the invariant
  -- that attributes of type T are always stored with the key "T".

-- | Add a new attribute to a style, or replace the old attribute of
--   the same type if one exists.
setAttr :: forall a. AttributeClass a => a -> Style -> Style
setAttr a = inStyle $ M.insert (show . typeOf $ (undefined :: a)) (Attribute a)

-- | The empty style contains no attributes; composition of styles is
--   right-biased union; i.e. if the two styles contain attributes of
--   the same type, the one from the right is taken.
instance Monoid Style where
  mempty = Style M.empty
  (Style s1) `mappend` (Style s2) = Style $ s2 `M.union` s1

-- | Create a style from a single attribute.
attrToStyle :: forall a. AttributeClass a => a -> Style
attrToStyle a = Style (M.singleton (show . typeOf $ (undefined :: a)) (Attribute a))

-- | Attempt to add a new attribute to a style, but if an attribute of
--   the same type already exists, do not replace it.
addAttr :: forall a. AttributeClass a => a -> Style -> Style
addAttr a s = attrToStyle a <> s

-- | Apply an attribute to a diagram.  Note that child attributes
--   always have precedence over parent attributes.
applyAttr :: AttributeClass a => a -> Diagram b -> Diagram b
applyAttr = applyStyle . attrToStyle

-- | Apply a style to a diagram.
applyStyle :: Style -> Diagram b -> Diagram b
applyStyle s d = d { prims = (map . first) (s<>) (prims d) }

------------------------------------------------------------
--  Primitives  --------------------------------------------
------------------------------------------------------------

-- | A value of type @Prim b@ is an opaque (existentially quantified)
--   primitive which backend @b@ knows how to render.
data Prim b where
  Prim :: (BSpace b ~ TSpace t, Renderable t b) => t -> Prim b

-- | Convenience function for constructing a singleton list of
--   primitives with empty style.
prim :: (BSpace b ~ TSpace t, Renderable t b) => t -> [(Style, Prim b)]
prim p = [(mempty, Prim p)]

-- Note that we also require the vector spaces for the backend and the
-- primitive to be the same; this is necessary since the rendering
-- backend may need to lapp some transformations to the primitive
-- after unpacking the existential package.

-- | The 'Transformable' instance for 'Prim' just pushes calls to
--   'transform' down through the 'Prim' constructor.
instance Backend b => Transformable (Prim b) where
  type TSpace (Prim b) = BSpace b
  transform v (Prim p) = Prim (transform v p)

-- | The 'Renderable' instance for 'Prim' just pushes calls to
--   'render' down through the 'Prim' constructor.
instance Backend b => Renderable (Prim b) b where
  render b (Prim p) = render b p

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
newtype Bounds v = Bounds (v -> Scalar v)

-- | Bounding functions form a monoid, with the constantly zero
--   function (/i.e./ the empty region) as the identity, and pointwise
--   maximum as composition.  Hence, if @b1@ is the bounding function
--   for diagram @d1@, and @b2@ is the bounding function for @d2@,
--   then @b1 \`mappend\` b2@ is the bounding function for @d1
--   \`atop\` d2@.
instance (Ord (Scalar v), AdditiveGroup (Scalar v)) => Monoid (Bounds v) where
  mempty = Bounds $ const zeroV
  mappend (Bounds b1) (Bounds b2) = Bounds $ max <$> b1 <*> b2

------------------------------------------------------------
--  Transforming bounding regions  -------------------------
------------------------------------------------------------

-- | The type of (n-1)-dimensional oriented subspaces.  An
-- \"oriented\" space consists of a set of basis vectors, together
-- with another vector indicating the orientation.
data OrientedSubspace v = OS { osBasis       :: [v]
                             , osOrientation :: v
                             }
  deriving Functor

-- map ortho basis generates n vectors spanning an
-- (n-1) dimensional space; dropping one gives us
-- a basis.  To help keep numerical error to a
-- minimum, drop the one with the smallest
-- magnitude.

-- | From a vector @v@, generate a set of vectors that span the
--   subspace orthogonal to @v@.
orthogonalSpace :: (InnerSpace v, HasBasis v, Floating (Scalar v), Ord (Scalar v)) => v -> OrientedSubspace v
orthogonalSpace v = OS { osBasis       = ssBasis
                       , osOrientation = v
                       }
  where ssBasis = tail . sortBy (comparing magnitude) . map ortho $ basis
        basis   = map (basisValue . fst) (decompose v)
        ortho b = b ^-^ prj b
        prj     = proj v

-- | From an oriented subspace, generate a vector which is orthogonal
--   to the subspace, with the same orientation (i.e., in the same
--   half-space as the orientation vector).
orthogonalVec :: ( InnerSpace v, HasBasis v
                 , s ~ Scalar v
                 , L.Field s, Ord s, Num s )
                   => OrientedSubspace v -> v
orthogonalVec (OS vs o) = v'
  where bs = map fst . decompose . head $ vs
        v = listToVec bs
          . LV.toList . L.nullVector . LM.fromLists
          . map vecToList
          $ vs
        v' = if (v <.> o < 0) then negateV v else v

-- | @proj v u@ computes the projection of @u@ onto @v@.
proj :: (InnerSpace v, Floating (Scalar v)) => v -> v -> v
proj v u = (u <.> v') *^ v'
  where v' = normalized v

instance ( Transformable v, HasLinearMap v, HasLinearMap (Scalar v)
         , InnerSpace v, Floating (Scalar v), L.Field (Scalar v), Ord (Scalar v))
    => Transformable (Bounds v) where
  type TSpace (Bounds v) = TSpace v
  transform t (Bounds b) =   -- XXX add lots of comments explaining this!
    Bounds $ \v ->
      let v' = orthogonalVec (fmap (transform $ inv t) (orthogonalSpace v))
          vi = transform (inv t) v
      in  b v' / (v' <.> vi) * magnitudeSq v'

vecToList :: HasBasis v => v -> [Scalar v]
vecToList = map snd . decompose

listToVec :: HasBasis v => [Basis v] -> [Scalar v] -> v
listToVec bs ss = recompose $ zip bs ss

------------------------------------------------------------
--  Diagrams  ----------------------------------------------
------------------------------------------------------------

-- TODO: At some point, we may want to abstract this into a type
-- class...

-- | The basic 'Diagram' data type.  A diagram consists of a list of
--   primitives paired with styles, a functional convex bounding
--   region, and a set of named (local) points.
data Diagram b = Diagram { prims  :: [(Style, Prim b)]
                         , bounds :: Bounds (BSpace b)
                         , names  :: NameSet (BSpace b)
                         }

------------------------------------------------------------
--  Primitive operations  ----------------------------------
------------------------------------------------------------

-- XXX rebase ought to be implemented in terms of transform?

-- TODO: does rebase really need to involve expressions?  Can't those
-- go in the standard library?

-- | @'rebase' u d@ is the same as @d@, except with the local origin
--   moved to @u@.
rebase :: ( Backend b, v ~ BSpace b
          , InnerSpace v, HasLinearMap v, HasLinearMap (Scalar v)
          , AdditiveGroup (Scalar v), Fractional (Scalar v)
          , Scalar (Scalar v) ~ Scalar v )
       => LExpr v -> Diagram b -> Diagram b
rebase e (Diagram ps b (NameSet s))
  = Diagram { prims  = (map . second) (translate (negateV u)) ps
            , bounds = rebaseBounds u b
            , names  = NameSet $ M.map (map (^-^ u)) s
            }
  where u  = evalLExpr e (NameSet s)

-- | Rebase a bounding function, that is, change the local origin with
--   respect to which bounding queries are made.
rebaseBounds :: (InnerSpace v, AdditiveGroup (Scalar v), Fractional (Scalar v))
             => v -> Bounds v -> Bounds v
rebaseBounds u (Bounds f) = Bounds $ \v -> f v ^-^ ((u ^/ (v <.> v)) <.> v)

-- | 'Diagram's can be transformed by transforming each of their
--   components appropriately.
instance ( Backend b
         , InnerSpace (BSpace b)
         , TSpace (BSpace b) ~ BSpace b
         , Transformable (BSpace b)
         , s ~ Scalar (BSpace b)
         , Scalar s ~ s
         , Floating s
         , L.Field s
         , Ord s
         , HasLinearMap s )
    => Transformable (Diagram b) where
  type TSpace (Diagram b) = BSpace b
  transform t (Diagram ps b ns)
    = Diagram ((map . second) (transform t) ps)
              (transform t b)
              (transform t ns)

-- | Compose two diagrams by aligning their respective local origins.
--   The new diagram has all the primitives and all the names from the
--   two diagrams combined.  Put the first on top of the second (when
--   such a notion makes sense in the digrams' vector space, such as
--   R2; in other vector spaces, like R3, 'atop' is commutative).
atop :: (s ~ Scalar (BSpace b), Ord s, AdditiveGroup s)
     => Diagram b -> Diagram b -> Diagram b
atop (Diagram ps1 bs1 ns1) (Diagram ps2 bs2 ns2) =
  Diagram (ps1 <> ps2) (bs1 <> bs2) (ns1 <> ns2)

-- | Diagrams form a monoid since each of their three components do:
--   the empty diagram has no primitives, a constantly zero bounding
--   function, and no named points; diagrams compose via 'atop'.
instance (s ~ Scalar (BSpace b), Ord s, AdditiveGroup s) => Monoid (Diagram b) where
  mempty  = Diagram mempty mempty mempty
  mappend = atop

