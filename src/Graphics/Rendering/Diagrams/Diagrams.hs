{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , UndecidableInstances
           , GADTs
           , DeriveFunctor
           , ExistentialQuantification
           , ScopedTypeVariables
           , TupleSections
           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Diagrams
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
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

{- ~~~~ Note [breaking up basics module]

   This module is getting somewhat monolithic, and breaking it up into
   a collection of smaller modules would be a good idea in principle.
   However, it's not as easy as it sounds: most of the stuff here depends
   on other stuff in various cyclic ways.
-}

module Graphics.Rendering.Diagrams.Diagrams
       ( -- * Backends

         Backend(..)
       , MultiBackend(..)
       , Renderable(..)

         -- * Attributes

       , AttributeClass(..)
       , Attribute(..)
       , mkAttr, unwrapAttr, attrTransformation
       , freezeAttr, thawAttr

       , applyAttr

         -- * Styles

       , Style(..), inStyle
       , getAttr, setAttr, addAttr

       , attrToStyle
       , applyStyle

       , freezeStyle, thawStyle

         -- * Primtives

       , Prim(..), prim

         -- * Diagrams

       , AnnDiagram(..), Diagram

         -- ** Primitive operations
         -- $prim
       , atop

       , freeze, thaw

       ) where

import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Bounds
import Graphics.Rendering.Diagrams.HasOrigin
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.Names
import Graphics.Rendering.Diagrams.Util

import Data.Typeable

import Data.VectorSpace

import qualified Data.Map as M
import Data.Monoid
import Control.Arrow (first, (***))
import Control.Applicative

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
class ( HasLinearMap (BSpace b), Monoid (Render b) )
    => Backend b where
  type BSpace b  :: *           -- The vector space associated with this backend
  type Render b  :: *           -- The type of rendering operations used by this
                                --   backend, which must be a monoid
  type Result b  :: *           -- The result of the rendering operation
  data Options b :: *           -- The rendering options for this backend
  -- See Note [Haddock and associated types]

  -- | Perform a rendering operation with a local style.
  withStyle      :: b          -- ^ Backend token (needed only for type inference)
                 -> Style (BSpace b)  -- ^ Style to use
                 -> Render b   -- ^ Rendering operation to run
                 -> Render b   -- ^ Rendering operation using the style locally

  -- | 'doRender' is used to interpret rendering operations.
  doRender       :: b          -- ^ Backend token (needed only for type inference)
                 -> Options b  -- ^ Backend-specific collection of rendering options
                 -> Render b   -- ^ Rendering operation to perform
                 -> Result b   -- ^ Output of the rendering operation

  -- | 'adjustDia' allows the backend to make adjustments to the final
  --   diagram (e.g. to adjust the size based on the options) before
  --   rendering it.  A default implementation is provided which makes
  --   no adjustments.
  adjustDia :: b -> Options b -> AnnDiagram b m -> AnnDiagram b m
  adjustDia _ _ d = d

  -- | Render a diagram.  This has a default implementation in terms
  --   of 'adjustDia', 'withStyle', 'doRender', and the 'render'
  --   operation from the 'Renderable' class (first 'adjustDia' is
  --   used, then 'withStyle' and 'render' are used to render each
  --   primitive, the resulting operations are combined with
  --   'mconcat', and the final operation run with 'doRender') but
  --   backends may override it if desired.
  renderDia :: b -> Options b -> AnnDiagram b m -> Result b
  renderDia b opts =
    doRender b opts . mconcat . map renderOne . prims . adjustDia b opts
      where renderOne (s,p) = withStyle b s (render b p)

  -- See Note [backend token]

{-
~~~~ Note [Haddock and associated types]

As of version 2.8.1, Haddock doesn't seem to support
documentation for associated types; hence the comments next to
BSpace, Render, etc. above are not Haddock comments.  Making them
Haddock comments by adding a carat causes Haddock to choke with a
parse error.  Hopefully at some point in the future Haddock will
support this, at which time this comment can be deleted and the
above comments made into proper Haddock comments.
-}

-- | A class for backends which support rendering multiple diagrams,
--   e.g. to a multi-page pdf or something similar.
class Backend b => MultiBackend b where

  -- | Render multiple diagrams at once.
  renderDias :: b -> Options b -> [AnnDiagram b m] -> Result b

  -- See Note [backend token]


-- | The Renderable type class connects backends to primitives which
--   they know how to render.
class (Backend b, Transformable t) => Renderable t b where
  render :: b -> t -> Render b
  -- ^ Given a token representing the backen and a
  -- transformable object, render it in the appropriate rendering
  -- context.

  -- See Note [backend token]

{-
~~~~ Note [backend token]

A bunch of methods here take a "backend token" as an argument.  The
backend token is expected to carry no actual information; it is solely
to help out the type system. The problem is that all these methods
return some associated type applied to b (e.g. Render b) and unifying
them with something else will never work, since type families are not
necessarily injective.
-}

------------------------------------------------------------
--  Attributes  --------------------------------------------
------------------------------------------------------------

-- The attribute code is inspired by xmonad's Message type, which
-- was in turn based on ideas in /An Extensible Dynamically-Typed
-- Hierarchy of Exceptions/, Simon Marlow, 2006.

-- | Every attribute must be an instance of @AttributeClass@.
class Typeable a => AttributeClass a where

-- | An existential wrapper type to hold attributes.
data Attribute :: * -> * where
  Attribute :: AttributeClass a => a -> Bool -> Transformation v -> Attribute v

-- | Attributes can be transformed; it is up to the backend to decide
--   how to treat transformed attributes.
instance HasLinearMap v => Transformable (Attribute v) where
  type TSpace (Attribute v) = v
  transform t   (Attribute a True x)  = Attribute a True (t <> x)
  transform t a@(Attribute _ False _) = a

-- TODO: add to export lists etc.

-- | Create an (untransformed, unfrozen) attribute.
mkAttr :: ( HasLinearMap v, AttributeClass a)
            => a -> Attribute v
mkAttr a = Attribute a False mempty

-- | Freeze an attribute, i.e. it will now be affected by
--   transformations.
freezeAttr :: Attribute v -> Attribute v
freezeAttr (Attribute a _ t) = Attribute a True t

-- | Thaw an attribute, i.e. it will no longer be affected by
--   transformations.
thawAttr :: Attribute v -> Attribute v
thawAttr (Attribute a _ t) = Attribute a False t

-- | Unwrap an unknown 'Attribute' type, performing a (dynamic) type
--   check on the result, returning both the attribute and its
--   accompanying transformation.
unwrapAttr :: AttributeClass a => Attribute v -> Maybe (a, Transformation v)
unwrapAttr (Attribute a _ t) = (,t) <$> cast a

-- | Get the transformation that has been applied to an attribute.
attrTransformation :: Attribute v -> Transformation v
attrTransformation (Attribute _ _ t) = t

------------------------------------------------------------
--  Styles  ------------------------------------------------
------------------------------------------------------------

-- | A @Style@ is a collection of attributes, containing at most one
--   attribute of any given type.
newtype Style v = Style (M.Map String (Attribute v))
  -- The String keys are serialized TypeRep values, corresponding to
  -- the type of the stored attribute.

inStyle :: (M.Map String (Attribute v) -> M.Map String (Attribute v))
        -> Style v -> Style v
inStyle f (Style s) = Style (f s)

instance HasLinearMap v => Transformable (Style v) where
  type TSpace (Style v) = v
  transform = inStyle . M.map . transform

-- | Extract an attribute and the accompanying transformation from a
--   style using the magic of type inference and "Data.Typeable".
getAttr :: forall a v. (HasLinearMap v, AttributeClass a)
             => Style v -> Maybe (a, Transformation v)
getAttr (Style s) = M.lookup ty s >>= unwrapAttr
  where ty = (show . typeOf $ (undefined :: a))
  -- the unwrapAttr should never fail, since we maintain the invariant
  -- that attributes of type T are always stored with the key "T".

-- | Add a new attribute to a style, or replace the old attribute of
--   the same type if one exists.
setAttr :: forall a v. (HasLinearMap v, AttributeClass a)
             => a -> Style v -> Style v
setAttr a = inStyle $ M.insert (show . typeOf $ (undefined :: a)) (mkAttr a)

-- | The empty style contains no attributes; composition of styles is
--   right-biased union; i.e. if the two styles contain attributes of
--   the same type, the one from the right is taken.
instance Monoid (Style v) where
  mempty = Style M.empty
  (Style s1) `mappend` (Style s2) = Style $ s2 `M.union` s1

-- | Create a style from a single attribute.
attrToStyle :: forall a v. (HasLinearMap v, AttributeClass a)
                 => a -> Style v
attrToStyle a = Style (M.singleton (show . typeOf $ (undefined :: a)) (mkAttr a))

-- | Attempt to add a new attribute to a style, but if an attribute of
--   the same type already exists, do not replace it.
addAttr :: (HasLinearMap v, AttributeClass a)
             => a -> Style v -> Style v
addAttr a s = attrToStyle a <> s

-- | Apply an attribute to a diagram.  Note that child attributes
--   always have precedence over parent attributes.
applyAttr :: (v ~ BSpace b, HasLinearMap v, AttributeClass a)
               => a -> AnnDiagram b m -> AnnDiagram b m
applyAttr = applyStyle . attrToStyle

-- | Apply a style to a diagram.
applyStyle :: Style (BSpace b) -> AnnDiagram b m -> AnnDiagram b m
applyStyle s d = d { prims = (map . first) (s<>) (prims d) }

-- TODO: comment these and add to export list

freezeStyle :: Style v -> Style v
freezeStyle = (inStyle . fmap) freezeAttr

thawStyle :: Style v -> Style v
thawStyle = (inStyle . fmap) thawAttr

freeze :: AnnDiagram b m -> AnnDiagram b m
freeze d = d { prims = (map . first) freezeStyle (prims d) }

thaw :: AnnDiagram b m -> AnnDiagram b m
thaw d = d { prims = (map . first) thawStyle (prims d) }

------------------------------------------------------------
--  Primitives  --------------------------------------------
------------------------------------------------------------

-- | A value of type @Prim b@ is an opaque (existentially quantified)
--   primitive which backend @b@ knows how to render.
data Prim b where
  Prim :: (BSpace b ~ TSpace t, Renderable t b) => t -> Prim b

-- | Convenience function for constructing a singleton list of
--   primitives with empty style.
prim :: (BSpace b ~ TSpace t, Renderable t b) => t -> [(Style (BSpace b), Prim b)]
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
--  Diagrams  ----------------------------------------------
------------------------------------------------------------

-- TODO: At some point, we may want to abstract this into a type
-- class...

-- TODO: Change the prims list into a joinlist?  Would it help to
-- remember more structure?

-- | The basic 'AnnDiagram' data type representing annotated
--   diagrams.  A diagram consists of
--
--     * a list of primitives paired with styles
--
--     * a convex bounding region (represented functionally),
--
--     * a set of named (local) points, and
--
--     * a sampling function that associates an annotation (taken from
--       some monoid) to each point in the vector space.
--
--   TODO: write more here.
--   got idea for annotations from graphics-drawingcombinators.
data AnnDiagram b m = Diagram { prims   :: [(Style (BSpace b), Prim b)]
                              , bounds_ :: Bounds (BSpace b)
                              , names   :: NameSet (BSpace b)
                              , sample  :: Point (BSpace b) -> m
                              }
  deriving (Functor)

-- | The default sort of diagram is one where sampling at a point
--   simply tells you whether that point is occupied or not.
--   Transforming a default diagram into one with more interesting
--   annotations can be done via the 'Functor' and 'Applicative'
--   instances for @'AnnDiagram' b@.
type Diagram b = AnnDiagram b Any

------------------------------------------------------------
--  Instances
------------------------------------------------------------

---- Monoid

-- | Diagrams form a monoid since each of their components do:
--   the empty diagram has no primitives, a constantly zero bounding
--   function, no named points, and a constantly empty sampling function.
--
--   Diagrams compose by aligning their respective local origins.  The
--   new diagram has all the primitives and all the names from the two
--   diagrams combined, and sampling functions are combined pointwise.
--   The first diagram goes on top of the second (when such a notion
--   makes sense in the digrams' vector space, such as R2; in other
--   vector spaces, like R3, @mappend@ is commutative).
instance (s ~ Scalar (BSpace b), Ord s, AdditiveGroup s, Monoid m)
           => Monoid (AnnDiagram b m) where
  mempty  = Diagram mempty mempty mempty mempty
  mappend (Diagram ps1 bs1 ns1 smp1) (Diagram ps2 bs2 ns2 smp2) =
    Diagram (ps1 <> ps2) (bs1 <> bs2) (ns1 <> ns2) (smp1 <> smp2)

-- | A convenient synonym for 'mappend' on diagrams (to help remember
--   which diagram goes on top of which when combining them).
atop :: (s ~ Scalar (BSpace b), Ord s, AdditiveGroup s, Monoid m)
     => AnnDiagram b m -> AnnDiagram b m -> AnnDiagram b m
atop = mappend

---- Applicative

-- | A diagram with annotations of type @(a -> b)@ can be \"applied\"
--   to a diagram with annotations of type @a@, resulting in a
--   combined diagram with annotations of type @b@.  In particular,
--   all components of the two diagrams are combined as in the
--   @Monoid@ instance, except the annotations which are combined via
--   @(<*>)@.
instance (s ~ Scalar (BSpace b), AdditiveGroup s, Ord s)
           => Applicative (AnnDiagram b) where
  pure a = Diagram mempty mempty mempty (const a)

  (Diagram ps1 bs1 ns1 smp1) <*> (Diagram ps2 bs2 ns2 smp2)
    = Diagram (ps1 <> ps2) (bs1 <> bs2) (ns1 <> ns2) (smp1 <*> smp2)

---- Boundable

instance ( InnerSpace (BSpace b)
         , OrderedField (Scalar (BSpace b))
         ) => Boundable (AnnDiagram b m) where
  type BoundSpace (AnnDiagram b m) = BSpace b
  bounds (Diagram {bounds_ = b}) = b

---- HasOrigin

-- | Every diagram has an intrinsic \"local origin\" which is the
--   basis for all combining operations.
instance ( Backend b, v ~ BSpace b, s ~ Scalar v
         , InnerSpace v, HasLinearMap v
         , Fractional s, AdditiveGroup s
         )
       => HasOrigin (AnnDiagram b m) v where

  moveOriginTo p (Diagram ps b (NameSet s) smp)
    = Diagram { prims   = map (tr *** tr) ps
              , bounds_ = moveOriginTo p b
              , names   = NameSet $ M.map (map tr) s
              , sample  = smp . tr
              }
          -- the scoped type variables are necessary here since GHC no longer
          -- generalizes let-bound functions
    where tr :: (Transformable t, TSpace t ~ v) => t -> t
          tr = translate (origin .-. p)

---- Transformable

-- | 'Diagram's can be transformed by transforming each of their
--   components appropriately.
instance ( Backend b, InnerSpace (BSpace b)
         , s ~ Scalar (BSpace b), Floating s, AdditiveGroup s
         )
    => Transformable (AnnDiagram b m) where
  type TSpace (AnnDiagram b m) = BSpace b
  transform t (Diagram ps b ns smp)
    = Diagram (map (transform t *** transform t) ps)
              (transform t b)
              (transform t ns)
              (transform t smp)
