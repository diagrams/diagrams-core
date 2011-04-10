{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , GADTs
           , ExistentialQuantification
           , ScopedTypeVariables
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams.Core
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

module Graphics.Rendering.Diagrams.Core
       (
         -- * Backends

         Backend(..)
       , MultiBackend(..)
       , Renderable(..)

         -- * Attributes

       , AttributeClass(..)
       , Attribute(..)
       , mkAttr, unwrapAttr
       , applyAttr

         -- * Styles

       , Style(..), inStyle
       , getAttr, setAttr, addAttr

       , attrToStyle
       , applyStyle

         -- * Primtives

       , Prim(..)

         -- * Diagrams

       , AnnDiagram(..), mkAD, Diagram
       , prims
       , bounds, names, annot, sample
       , setBounds

         -- XXX export other stuff

         -- ** Primitive operations
         -- $prim
       , atop

       , freeze

       ) where

import Graphics.Rendering.Diagrams.UDTree
import Graphics.Rendering.Diagrams.SplitMonoid
import Graphics.Rendering.Diagrams.V
import Graphics.Rendering.Diagrams.Annot
import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Bounds
import Graphics.Rendering.Diagrams.HasOrigin
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.Names
import Graphics.Rendering.Diagrams.Util

import Data.Typeable

import Data.VectorSpace
import Data.AffineSpace ((.-.))

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
--   /backends/.  Each backend/vector space combination must be an
--   instance of the 'Backend' class. A minimal complete definition
--   consists of the three associated types, along with /either/
--
--   * implementations for 'withStyle' and 'doRender', /or/
--
--   * an implementation of 'renderDia'.
--
class (HasLinearMap v, Monoid (Render b v)) => Backend b v where
  data Render  b v :: *         -- The type of rendering operations used by this
                                --   backend, which must be a monoid
  type Result  b v :: *         -- The result of the rendering operation
  data Options b v :: *         -- The rendering options for this backend
  -- See Note [Haddock and associated types]

  -- | Perform a rendering operation with a local style.
  withStyle      :: b          -- ^ Backend token (needed only for type inference)
                 -> Style      -- ^ Style to use
                 -> Transformation v  -- ^ Transformation to be applied to the style
                 -> Render b v -- ^ Rendering operation to run
                 -> Render b v -- ^ Rendering operation using the style locally

  -- | 'doRender' is used to interpret rendering operations.
  doRender       :: b           -- ^ Backend token (needed only for type inference)
                 -> Options b v -- ^ Backend-specific collection of rendering options
                 -> Render b v  -- ^ Rendering operation to perform
                 -> Result b v  -- ^ Output of the rendering operation

  -- | 'adjustDia' allows the backend to make adjustments to the final
  --   diagram (e.g. to adjust the size based on the options) before
  --   rendering it.  A default implementation is provided which makes
  --   no adjustments.
  adjustDia :: Monoid m => b -> Options b v -> AnnDiagram b v m -> AnnDiagram b v m
  adjustDia _ _ d = d

  -- XXX expand this comment.  Explain about freeze, split
  -- transformations, etc.
  -- | Render a diagram.  This has a default implementation in terms
  --   of 'adjustDia', 'withStyle', 'doRender', and the 'render'
  --   operation from the 'Renderable' class (first 'adjustDia' is
  --   used, then 'withStyle' and 'render' are used to render each
  --   primitive, the resulting operations are combined with
  --   'mconcat', and the final operation run with 'doRender') but
  --   backends may override it if desired.
  renderDia :: Monoid m => b -> Options b v -> AnnDiagram b v m -> Result b v
  renderDia b opts =
    doRender b opts . mconcat . map renderOne . prims . adjustDia b opts
      where renderOne :: (Prim b v, (Split (Transformation v), Style))
                      -> Render b v
            renderOne (p, (M t,      s))
              = withStyle b s mempty (render b (transform t p))

            renderOne (p, (t1 :| t2, s))
              = withStyle b s t1 (render b (transform (t1 <> t2) p))

  -- See Note [backend token]

{-
~~~~ Note [Haddock and associated types]

As of version 2.8.1, Haddock doesn't seem to support
documentation for associated types; hence the comments next to
Render, etc. above are not Haddock comments.  Making them
Haddock comments by adding a carat causes Haddock to choke with a
parse error.  Hopefully at some point in the future Haddock will
support this, at which time this comment can be deleted and the
above comments made into proper Haddock comments.
-}

-- | A class for backends which support rendering multiple diagrams,
--   e.g. to a multi-page pdf or something similar.
class Backend b v => MultiBackend b v where

  -- | Render multiple diagrams at once.
  renderDias :: b -> Options b v -> [AnnDiagram b v m] -> Result b v

  -- See Note [backend token]


-- | The Renderable type class connects backends to primitives which
--   they know how to render.
class Transformable t => Renderable t b where
  render :: b -> t -> Render b (V t)
  -- ^ Given a token representing the backend and a
  --   transformable object, render it in the appropriate rendering
  --   context.

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
data Attribute :: * where
  Attribute :: AttributeClass a => a -> Attribute

-- TODO: add to export lists etc.

-- | Create an attribute.
mkAttr :: AttributeClass a => a -> Attribute
mkAttr = Attribute

-- | Unwrap an unknown 'Attribute' type, performing a (dynamic) type
--   check on the result.
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
--   inference and "Data.Typeable".
getAttr :: forall a. AttributeClass a => Style -> Maybe a
getAttr (Style s) = M.lookup ty s >>= unwrapAttr
  where ty = (show . typeOf $ (undefined :: a))
  -- the unwrapAttr should never fail, since we maintain the invariant
  -- that attributes of type T are always stored with the key "T".

-- | Add a new attribute to a style, or replace the old attribute of
--   the same type if one exists.
setAttr :: forall a. AttributeClass a => a -> Style -> Style
setAttr a = inStyle $ M.insert (show . typeOf $ (undefined :: a)) (mkAttr a)

-- | The empty style contains no attributes; composition of styles is
--   right-biased union; i.e. if the two styles contain attributes of
--   the same type, the one from the right is taken.
instance Monoid Style where
  mempty = Style M.empty
  (Style s1) `mappend` (Style s2) = Style $ s2 `M.union` s1

-- | Create a style from a single attribute.
attrToStyle :: forall a. AttributeClass a => a -> Style
attrToStyle a = Style (M.singleton (show . typeOf $ (undefined :: a)) (mkAttr a))

-- | Attempt to add a new attribute to a style, but if an attribute of
--   the same type already exists, do not replace it.
addAttr :: AttributeClass a => a -> Style -> Style
addAttr a s = attrToStyle a <> s

-- | Apply an attribute to a diagram.  Note that child attributes
--   always have precedence over parent attributes.
applyAttr :: (HasLinearMap v, AttributeClass a)
          => a -> AnnDiagram b v m -> AnnDiagram b v m
applyAttr = applyStyle . attrToStyle

-- | Apply a style to a diagram.
applyStyle :: HasLinearMap v => Style -> AnnDiagram b v m -> AnnDiagram b v m
applyStyle s (AD dia) = AD (applyD (mempty, s) dia)

-- XXX comment me
freeze :: HasLinearMap v => AnnDiagram b v m -> AnnDiagram b v m
freeze (AD dia) = AD (applyD (split, mempty) dia)

------------------------------------------------------------
--  Primitives  --------------------------------------------
------------------------------------------------------------

-- | A value of type @Prim b v@ is an opaque (existentially quantified)
--   primitive which backend @b@ knows how to render in vector space @v@.
data Prim b v where
  Prim :: Renderable t b => t -> Prim b (V t)

type instance V (Prim b v) = v

-- Note that we also require the vector spaces for the backend and the
-- primitive to be the same; this is necessary since the rendering
-- backend may need to lapp some transformations to the primitive
-- after unpacking the existential package.

-- | The 'Transformable' instance for 'Prim' just pushes calls to
--   'transform' down through the 'Prim' constructor.
instance Backend b v => Transformable (Prim b v) where
  transform v (Prim p) = Prim (transform v p)

-- | The 'Renderable' instance for 'Prim' just pushes calls to
--   'render' down through the 'Prim' constructor.
instance Backend b v => Renderable (Prim b v) b where
  render b (Prim p) = render b p

------------------------------------------------------------
--  Diagrams  ----------------------------------------------
------------------------------------------------------------

-- TODO: At some point, we may want to abstract this into a type
-- class...

--
--   TODO: write more here.
--   got idea for annotations from graphics-drawingcombinators.

newtype AnnDiagram b v m = AD { unAD :: UDTree (Bounds v, NameSet v, Annot v m)
                                               (Split (Transformation v), Style)
                                               (Prim b v)
                              }

type instance V (AnnDiagram b v m) = v

-- | The default sort of diagram is one where sampling at a point
--   simply tells you whether that point is occupied or not.
--   Transforming a default diagram into one with more interesting
--   annotations can be done via the 'Functor' and 'Applicative'
--   instances for @'AnnDiagram' b@.
type Diagram b v = AnnDiagram b v Any

-- XXX comment these

prims :: HasLinearMap v
      => AnnDiagram b v m -> [(Prim b v, (Split (Transformation v), Style))]
prims = flatten . unAD

bounds :: AnnDiagram b v m -> Bounds v
bounds = (\(b,_,_) -> b) . getU . unAD

names :: AnnDiagram b v m -> NameSet v
names = (\(_,n,_) -> n) . getU . unAD

annot :: AnnDiagram b v m -> Annot v m
annot = (\(_,_,a) -> a) . getU . unAD

sample :: AnnDiagram b v m -> Point v -> m
sample = queryAnnot . annot

alterAD :: (Bounds v   -> Bounds v)
        -> (NameSet v  -> NameSet v)
        -> (Annot v m1 -> Annot v m2)
        -> AnnDiagram b v m1 -> AnnDiagram b v m2
alterAD f g h (AD dia) = AD $ mapU (\(b,n,a) -> (f b, g n, h a)) dia

mkAD :: Prim b v -> Bounds v -> NameSet v -> Annot v m -> AnnDiagram b v m
mkAD p b n a = AD $ leaf (b,n,a) p

setBounds :: Bounds v -> AnnDiagram b v m -> AnnDiagram b v m
setBounds b = alterAD (const b) id id

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
deriving instance (HasLinearMap v, Ord (Scalar v), AdditiveGroup (Scalar v), Monoid m)
  => Monoid (AnnDiagram b v m)

-- | A convenient synonym for 'mappend' on diagrams (to help remember
--   which diagram goes on top of which when combining them).
atop :: (Backend b v, s ~ Scalar v, Ord s, AdditiveGroup s, Monoid m)
     => AnnDiagram b v m -> AnnDiagram b v m -> AnnDiagram b v m
atop = mappend

---- Functor

instance Functor (AnnDiagram b v) where
  fmap f = alterAD id id (fmap f)

---- Applicative

-- XXX what to do with this?
-- | A diagram with annotations of type @(a -> b)@ can be \"applied\"
--   to a diagram with annotations of type @a@, resulting in a
--   combined diagram with annotations of type @b@.  In particular,
--   all components of the two diagrams are combined as in the
--   @Monoid@ instance, except the annotations which are combined via
--   @(<*>)@.

-- instance (Backend b v, s ~ Scalar v, AdditiveGroup s, Ord s)
--            => Applicative (AnnDiagram b v) where
--   pure a = Diagram mempty mempty mempty (Annot $ const a)

--   (Diagram ps1 bs1 ns1 smp1) <*> (Diagram ps2 bs2 ns2 smp2)
--     = Diagram (ps1 <> ps2) (bs1 <> bs2) (ns1 <> ns2) (smp1 <*> smp2)

---- Boundable

instance (Backend b v, InnerSpace v, OrderedField (Scalar v) )
         => Boundable (AnnDiagram b v m) where
  getBounds = bounds

---- HasOrigin

-- | Every diagram has an intrinsic \"local origin\" which is the
--   basis for all combining operations.
instance (Backend b v, InnerSpace v, OrderedField (Scalar v), Monoid m)
      => HasOrigin (AnnDiagram b v m) where

  moveOriginTo p = translate (origin .-. p)

-- ( Backend b v, s ~ Scalar v
--          , InnerSpace v, HasLinearMap v
--          , Fractional s, AdditiveGroup s
--          )
--        =>

---- Transformable

-- | 'Diagram's can be transformed by transforming each of their
--   components appropriately.
instance ( Backend b v, InnerSpace v
         , OrderedField (Scalar v)
         , Monoid m
         )
    => Transformable (AnnDiagram b v m) where

  transform t (AD dia) = alterAD (transform t)
                                 (transform t)
                                 (transform t)
                       . AD . applyD (M t, mempty)
                       $ dia
