{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , GADTs
           , ExistentialQuantification
           , ScopedTypeVariables
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , TypeOperators
           , OverlappingInstances
           , UndecidableInstances
           , TupleSections
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
-- "Graphics.Rendering.Diagrams.Core" defines types and classes for
-- primitives, diagrams, and backends.
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
         -- * Diagrams
         -- $dia

         -- ** Annotations
         UpAnnots, DownAnnots
       , AnnDiagram(..), mkAD, Diagram

         -- * Operations on diagrams
       , prims
       , bounds, names, query, sample
       , named
       , atop
       , freeze

         -- * Primtives
         -- $prim

       , Prim(..), nullPrim

         -- * Backends

       , Backend(..)
       , MultiBackend(..)

         -- * Renderable

       , Renderable(..)

       ) where

import Graphics.Rendering.Diagrams.Monoids
import Graphics.Rendering.Diagrams.MList
import Graphics.Rendering.Diagrams.UDTree

import Graphics.Rendering.Diagrams.V
import Graphics.Rendering.Diagrams.Query
import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Bounds
import Graphics.Rendering.Diagrams.HasOrigin
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.Names
import Graphics.Rendering.Diagrams.Style
import Graphics.Rendering.Diagrams.Util

import Data.VectorSpace
import Data.AffineSpace ((.-.))

import Data.Monoid
import Control.Arrow (second)

-- XXX TODO: add lots of actual diagrams to illustrate the
-- documentation!  Haddock supports \<\<inline image urls\>\>.

------------------------------------------------------------
--  Diagrams  ----------------------------------------------
------------------------------------------------------------

-- $dia
-- XXX write me

-- | Monoidal annotations which travel up the diagram tree, i.e. which
--   are aggregated from component diagrams to the whole:
--
--   * functional bounding regions (see "Graphics.Rendering.Diagrams.Bounds")
--
--   * name/point associations (see "Graphics.Rendering.Diagrams.Names")
--
--   * query functions (see "Graphics.Rendering.Diagrams.Query")
type UpAnnots v m = Bounds v ::: NameMap v ::: Query v m ::: Nil

-- | Monoidal annotations which travel down the diagram tree,
--   i.e. which accumulate along each path to a leaf (and which can
--   act on the upwards-travelling annotations):
--
--   * transformations (split at the innermost freeze): see
--     "Graphics.Rendering.Diagrams.Transform"
--
--   * styles (see "Graphics.Rendering.Diagrams.Style")
--
--   * names (see "Graphics.Rendering.Diagrams.Names")
type DownAnnots v = Split (Transformation v) ::: Style ::: AM [] Name ::: Nil

newtype AnnDiagram b v m
  = AD { unAD :: UDTree (UpAnnots v m) (DownAnnots v) (Prim b v) }

inAD :: (UDTree (UpAnnots v m) (DownAnnots v) (Prim b v)
         -> UDTree (UpAnnots v' m') (DownAnnots v') (Prim b' v'))
     -> (AnnDiagram b v m -> AnnDiagram b' v' m')
inAD f = AD . f . unAD

type instance V (AnnDiagram b v m) = v

-- | The default sort of diagram is one where sampling at a point
--   simply tells you whether that point is occupied or not.
--   Transforming a default diagram into one with more interesting
--   annotations can be done via the 'Functor' and 'Applicative'
--   instances for @'AnnDiagram' b@.
type Diagram b v = AnnDiagram b v Any

-- XXX comment these

prims :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid m)
      => AnnDiagram b v m -> [(Prim b v, (Split (Transformation v), Style))]
prims = (map . second) (wibble . toTuple) . flatten . unAD
  where wibble (t,(s,_)) = (t,s)

bounds :: (OrderedField (Scalar v), InnerSpace v, HasLinearMap v)
       => AnnDiagram b v m -> Bounds v
bounds = getU' . unAD

names :: HasLinearMap v => AnnDiagram b v m -> NameMap v
names = getU' . unAD

named :: forall v b n m.
         ( IsName n
         , HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid m)
      => n -> AnnDiagram b v m -> AnnDiagram b v m
named = inAD . applyU . inj . fromNames . (:[]) . (,origin :: Point v)

query :: (HasLinearMap v, Monoid m) => AnnDiagram b v m -> Query v m
query = getU' . unAD

sample :: (HasLinearMap v, Monoid m) => AnnDiagram b v m -> Point v -> m
sample = runQuery . query

mkAD :: Prim b v -> Bounds v -> NameMap v -> Query v m -> AnnDiagram b v m
mkAD p b n a = AD $ leaf (b ::: n ::: a ::: Nil) p

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
instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid m)
  => Monoid (AnnDiagram b v m) where
  mempty = AD mempty
  (AD d1) `mappend` (AD d2) = AD (d2 `mappend` d1)
    -- swap order so that primitives of d2 come first, i.e. will be
    -- rendered first, i.e. will be on the bottom.

-- | A convenient synonym for 'mappend' on diagrams (to help remember
--   which diagram goes on top of which when combining them).
atop :: (HasLinearMap v, OrderedField (Scalar v), InnerSpace v, Monoid m)
     => AnnDiagram b v m -> AnnDiagram b v m -> AnnDiagram b v m
atop = mappend

---- Functor

-- This is a bit ugly, but it will have to do for now...
instance Functor (AnnDiagram b v) where
  fmap f = inAD (mapU g)
    where g (b ::: n ::: a ::: Nil) = (b ::: n ::: fmap f a ::: Nil)
          g _ = error "impossible case in Functor (AnnDiagram b v) instance (g)"

---- Applicative

-- XXX what to do with this?
-- A diagram with queries of result type @(a -> b)@ can be \"applied\"
--   to a diagram with queries of result type @a@, resulting in a
--   combined diagram with queries of result type @b@.  In particular,
--   all components of the two diagrams are combined as in the
--   @Monoid@ instance, except the queries which are combined via
--   @(<*>)@.

-- instance (Backend b v, s ~ Scalar v, AdditiveGroup s, Ord s)
--            => Applicative (AnnDiagram b v) where
--   pure a = Diagram mempty mempty mempty (Query $ const a)

--   (Diagram ps1 bs1 ns1 smp1) <*> (Diagram ps2 bs2 ns2 smp2)
--     = Diagram (ps1 <> ps2) (bs1 <> bs2) (ns1 <> ns2) (smp1 <*> smp2)

---- HasStyle

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid m)
      => HasStyle (AnnDiagram b v m) where
  applyStyle = inAD . applyD . inj

-- XXX comment me
freeze :: forall v b m. (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid m)
       => AnnDiagram b v m -> AnnDiagram b v m
freeze = inAD . applyD . inj $ (split :: Split (Transformation v))

---- Boundable

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v) )
         => Boundable (AnnDiagram b v m) where
  getBounds = bounds

---- HasOrigin

-- | Every diagram has an intrinsic \"local origin\" which is the
--   basis for all combining operations.
instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid m)
      => HasOrigin (AnnDiagram b v m) where

  moveOriginTo = translate . (origin .-.)

---- Transformable

-- | 'Diagram's can be transformed by transforming each of their
--   components appropriately.
instance (HasLinearMap v, OrderedField (Scalar v), InnerSpace v, Monoid m)
      => Transformable (AnnDiagram b v m) where
  transform = inAD . applyD . inj . M

---- Qualifiable

-- | 'Diagram's can be qualified so that all their named points can
--   now be referred to using the qualification prefix.
instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid m)
      => Qualifiable (AnnDiagram b v m) where
  (|>) = inAD . applyD . inj . AM . (:[]) . toName


------------------------------------------------------------
--  Primitives  --------------------------------------------
------------------------------------------------------------

-- $prim
-- Ultimately, every diagram is essentially a collection of
-- /primitives/, basic building blocks which can be rendered by
-- backends.  However, not every backend must be able to render every
-- type of primitive; the collection of primitives a given backend
-- knows how to render is determined by instances of 'Renderable'.

-- | A value of type @Prim b v@ is an opaque (existentially quantified)
--   primitive which backend @b@ knows how to render in vector space @v@.
data Prim b v where
  Prim :: Renderable t b => t -> Prim b (V t)

type instance V (Prim b v) = v

-- | The 'Transformable' instance for 'Prim' just pushes calls to
--   'transform' down through the 'Prim' constructor.
instance Backend b v => Transformable (Prim b v) where
  transform v (Prim p) = Prim (transform v p)

-- | The 'Renderable' instance for 'Prim' just pushes calls to
--   'render' down through the 'Prim' constructor.
instance Backend b v => Renderable (Prim b v) b where
  render b (Prim p) = render b p

-- | The null primitive.
data NullPrim v = NullPrim

type instance (V (NullPrim v)) = v

instance HasLinearMap v => Transformable (NullPrim v) where
  transform _ _ = NullPrim

instance (HasLinearMap v, Monoid (Render b v)) => Renderable (NullPrim v) b where
  render _ _ = mempty

-- | The null primitive, which every backend can render by doing
--   nothing.
nullPrim :: (HasLinearMap v, Monoid (Render b v)) => Prim b v
nullPrim = Prim NullPrim



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
  renderDia :: (InnerSpace v, OrderedField (Scalar v), Monoid m)
            => b -> Options b v -> AnnDiagram b v m -> Result b v
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

