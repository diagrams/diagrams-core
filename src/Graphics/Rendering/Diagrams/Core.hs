{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , GADTs
           , ExistentialQuantification
           , ScopedTypeVariables
           , GeneralizedNewtypeDeriving
           , DeriveDataTypeable
           , TypeOperators
           , OverlappingInstances
           , UndecidableInstances
           , TupleSections
           , EmptyDataDecls
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

{- ~~~~ Note [breaking up Core module]

   Although it's not as bad as it used to be, this module has a lot of
   stuff in it, and it might seem a good idea in principle to break it up
   into smaller modules.  However, it's not as easy as it sounds: everything
   in this module cyclically depends on everything else.
-}

module Graphics.Rendering.Diagrams.Core
       (
         -- * Diagrams

         -- ** Annotations
         UpAnnots, DownAnnots
       , QDiagram(..), mkQD, Diagram

         -- * Operations on diagrams
         -- ** Extracting information
       , prims
       , envelope, names, query, sample
       , value, resetValue, clearValue

         -- ** Combining diagrams

         -- | For many more ways of combining diagrams, see
         -- "Diagrams.Combinators" from the diagrams-lib package.

       , atop

         -- ** Modifying diagrams
         -- *** Names
       , named
       , namePoint
       , withName
       , withNameAll
       , withNames

         -- *** Other
       , freeze
       , setEnvelope

         -- * Primtives
         -- $prim

       , Prim(..), nullPrim

         -- * Backends

       , Backend(..)
       , MultiBackend(..)

         -- ** Null backend

       , NullBackend, D

         -- * Renderable

       , Renderable(..)

       ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (first, second)
import           Control.Newtype
import           Data.AffineSpace ((.-.))
import           Data.Maybe (listToMaybe, fromMaybe)
import           Data.Semigroup
import qualified Data.Traversable as T
import           Data.Typeable
import           Data.VectorSpace

import           Graphics.Rendering.Diagrams.Monoids
import           Graphics.Rendering.Diagrams.MList
import           Graphics.Rendering.Diagrams.DUBLTree

import           Graphics.Rendering.Diagrams.V
import           Graphics.Rendering.Diagrams.Query
import           Graphics.Rendering.Diagrams.Transform
import           Graphics.Rendering.Diagrams.Envelope
import           Graphics.Rendering.Diagrams.HasOrigin
import           Graphics.Rendering.Diagrams.Juxtapose
import           Graphics.Rendering.Diagrams.Points
import           Graphics.Rendering.Diagrams.Names
import           Graphics.Rendering.Diagrams.Style

-- XXX TODO: add lots of actual diagrams to illustrate the
-- documentation!  Haddock supports \<\<inline image urls\>\>.

------------------------------------------------------------
--  Diagrams  ----------------------------------------------
------------------------------------------------------------

-- | Monoidal annotations which travel up the diagram tree, i.e. which
--   are aggregated from component diagrams to the whole:
--
--   * envelopes (see "Graphics.Rendering.Diagrams.Envelope").
--     The envelopes are \"deletable\" meaning that at any point we can
--     throw away the existing envelope and replace it with a new one;
--     sometimes we want to consider a diagram as having a different
--     envelope unrelated to its \"natural\" envelope.
--
--   * name/point associations (see "Graphics.Rendering.Diagrams.Names")
--
--   * query functions (see "Graphics.Rendering.Diagrams.Query")
type UpAnnots v m = Deletable (Envelope v) ::: NameMap v ::: Query v m ::: Nil

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
type DownAnnots v = (Split (Transformation v) :+: Style v) ::: Name ::: Nil

-- | The fundamental diagram type is represented by trees of
--   primitives with various monoidal annotations.  The @Q@ in
--   @QDiagram@ stands for \"Queriable\", as distinguished from
--   'Diagram', a synonym for @QDiagram@ with the query type
--   specialized to 'Any'.
newtype QDiagram b v m
  = QD { unQD :: DUBLTree (DownAnnots v) (UpAnnots v m) () (Prim b v) }
  deriving (Typeable)

instance Newtype (QDiagram b v m)
                 (DUBLTree (DownAnnots v) (UpAnnots v m) () (Prim b v)) where
  pack   = QD
  unpack = unQD

type instance V (QDiagram b v m) = v

-- | The default sort of diagram is one where querying at a point
--   simply tells you whether that point is occupied or not.
--   Transforming a default diagram into one with a more interesting
--   query can be done via the 'Functor' instance of @'QDiagram' b@.
type Diagram b v = QDiagram b v Any

fromOption :: a -> Option a -> a
fromOption a (Option Nothing)  = a
fromOption _ (Option (Just a)) = a

-- | Extract a list of primitives from a diagram, together with their
--   associated transformations and styles.
prims :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
      => QDiagram b v m -> [(Prim b v, (Split (Transformation v), Style v))]
prims = (map . second) (untangle . fst . toTuple . fromOption empty) . flatten . unQD

-- | Get the envelope of a diagram.
envelope :: (OrderedField (Scalar v), InnerSpace v, HasLinearMap v)
       => QDiagram b v m -> Envelope v
envelope = unDelete . getU' . unQD

-- | Replace the envelope of a diagram.
setEnvelope :: forall b v m. (OrderedField (Scalar v), InnerSpace v, HasLinearMap v, Monoid' m)
          => Envelope v -> QDiagram b v m -> QDiagram b v m
setEnvelope b = over QD ( applyUpre (inj . toDeletable $ b)
                      . applyUpre (inj (deleteL :: Deletable (Envelope v)))
                      . applyUpost (inj (deleteR :: Deletable (Envelope v)))
                      )

-- | Get the name map of a diagram.
names :: (AdditiveGroup (Scalar v), Floating (Scalar v), InnerSpace v, HasLinearMap v)
       => QDiagram b v m -> NameMap v
names = getU' . unQD

-- | Attach an atomic name to (the local origin of) a diagram.
named :: forall v b n m.
         ( IsName n
         , HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
      => n -> QDiagram b v m -> QDiagram b v m
named = namePoint (locateEnvelope <$> const origin <*> envelope)

-- | Attach an atomic name to a certain point and envelope, computed
--   from the given diagram.
namePoint :: forall v b n m.
         ( IsName n
         , HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
      => (QDiagram b v m -> LocatedEnvelope v) -> n -> QDiagram b v m -> QDiagram b v m
namePoint p n d = over QD (applyUpre . inj $ fromNamesB [(n,p d)]) d

-- | Given a name and a diagram transformation indexed by a located
--   envelope, perform the transformation using the most recent
--   located envelope associated with (some qualification of) the
--   name, or perform the identity transformation if the name does not
--   exist.
withName :: ( IsName n, AdditiveGroup (Scalar v), Floating (Scalar v)
            , InnerSpace v, HasLinearMap v)
         => n -> (LocatedEnvelope v -> QDiagram b v m -> QDiagram b v m)
         -> QDiagram b v m -> QDiagram b v m
withName n f d = maybe id f (lookupN (toName n) (names d) >>= listToMaybe) d

-- | Given a name and a diagram transformation indexed by a list of
--   located envelopes, perform the transformation using the
--   collection of all such located envelopes associated with (some
--   qualification of) the given name.
withNameAll :: ( IsName n, AdditiveGroup (Scalar v), Floating (Scalar v)
               , InnerSpace v, HasLinearMap v)
            => n -> ([LocatedEnvelope v] -> QDiagram b v m -> QDiagram b v m)
            -> QDiagram b v m -> QDiagram b v m
withNameAll n f d = f (fromMaybe [] (lookupN (toName n) (names d))) d

-- | Given a list of names and a diagram transformation indexed by a
--   list of located envelopes, perform the transformation using the
--   list of most recent envelopes associated with (some qualification
--   of) each name.  Do nothing (the identity transformation) if any
--   of the names do not exist.
withNames :: ( IsName n, AdditiveGroup (Scalar v), Floating (Scalar v)
             , InnerSpace v, HasLinearMap v)
          => [n] -> ([LocatedEnvelope v] -> QDiagram b v m -> QDiagram b v m)
          -> QDiagram b v m -> QDiagram b v m
withNames ns f d = maybe id f (T.sequence (map ((listToMaybe=<<) . ($nd) . lookupN . toName) ns)) d
  where nd = names d

-- | Get the query function associated with a diagram.
query :: (HasLinearMap v, Monoid m) => QDiagram b v m -> Query v m
query = getU' . unQD

-- | Sample a diagram's query function at a given point.
sample :: (HasLinearMap v, Monoid m) => QDiagram b v m -> Point v -> m
sample = runQuery . query

-- | Set the query value for 'True' points in a diagram (/i.e./ points
--   "inside" the diagram); 'False' points will be set to 'mempty'.
value :: Monoid m => m -> QDiagram b v Any -> QDiagram b v m
value m = fmap fromAny
  where fromAny (Any True)  = m
        fromAny (Any False) = mempty

-- | Reset the query values of a diagram to True/False: any values
--   equal to 'mempty' are set to 'False'; any other values are set to
--   'True'.
resetValue :: (Eq m, Monoid m) => QDiagram b v m -> QDiagram b v Any
resetValue = fmap toAny
  where toAny m | m == mempty = Any False
                | otherwise   = Any True

-- | Set all the query values of a diagram to 'False'.
clearValue :: QDiagram b v m -> QDiagram b v Any
clearValue = fmap (const (Any False))

-- | Create a diagram from a single primitive, along with an envelope,
--   name map, and query function.
mkQD :: Prim b v -> Envelope v -> NameMap v -> Query v m -> QDiagram b v m
mkQD p b n a = QD $ leaf (toDeletable b ::: n ::: a ::: Nil) p

------------------------------------------------------------
--  Instances
------------------------------------------------------------

---- Monoid

-- | Diagrams form a monoid since each of their components do: the
--   empty diagram has no primitives, an empty envelope, no named
--   points, and a constantly empty query function.
--
--   Diagrams compose by aligning their respective local origins.  The
--   new diagram has all the primitives and all the names from the two
--   diagrams combined, and query functions are combined pointwise.
--   The first diagram goes on top of the second.  \"On top of\"
--   probably only makes sense in vector spaces of dimension lower
--   than 3, but in theory it could make sense for, say, 3-dimensional
--   diagrams when viewed by 4-dimensional beings.
instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
  => Monoid (QDiagram b v m) where
  mempty  = QD Empty
  mappend = (<>)

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
  => Semigroup (QDiagram b v m) where
  (QD d1) <> (QD d2) = QD (d2 <> d1)
    -- swap order so that primitives of d2 come first, i.e. will be
    -- rendered first, i.e. will be on the bottom.

-- | A convenient synonym for 'mappend' on diagrams, designed to be
--   used infix (to help remember which diagram goes on top of which
--   when combining them, namely, the first on top of the second).
atop :: (HasLinearMap v, OrderedField (Scalar v), InnerSpace v, Semigroup m)
     => QDiagram b v m -> QDiagram b v m -> QDiagram b v m
atop = (<>)

infixl 6 `atop`

---- Functor

-- This is a bit ugly, but it will have to do for now...
instance Functor (QDiagram b v) where
  fmap f = over QD (mapU g)
    where g (b ::: n ::: a ::: Nil) = b ::: n ::: fmap f a ::: Nil
          g _ = error "impossible case in Functor (QDiagram b v) instance (g)"

---- Applicative

-- XXX what to do with this?
-- A diagram with queries of result type @(a -> b)@ can be \"applied\"
--   to a diagram with queries of result type @a@, resulting in a
--   combined diagram with queries of result type @b@.  In particular,
--   all components of the two diagrams are combined as in the
--   @Monoid@ instance, except the queries which are combined via
--   @(<*>)@.

-- instance (Backend b v, s ~ Scalar v, AdditiveGroup s, Ord s)
--            => Applicative (QDiagram b v) where
--   pure a = Diagram mempty mempty mempty (Query $ const a)

--   (Diagram ps1 bs1 ns1 smp1) <*> (Diagram ps2 bs2 ns2 smp2)
--     = Diagram (ps1 <> ps2) (bs1 <> bs2) (ns1 <> ns2) (smp1 <*> smp2)

---- HasStyle

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
      => HasStyle (QDiagram b v m) where
  applyStyle = over QD . applyD . inj
             . (inR :: Style v -> Split (Transformation v) :+: Style v)

-- | By default, diagram attributes are not affected by
--   transformations.  This means, for example, that @lw 0.01 circle@
--   and @scale 2 (lw 0.01 circle)@ will be drawn with lines of the
--   /same/ width, and @scaleY 3 circle@ will be an ellipse drawn with
--   a uniform line.  Once a diagram is frozen, however,
--   transformations do affect attributes, so, for example, @scale 2
--   (freeze (lw 0.01 circle))@ will be drawn with a line twice as
--   thick as @lw 0.01 circle@, and @scaleY 3 (freeze circle)@ will be
--   drawn with a \"stretched\", variable-width line.
--
--   Another way of thinking about it is that pre-@freeze@, we are
--   transforming the \"abstract idea\" of a diagram, and the
--   transformed version is then drawn; when doing a @freeze@, we
--   produce a concrete drawing of the diagram, and it is this visual
--   representation itself which is acted upon by subsequent
--   transformations.
freeze :: forall v b m. (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
       => QDiagram b v m -> QDiagram b v m
freeze = over QD . applyD . inj
       . (inL :: Split (Transformation v) -> Split (Transformation v) :+: Style v)
       $ split

---- Juxtaposable

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
      => Juxtaposable (QDiagram b v m) where
  juxtapose = juxtaposeDefault

---- Enveloped

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v) )
         => Enveloped (QDiagram b v m) where
  getEnvelope = envelope

---- HasOrigin

-- | Every diagram has an intrinsic \"local origin\" which is the
--   basis for all combining operations.
instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
      => HasOrigin (QDiagram b v m) where

  moveOriginTo = translate . (origin .-.)

---- Transformable

-- | Diagrams can be transformed by transforming each of their
--   components appropriately.
instance (HasLinearMap v, OrderedField (Scalar v), InnerSpace v)
      => Transformable (QDiagram b v m) where
  transform = over QD . applyD . inj
            . (inL :: Split (Transformation v) -> Split (Transformation v) :+: Style v)
            . M

---- Qualifiable

-- | Diagrams can be qualified so that all their named points can
--   now be referred to using the qualification prefix.
instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
      => Qualifiable (QDiagram b v m) where
  (|>) = over QD . applyD . inj . toName


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
instance HasLinearMap v => Transformable (Prim b v) where
  transform v (Prim p) = Prim (transform v p)

-- | The 'Renderable' instance for 'Prim' just pushes calls to
--   'render' down through the 'Prim' constructor.
instance HasLinearMap v => Renderable (Prim b v) b where
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
--   consists of the three associated types and implementations for
--   'withStyle' and 'doRender'.
--
class (HasLinearMap v, Monoid (Render b v)) => Backend b v where
  -- | The type of rendering operations used by this backend, which
  --   must be a monoid. For example, if @Render b v = M ()@ for some
  --   monad @M@, a monoid instance can be made with @mempty = return
  --   ()@ and @mappend = (>>)@.
  data Render  b v :: *

  -- | The result of running/interpreting a rendering operation.
  type Result  b v :: *

  -- | Backend-specific rendering options.
  data Options b v :: *

  -- | Perform a rendering operation with a local style.
  withStyle      :: b          -- ^ Backend token (needed only for type inference)
                 -> Style v    -- ^ Style to use
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
  --   rendering it.  It can also make adjustments to the options
  --   record, usually to fill in incompletely specified size
  --   information.  A default implementation is provided which makes
  --   no adjustments.  See the diagrams-lib package for other useful
  --   implementations.
  adjustDia :: Monoid' m => b -> Options b v
            -> QDiagram b v m -> (Options b v, QDiagram b v m)
  adjustDia _ o d = (o,d)

  -- XXX expand this comment.  Explain about freeze, split
  -- transformations, etc.
  -- | Render a diagram.  This has a default implementation in terms
  --   of 'adjustDia', 'withStyle', 'doRender', and the 'render'
  --   operation from the 'Renderable' class (first 'adjustDia' is
  --   used, then 'withStyle' and 'render' are used to render each
  --   primitive, the resulting operations are combined with
  --   'mconcat', and the final operation run with 'doRender') but
  --   backends may override it if desired.
  renderDia :: (InnerSpace v, OrderedField (Scalar v), Monoid' m)
            => b -> Options b v -> QDiagram b v m -> Result b v
  renderDia b opts d =
    doRender b opts' . mconcat . map renderOne . prims $ d'
      where (opts', d') = adjustDia b opts d
            renderOne :: (Prim b v, (Split (Transformation v), Style v))
                      -> Render b v
            renderOne (p, (M t,      s))
              = withStyle b s mempty (render b (transform t p))

            renderOne (p, (t1 :| t2, s))
              = withStyle b s t1 (render b (transform (t1 <> t2) p))

  -- See Note [backend token]

-- | The @D@ type is provided for convenience in situations where you
--   must give a diagram a concrete, monomorphic type, but don't care
--   which one.  Such situations arise when you pass a diagram to a
--   function which is polymorphic in its input but monomorphic in its
--   output, such as 'width', 'height', 'phantom', or 'names'.  Such
--   functions compute some property of the diagram, or use it to
--   accomplish some other purpose, but do not result in the diagram
--   being rendered.  If the diagram does not have a monomorphic type,
--   GHC complains that it cannot determine the diagram's type.
--
--   For example, here is the error we get if we try to compute the
--   width of a radius-1 circle (this example requires
--   @diagrams-lib@):
--
--   > ghci> width (circle 1)
--   >
--   > <interactive>:1:8:
--   >     No instances for (Backend b0 R2,
--   >                       Renderable Diagrams.TwoD.Ellipse.Ellipse b0)
--   >       arising from a use of `circle'
--   >     Possible fix:
--   >       add instance declarations for
--   >       (Backend b0 R2, Renderable Diagrams.TwoD.Ellipse.Ellipse b0)
--   >     In the first argument of `width', namely `(circle 1)'
--   >     In the expression: width (circle 1)
--   >     In an equation for `it': it = width (circle 1)
--
--   GHC complains that it cannot find an instance for \"@Backend b0
--   R2@\"; what is really going on is that it does not have enough
--   information to decide which backend to use for the circle (hence
--   the type variable @b0@).  This is annoying because /we/ know that
--   the choice of backend cannot possibly affect the width of the
--   circle; but there is no way for GHC to know that.
--
--   The solution is to annotate @circle 1@ with the type @'D' 'R2'@,
--   like so:
--
--   > ghci> width (circle 1 :: D R2)
--   > 2.0

type D v = Diagram NullBackend v


-- | A null backend which does no actual rendering.  It is provided
--   mainly for convenience in situations where you must give a
--   diagram a concrete, monomorphic type, but don't actually care
--   which one.  See 'D' for more explanation and examples.
--
--   It is courteous, when defining a new primitive @P@, to make an instance
--
--   > instance Renderable P NullBackend where
--   >   render _ _ = mempty
--
--   This ensures that the trick with 'D' annotations can be used for
--   diagrams containing your primitive.
data NullBackend

-- Note: we can't make a once-and-for-all instance
--
-- > instance Renderable a NullBackend where
-- >   render _ _ = mempty
--
-- because it overlaps with the Renderable instance for NullPrim.

instance Monoid (Render NullBackend v) where
  mempty      = NullBackendRender
  mappend _ _ = NullBackendRender

instance HasLinearMap v => Backend NullBackend v where
  data Render NullBackend v = NullBackendRender
  type Result NullBackend v = ()
  data Options NullBackend v

  withStyle _ _ _ _ = NullBackendRender
  doRender _ _ _    = ()

-- | A class for backends which support rendering multiple diagrams,
--   e.g. to a multi-page pdf or something similar.
class Backend b v => MultiBackend b v where

  -- | Render multiple diagrams at once.
  renderDias :: b -> Options b v -> [QDiagram b v m] -> Result b v

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

