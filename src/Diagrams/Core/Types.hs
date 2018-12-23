{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
-- We have some orphan Action instances here, but since Action is a multi-param
-- class there is really no better place to put them.

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Types
-- Copyright   :  (c) 2011-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The core library of primitives forming the basis of an embedded
-- domain-specific language for describing and rendering diagrams.
--
-- "Diagrams.Core.Types" defines types and classes for
-- primitives, diagrams, and backends.
--
-----------------------------------------------------------------------------

{- ~~~~ Note [breaking up Types module]

   Although it's not as bad as it used to be, this module has a lot of
   stuff in it, and it might seem a good idea in principle to break it up
   into smaller modules.  However, it's not as easy as it sounds: everything
   in this module cyclically depends on everything else.
-}

module Diagrams.Core.Types
       (
         -- * Diagrams

         -- ** Annotations

         -- *** Static annotations
         Annotation(Href, OpacityGroup, KeyVal)
       , applyAnnotation, href, opacityGroup, groupOpacity, keyVal

         -- *** Dynamic (monoidal) annotations
       , UpAnnots, DownAnnots, transfToAnnot, transfFromAnnot

         -- ** Basic type definitions
       , QDiaLeaf(..), withQDiaLeaf
       , QDiagram(..), Diagram

         -- * Operations on diagrams
         -- ** Creating diagrams
       , mkQD, mkQD', pointDiagram

         -- ** Extracting information
       , envelope, trace, subMap, names, query

         -- ** Combining diagrams

         -- | For many more ways of combining diagrams, see
         --   "Diagrams.Combinators" and "Diagrams.TwoD.Combinators"
         --   from the diagrams-lib package.

       , atop

         -- ** Modifying diagrams
         -- *** Names
       , nameSub
       , lookupName
       , withName
       , withNameAll
       , withNames
       , localize

         -- *** Other
       , setEnvelope
       , setTrace

         -- * Subdiagrams

       , Subdiagram(..), mkSubdiagram
       , getSub, rawSub
       , location
       , subPoint

         -- * Subdiagram maps

       , SubMap(..)

       , fromNames, rememberAs, lookupSub

         -- * Primtives
         -- $prim

       , Prim(..)
       , _Prim

         -- * Backends

       , Backend(..)
       , DTree
       , DNode(..)

       , RTree
       , RNode(..)
       , _RStyle
       , _RAnnot
       , _RPrim
       , _REmpty

         -- ** Null backend

       , NullBackend, D

         -- ** Number classes
       , TypeableFloat

         -- * Renderable

       , Renderable(..)

       ) where

import           Control.Arrow             (first, second, (***))
import           Control.Lens              (Lens', Prism', Rewrapped,
                                            Wrapped (..), iso, lens, over,
                                            prism', view, (^.), _Wrapped,
                                            _Wrapping)
import           Control.Monad             (mplus)
import           Data.List                 (isSuffixOf)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Semigroup
import qualified Data.Traversable          as T
import           Data.Tree
import           Data.Typeable

import           Data.Monoid.Action
import           Data.Monoid.Coproduct
import           Data.Monoid.Deletable
import           Data.Monoid.MList
import           Data.Monoid.WithSemigroup
import qualified Data.Tree.DUAL            as D

import           Diagrams.Core.Envelope
import           Diagrams.Core.HasOrigin
import           Diagrams.Core.Juxtapose
import           Diagrams.Core.Names
import           Diagrams.Core.Points
import           Diagrams.Core.Query
import           Diagrams.Core.Style
import           Diagrams.Core.Trace
import           Diagrams.Core.Transform
import           Diagrams.Core.V

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

-- XXX TODO: add lots of actual diagrams to illustrate the
-- documentation!  Haddock supports \<\<inline image urls\>\>.

-- | Constraint for numeric types that are 'RealFloat' and 'Typeable',
--   which often occur together.  This is used to shorten shorten type
--   constraint contexts.
type TypeableFloat n = (Typeable n, RealFloat n)

------------------------------------------------------------
--  Diagrams  ----------------------------------------------
------------------------------------------------------------

-- | Monoidal annotations which travel up the diagram tree, /i.e./ which
--   are aggregated from component diagrams to the whole:
--
--   * envelopes (see "Diagrams.Core.Envelope").
--     The envelopes are \"deletable\" meaning that at any point we can
--     throw away the existing envelope and replace it with a new one;
--     sometimes we want to consider a diagram as having a different
--     envelope unrelated to its \"natural\" envelope.
--
--   * traces (see "Diagrams.Core.Trace"), also
--     deletable.
--
--   * name/subdiagram associations (see "Diagrams.Core.Names")
--
--   * query functions (see "Diagrams.Core.Query")
type UpAnnots b v n m = Deletable (Envelope v n)
                    ::: Deletable (Trace v n)
                    ::: Deletable (SubMap b v n m)
                    ::: Query v n m
                    ::: ()

-- | Monoidal annotations which travel down the diagram tree,
--   /i.e./ which accumulate along each path to a leaf (and which can
--   act on the upwards-travelling annotations):
--
--   * styles (see "Diagrams.Core.Style")
--
--   * names (see "Diagrams.Core.Names")
type DownAnnots v n = (Transformation v n :+: Style v n)
                  ::: Name
                  ::: ()

  -- Note that we have to put the transformations and styles together
  -- using a coproduct because the transformations can act on the
  -- styles.

-- | Inject a transformation into a default downwards annotation
--   value.
transfToAnnot :: Transformation v n -> DownAnnots v n
transfToAnnot
  = inj
  . (inL :: Transformation v n -> Transformation v n :+: Style v n)

-- | Extract the (total) transformation from a downwards annotation
--   value.
transfFromAnnot :: (Additive v, Num n) => DownAnnots v n -> Transformation v n
transfFromAnnot = option mempty killR . fst

-- | A leaf in a 'QDiagram' tree is either a 'Prim', or a \"delayed\"
--   @QDiagram@ which expands to a real @QDiagram@ once it learns the
--   \"final context\" in which it will be rendered.  For example, in
--   order to decide how to draw an arrow, we must know the precise
--   transformation applied to it (since the arrow head and tail are
--   scale-invariant).
data QDiaLeaf b v n m
  = PrimLeaf (Prim b v n)
  | DelayedLeaf (DownAnnots v n -> n -> n -> QDiagram b v n m)
    -- ^ The @QDiagram@ produced by a @DelayedLeaf@ function /must/
    --   already apply any transformation in the given
    --   @DownAnnots@ (that is, the transformation will not
    --   be applied by the context).
  deriving Functor

withQDiaLeaf :: (Prim b v n -> r)
            -> ((DownAnnots v n -> n -> n -> QDiagram b v n m) -> r)
            -> QDiaLeaf b v n m -> r
withQDiaLeaf f _ (PrimLeaf p)      = f p
withQDiaLeaf _ g (DelayedLeaf dgn) = g dgn

-- | Static annotations which can be placed at a particular node of a
--   diagram tree.
data Annotation
  = Href String    -- ^ Hyperlink
  | OpacityGroup Double
  | KeyVal (String, String)
  deriving Show

-- | Apply a static annotation at the root of a diagram.
applyAnnotation
  :: (Metric v, OrderedField n, Semigroup m)
  => Annotation -> QDiagram b v n m -> QDiagram b v n m
applyAnnotation an (QD dt) = QD (D.annot an dt)

-- | Make a diagram into a hyperlink.  Note that only some backends
--   will honor hyperlink annotations.
href :: (Metric v, OrderedField n, Semigroup m)
  => String -> QDiagram b v n m -> QDiagram b v n m
href = applyAnnotation . Href

-- | Change the transparency of a 'Diagram' as a group.
opacityGroup, groupOpacity :: (Metric v, OrderedField n, Semigroup m)
  => Double -> QDiagram b v n m -> QDiagram b v n m
opacityGroup = applyAnnotation . OpacityGroup
groupOpacity = applyAnnotation . OpacityGroup

-- | Apply a general Key-Value annotation
keyVal :: (Metric v, OrderedField n, Semigroup m)
  => (String, String) -> QDiagram b v n m -> QDiagram b v n m
keyVal = applyAnnotation . KeyVal

-- | The fundamental diagram type.  The type variables are as follows:
--
--   * @b@ represents the backend, such as @SVG@ or @Cairo@.  Note
--     that each backend also exports a type synonym @B@ for itself,
--     so the type variable @b@ may also typically be instantiated by
--     @B@, meaning \"use whatever backend is in scope\".
--
--   * @v@ represents the vector space of the diagram.  Typical
--     instantiations include @V2@ (for a two-dimensional diagram) or
--     @V3@ (for a three-dimensional diagram).
--
--   * @n@ represents the numerical field the diagram uses.  Typically
--     this will be a concrete numeric type like @Double@.
--
--   * @m@ is the monoidal type of \"query annotations\": each point
--     in the diagram has a value of type @m@ associated to it, and
--     these values are combined according to the 'Monoid' instance
--     for @m@.  Most often, @m@ is simply instantiated to 'Any',
--     associating a simple @Bool@ value to each point indicating
--     whether the point is inside the diagram; 'Diagram' is a synonym
--     for @QDiagram@ with @m@ thus instantiated to @Any@.
--
--   Diagrams can be combined via their 'Monoid' instance, transformed
--   via their 'Transformable' instance, and assigned attributes via
--   their 'HasStyle' instance.
--
--   Note that the @Q@ in @QDiagram@ stands for \"Queriable\", as
--   distinguished from 'Diagram', where @m@ is fixed to @Any@.  This
--   is not really a very good name, but it's probably not worth
--   changing it at this point.
newtype QDiagram b v n m
  = QD (D.DUALTree (DownAnnots v n) (UpAnnots b v n m) Annotation (QDiaLeaf b v n m))
#if __GLASGOW_HASKELL__ >= 707
  deriving Typeable
#else

instance forall b v. (Typeable b, Typeable1 v) => Typeable2 (QDiagram b v) where
  typeOf2 _ = mkTyConApp (mkTyCon3 "diagrams-core" "Diagrams.Core.Types" "QDiagram") [] `mkAppTy`
              typeOf (undefined :: b)                                                   `mkAppTy`
              typeOf1 (undefined :: v n)
#endif

instance Wrapped (QDiagram b v n m) where
  type Unwrapped (QDiagram b v n m) =
        D.DUALTree (DownAnnots v n) (UpAnnots b v n m) Annotation (QDiaLeaf b v n m)
  _Wrapped' = iso (\(QD d) -> d) QD

instance Rewrapped (QDiagram b v n m) (QDiagram b' v' n' m')

type instance V (QDiagram b v n m) = v
type instance N (QDiagram b v n m) = n

-- | @Diagram b@ is a synonym for @'QDiagram' b (V b) (N b) 'Any'@.  That is,
--   the default sort of diagram is one where querying at a point
--   simply tells you whether the diagram contains that point or not.
--   Transforming a default diagram into one with a more interesting
--   query can be done via the 'Functor' instance of @'QDiagram' b v n@ or
--   the 'value' function.
type Diagram b = QDiagram b (V b) (N b) Any

-- | Create a \"point diagram\", which has no content, no trace, an
--   empty query, and a point envelope.
pointDiagram :: (Metric v, Fractional n)
             => Point v n -> QDiagram b v n m
pointDiagram p = QD $ D.leafU (inj . toDeletable $ pointEnvelope p)

-- | A useful variant of 'getU' which projects out a certain
--   component.
getU' :: (Monoid u', u :>: u') => D.DUALTree d u a l -> u'
getU' = maybe mempty (option mempty id . get) . D.getU

-- | Lens onto the 'Envelope' of a 'QDiagram'.
envelope :: (OrderedField n, Metric v, Monoid' m)
         => Lens' (QDiagram b v n m) (Envelope v n)
envelope = lens (unDelete . getU' . view _Wrapped') (flip setEnvelope)

-- | Replace the envelope of a diagram.
setEnvelope :: forall b v n m. ( OrderedField n, Metric v
                               , Monoid' m)
          => Envelope v n -> QDiagram b v n m -> QDiagram b v n m
setEnvelope e =
    over _Wrapped' ( D.applyUpre (inj . toDeletable $ e)
                . D.applyUpre (inj (deleteL :: Deletable (Envelope v n)))
                . D.applyUpost (inj (deleteR :: Deletable (Envelope v n)))
              )

-- | Lens onto the 'Trace' of a 'QDiagram'.
trace :: (Metric v, OrderedField n, Semigroup m) =>
         Lens' (QDiagram b v n m) (Trace v n)
trace = lens (unDelete . getU' . view _Wrapped') (flip setTrace)

-- | Replace the trace of a diagram.
setTrace :: forall b v n m. ( OrderedField n, Metric v
                            , Semigroup m)
         => Trace v n -> QDiagram b v n m -> QDiagram b v n m
setTrace t = over _Wrapped' ( D.applyUpre (inj . toDeletable $ t)
                            . D.applyUpre (inj (deleteL :: Deletable (Trace v n)))
                            . D.applyUpost (inj (deleteR :: Deletable (Trace v n)))
                            )

-- | Lens onto the 'SubMap' of a 'QDiagram' (/i.e./ an association from
--   names to subdiagrams).
subMap :: (Metric v, Semigroup m, OrderedField n)
       => Lens' (QDiagram b v n m) (SubMap b v n m)
subMap = lens (unDelete . getU' . view _Wrapped') (flip setMap)
  where
    setMap :: (Metric v, Semigroup m, OrderedField n) =>
              SubMap b v n m -> QDiagram b v n m -> QDiagram b v n m
    setMap m = over _Wrapped' ( D.applyUpre . inj . toDeletable $ m)

-- | Get a list of names of subdiagrams and their locations.
names :: (Metric v, Semigroup m, OrderedField n)
      => QDiagram b v n m -> [(Name, [Point v n])]
names = (map . second . map) location . M.assocs . view (subMap . _Wrapped')

-- | Attach an atomic name to a certain subdiagram, computed from the
--   given diagram /with the mapping from name to subdiagram
--   included/.  The upshot of this knot-tying is that if @d' = d #
--   named x@, then @lookupName x d' == Just d'@ (instead of @Just
--   d@).
nameSub :: (IsName nm , Metric v, OrderedField n, Semigroup m)
  => (QDiagram b v n m -> Subdiagram b v n m) -> nm -> QDiagram b v n m -> QDiagram b v n m
nameSub s n d = d'
  where d' = over _Wrapped' (D.applyUpre . inj . toDeletable $ fromNames [(n,s d')]) d

-- | Lookup the most recent diagram associated with (some
--   qualification of) the given name.
lookupName :: (IsName nm, Metric v, Semigroup m, OrderedField n)
           => nm -> QDiagram b v n m -> Maybe (Subdiagram b v n m)
lookupName n d = lookupSub (toName n) (d^.subMap) >>= listToMaybe

-- | Given a name and a diagram transformation indexed by a
--   subdiagram, perform the transformation using the most recent
--   subdiagram associated with (some qualification of) the name,
--   or perform the identity transformation if the name does not exist.
withName :: (IsName nm, Metric v
            , Semigroup m, OrderedField n)
         => nm -> (Subdiagram b v n m -> QDiagram b v n m -> QDiagram b v n m)
         -> QDiagram b v n m -> QDiagram b v n m
withName n f d = maybe id f (lookupName n d) d

-- | Given a name and a diagram transformation indexed by a list of
--   subdiagrams, perform the transformation using the
--   collection of all such subdiagrams associated with (some
--   qualification of) the given name.
withNameAll :: (IsName nm, Metric v
               , Semigroup m, OrderedField n)
            => nm -> ([Subdiagram b v n m] -> QDiagram b v n m -> QDiagram b v n m)
            -> QDiagram b v n m -> QDiagram b v n m
withNameAll n f d = f (fromMaybe [] (lookupSub (toName n) (d^.subMap))) d

-- | Given a list of names and a diagram transformation indexed by a
--   list of subdiagrams, perform the transformation using the
--   list of most recent subdiagrams associated with (some qualification
--   of) each name.  Do nothing (the identity transformation) if any
--   of the names do not exist.
withNames :: (IsName nm, Metric v
             , Semigroup m, OrderedField n)
          => [nm] -> ([Subdiagram b v n m] -> QDiagram b v n m -> QDiagram b v n m)
          -> QDiagram b v n m -> QDiagram b v n m
withNames ns f d = maybe id f ns' d
  where
    nd = d^.subMap
    ns' = T.sequence (map ((listToMaybe=<<) . ($ nd) . lookupSub . toName) ns)

-- | \"Localize\" a diagram by hiding all the names, so they are no
--   longer visible to the outside.
localize :: forall b v n m. (Metric v, OrderedField n, Semigroup m)
         => QDiagram b v n m -> QDiagram b v n m
localize = over _Wrapped' ( D.applyUpre  (inj (deleteL :: Deletable (SubMap b v n m)))
                   . D.applyUpost (inj (deleteR :: Deletable (SubMap b v n m)))
                   )

-- | Get the query function associated with a diagram.
query :: Monoid m => QDiagram b v n m -> Query v n m
query = getU' . view _Wrapped'

-- | Create a diagram from a single primitive, along with an envelope,
--   trace, subdiagram map, and query function.
mkQD :: Prim b v n -> Envelope v n -> Trace v n -> SubMap b v n m -> Query v n m
     -> QDiagram b v n m
mkQD p = mkQD' (PrimLeaf p)

-- | Create a diagram from a generic QDiaLeaf, along with an envelope,
--   trace, subdiagram map, and query function.
mkQD' :: QDiaLeaf b v n m -> Envelope v n -> Trace v n -> SubMap b v n m -> Query v n m
      -> QDiagram b v n m
mkQD' l e t n q
  = QD $ D.leaf (toDeletable e *: toDeletable t *: toDeletable n *: q *: ()) l

------------------------------------------------------------
--  Instances
------------------------------------------------------------

---- Monoid

-- | Diagrams form a monoid since each of their components do: the
--   empty diagram has no primitives, an empty envelope, an empty
--   trace, no named subdiagrams, and a constantly empty query
--   function.
--
--   Diagrams compose by aligning their respective local origins.  The
--   new diagram has all the primitives and all the names from the two
--   diagrams combined, and query functions are combined pointwise.
--   The first diagram goes on top of the second.  \"On top of\"
--   probably only makes sense in vector spaces of dimension lower
--   than 3, but in theory it could make sense for, say, 3-dimensional
--   diagrams when viewed by 4-dimensional beings.
instance (Metric v, OrderedField n, Semigroup m)
  => Monoid (QDiagram b v n m) where
  mempty  = QD D.empty
  mappend = (<>)

instance (Metric v, OrderedField n, Semigroup m)
  => Semigroup (QDiagram b v n m) where
  (QD d1) <> (QD d2) = QD (d2 <> d1)
    -- swap order so that primitives of d2 come first, i.e. will be
    -- rendered first, i.e. will be on the bottom.

-- | A convenient synonym for 'mappend' on diagrams, designed to be
--   used infix (to help remember which diagram goes on top of which
--   when combining them, namely, the first on top of the second).
atop :: (OrderedField n, Metric v, Semigroup m)
     => QDiagram b v n m -> QDiagram b v n m -> QDiagram b v n m
atop = (<>)

infixl 6 `atop`

---- Functor

instance Functor (QDiagram b v n) where
  fmap f = over (_Wrapping QD)
           ( (D.mapU . second . second)
             ( (first . fmap . fmap . fmap)   f
             . (second . first . fmap . fmap) f
             )
           . (fmap . fmap) f
           )

---- Applicative

-- XXX what to do with this?
-- A diagram with queries of result type @(a -> b)@ can be \"applied\"
--   to a diagram with queries of result type @a@, resulting in a
--   combined diagram with queries of result type @b@.  In particular,
--   all components of the two diagrams are combined as in the
--   @Monoid@ instance, except the queries which are combined via
--   @(<*>)@.

-- instance (Backend b v n, Num n, Ord n)
--            => Applicative (QDiagram b v n) where
--   pure a = Diagram mempty mempty mempty (Query $ const a)
--
--   (Diagram ps1 bs1 ns1 smp1) <*> (Diagram ps2 bs2 ns2 smp2)
--     = Diagram (ps1 <> ps2) (bs1 <> bs2) (ns1 <> ns2) (smp1 <*> smp2)

---- HasStyle

instance (Metric v, OrderedField n, Semigroup m)
      => HasStyle (QDiagram b v n m) where
  applyStyle = over _Wrapped' . D.applyD . inj
             . (inR :: Style v n -> Transformation v n :+: Style v n)

---- Juxtaposable

instance (Metric v, OrderedField n, Monoid' m)
      => Juxtaposable (QDiagram b v n m) where
  juxtapose = juxtaposeDefault

---- Enveloped

instance (Metric v, OrderedField n, Monoid' m)
         => Enveloped (QDiagram b v n m) where
  getEnvelope = view envelope

---- Traced

instance (Metric v, OrderedField n, Semigroup m)
         => Traced (QDiagram b v n m) where
  getTrace = view trace

---- HasOrigin

-- | Every diagram has an intrinsic \"local origin\" which is the
--   basis for all combining operations.
instance (Metric v, OrderedField n, Semigroup m)
      => HasOrigin (QDiagram b v n m) where
  moveOriginTo = translate . (origin .-.)

---- Transformable

-- | Diagrams can be transformed by transforming each of their
--   components appropriately.
instance (OrderedField n, Metric v, Semigroup m)
      => Transformable (QDiagram b v n m) where
  transform = over _Wrapped' . D.applyD . transfToAnnot

---- Qualifiable

-- | Diagrams can be qualified so that all their named points can
--   now be referred to using the qualification prefix.
instance (Metric v, OrderedField n, Semigroup m)
      => Qualifiable (QDiagram b v n m) where
  (.>>) = over _Wrapped' . D.applyD . inj . toName


------------------------------------------------------------
--  Subdiagrams
------------------------------------------------------------

-- | A @Subdiagram@ represents a diagram embedded within the context
--   of a larger diagram.  Essentially, it consists of a diagram
--   paired with any accumulated information from the larger context
--   (transformations, attributes, etc.).

data Subdiagram b v n m = Subdiagram (QDiagram b v n m) (DownAnnots v n)

type instance V (Subdiagram b v n m) = v
type instance N (Subdiagram b v n m) = n

-- | Turn a diagram into a subdiagram with no accumulated context.
mkSubdiagram :: QDiagram b v n m -> Subdiagram b v n m
mkSubdiagram d = Subdiagram d empty

-- | Create a \"point subdiagram\", that is, a 'pointDiagram' (with no
--   content and a point envelope) treated as a subdiagram with local
--   origin at the given point.  Note this is not the same as
--   @mkSubdiagram . pointDiagram@, which would result in a subdiagram
--   with local origin at the parent origin, rather than at the given
--   point.
subPoint :: (Metric v, OrderedField n)
         => Point v n -> Subdiagram b v n m
subPoint p = Subdiagram
               (pointDiagram origin)
               (transfToAnnot $ translation (p .-. origin))

instance Functor (Subdiagram b v n) where
  fmap f (Subdiagram d a) = Subdiagram (fmap f d) a

instance (OrderedField n, Metric v, Monoid' m)
      => Enveloped (Subdiagram b v n m) where
  getEnvelope (Subdiagram d a) = transform (transfFromAnnot a) $ getEnvelope d

instance (OrderedField n, Metric v, Semigroup m)
      => Traced (Subdiagram b v n m) where
  getTrace (Subdiagram d a) = transform (transfFromAnnot a) $ getTrace d

instance (Metric v, OrderedField n)
      => HasOrigin (Subdiagram b v n m) where
  moveOriginTo = translate . (origin .-.)

instance Transformable (Subdiagram b v n m) where
  transform t (Subdiagram d a) = Subdiagram d (transfToAnnot t <> a)

-- | Get the location of a subdiagram; that is, the location of its
--   local origin /with respect to/ the vector space of its parent
--   diagram.  In other words, the point where its local origin
--   \"ended up\".
location :: (Additive v, Num n) => Subdiagram b v n m -> Point v n
location (Subdiagram _ a) = transform (transfFromAnnot a) origin

-- | Turn a subdiagram into a normal diagram, including the enclosing
--   context.  Concretely, a subdiagram is a pair of (1) a diagram and
--   (2) a \"context\" consisting of an extra transformation and
--   attributes.  @getSub@ simply applies the transformation and
--   attributes to the diagram to get the corresponding \"top-level\"
--   diagram.
getSub :: (Metric v, OrderedField n, Semigroup m)
       => Subdiagram b v n m -> QDiagram b v n m
getSub (Subdiagram d a) = over _Wrapped' (D.applyD a) d

-- | Extract the \"raw\" content of a subdiagram, by throwing away the
--   context.
rawSub :: Subdiagram b v n m -> QDiagram b v n m
rawSub (Subdiagram d _) = d

------------------------------------------------------------
--  Subdiagram maps  ---------------------------------------
------------------------------------------------------------

-- | A 'SubMap' is a map associating names to subdiagrams. There can
--   be multiple associations for any given name.
newtype SubMap b v n m = SubMap (M.Map Name [Subdiagram b v n m])
  -- See Note [SubMap Set vs list]

instance Wrapped (SubMap b v n m) where
  type Unwrapped (SubMap b v n m) = M.Map Name [Subdiagram b v n m]
  _Wrapped' = iso (\(SubMap m) -> m) SubMap

instance Rewrapped (SubMap b v n m) (SubMap b' v' n' m')

-- ~~~~ [SubMap Set vs list]
-- In some sense it would be nicer to use
-- Sets instead of a list, but then we would have to put Ord
-- constraints on v everywhere. =P

type instance V (SubMap b v n m) = v
type instance N (SubMap b v n m) = n

instance Functor (SubMap b v n) where
  fmap = over _Wrapped . fmap . map . fmap

instance Semigroup (SubMap b v n m) where
  SubMap s1 <> SubMap s2 = SubMap $ M.unionWith (++) s1 s2

-- | 'SubMap's form a monoid with the empty map as the identity, and
--   map union as the binary operation.  No information is ever lost:
--   if two maps have the same name in their domain, the resulting map
--   will associate that name to the concatenation of the information
--   associated with that name.
instance Monoid (SubMap b v n m) where
  mempty  = SubMap M.empty
  mappend = (<>)

instance (OrderedField n, Metric v)
      => HasOrigin (SubMap b v n m) where
  moveOriginTo = over _Wrapped' . moveOriginTo

instance Transformable (SubMap b v n m) where
  transform = over _Wrapped' . transform

-- | 'SubMap's are qualifiable: if @ns@ is a 'SubMap', then @a |>
--   ns@ is the same 'SubMap' except with every name qualified by
--   @a@.
instance Qualifiable (SubMap b v n m) where
  a .>> (SubMap m) = SubMap $ M.mapKeys (a .>>) m

-- | Construct a 'SubMap' from a list of associations between names
--   and subdiagrams.
fromNames :: IsName a => [(a, Subdiagram b v n m)] -> SubMap b v n m
fromNames = SubMap . M.fromListWith (++) . map (toName *** (:[]))

-- | Add a name/diagram association to a submap.
rememberAs :: IsName a => a -> QDiagram b v n m -> SubMap b v n m -> SubMap b v n m
rememberAs n b = over _Wrapped' $ M.insertWith (++) (toName n) [mkSubdiagram b]

-- | A name acts on a name map by qualifying every name in it.
instance Action Name (SubMap b v n m) where
  act = (.>>)

instance Action Name a => Action Name (Deletable a) where
  act n (Deletable l a r) = Deletable l (act n a) r

-- Names do not act on other things.

instance Action Name (Query v n m)
instance Action Name (Envelope v n)
instance Action Name (Trace v n)

-- | Look for the given name in a name map, returning a list of
--   subdiagrams associated with that name.  If no names match the
--   given name exactly, return all the subdiagrams associated with
--   names of which the given name is a suffix.
lookupSub :: IsName nm => nm -> SubMap b v n m -> Maybe [Subdiagram b v n m]
lookupSub a (SubMap m)
  = M.lookup n m `mplus`
    (flattenNames . filter ((n `nameSuffixOf`) . fst) . M.assocs $ m)
  where (Name n1) `nameSuffixOf` (Name n2) = n1 `isSuffixOf` n2
        flattenNames [] = Nothing
        flattenNames xs = Just . concatMap snd $ xs
        n = toName a

------------------------------------------------------------
--  Primitives  --------------------------------------------
------------------------------------------------------------

-- $prim
-- Ultimately, every diagram is essentially a tree whose leaves are /primitives/,
-- basic building blocks which can be rendered by backends.  However,
-- not every backend must be able to render every type of primitive;
-- the collection of primitives a given backend knows how to render is
-- determined by instances of 'Renderable'.

-- | A value of type @Prim b v n@ is an opaque (existentially quantified)
--   primitive which backend @b@ knows how to render in vector space @v@.
data Prim b v n where
  Prim :: (Transformable p, Typeable p, Renderable p b) => p -> Prim b (V p) (N p)

_Prim :: (Typeable p, Renderable p b) => Prism' (Prim b (V p) (N p)) p
_Prim = prism' Prim (\(Prim p) -> cast p)

type instance V (Prim b v n) = v
type instance N (Prim b v n) = n

-- | The 'Transformable' instance for 'Prim' just pushes calls to
--   'transform' down through the 'Prim' constructor.
instance Transformable (Prim b v n) where
  transform t (Prim p) = Prim (transform t p)

-- | The 'Renderable' instance for 'Prim' just pushes calls to
--   'render' down through the 'Prim' constructor.
instance Renderable (Prim b v n) b where
  render b (Prim p) = render b p

------------------------------------------------------------
-- Backends  -----------------------------------------------
------------------------------------------------------------

-- | A 'DTree' is a raw tree representation of a 'QDiagram', with all
--   the @u@-annotations removed.  It is used as an intermediate type
--   by diagrams-core; backends should not need to make use of it.
--   Instead, backends can make use of 'RTree', which 'DTree' gets
--   compiled and optimized to.
type DTree b v n a = Tree (DNode b v n a)

data DNode b v n a = DStyle (Style v n)
                   | DTransform (Transformation v n)
                   | DAnnot a
                   | DDelay
                     -- ^ @DDelay@ marks a point where a delayed subtree
                     --   was expanded.  Such subtrees already take all
                     --   non-frozen transforms above them into account,
                     --   so when later processing the tree, upon
                     --   encountering a @DDelay@ node we must drop any
                     --   accumulated non-frozen transformation.
                   | DPrim (Prim b v n)
                   | DEmpty

-- | An 'RTree' is a compiled and optimized representation of a
--   'QDiagram', which can be used by backends.  They have the
--   following invariant which backends may rely upon:
--
--   * @RPrim@ nodes never have any children.
type RTree b v n a = Tree (RNode b v n a)

data RNode b v n a = RStyle (Style v n) -- ^ A style node.
                   | RAnnot a
                   | RPrim (Prim b v n) -- ^ A primitive.
                   | REmpty

-- | Prism onto a style of an 'RNode'.
_RStyle :: Prism' (RNode b v n a) (Style v n)
_RStyle = prism' RStyle $ \n -> case n of RStyle s -> Just s; _ -> Nothing

-- | Prism onto an annotation of an 'RNode'.
_RAnnot :: Prism' (RNode b v n a) a
_RAnnot = prism' RAnnot $ \n -> case n of RAnnot a -> Just a; _ -> Nothing

-- | Prism onto a 'Prim' of an 'RNode'.
_RPrim :: Prism' (RNode b v n a) (Prim b v n)
_RPrim = prism' RPrim $ \n -> case n of RPrim p -> Just p; _ -> Nothing

-- | Prism onto an empty 'RNode'.
_REmpty :: Prism' (RNode b v n a) ()
_REmpty = prism' (const REmpty) $ \n -> case n of REmpty -> Just (); _ -> Nothing

-- | Abstract diagrams are rendered to particular formats by
--   /backends/.  Each backend/vector space combination must be an
--   instance of the 'Backend' class.
--
--   A minimal complete definition consists of 'Render', 'Result',
--   'Options', and 'renderRTree'. However, most backends will want to
--   implement 'adjustDia' as well; the default definition does
--   nothing.  Some useful standard definitions are provided in the
--   @Diagrams.TwoD.Adjust@ module from the @diagrams-lib@ package.
class Backend b v n where

  -- | An intermediate representation used for rendering primitives.
  --   (Typically, this will be some sort of monad, but it need not
  --   be.)  The 'Renderable' class guarantees that a backend will be
  --   able to convert primitives into this type; how these rendered
  --   primitives are combined into an ultimate 'Result' is completely
  --   up to the backend.
  data Render b v n :: *

  -- | The result of running/interpreting a rendering operation.
  type Result b v n :: *

  -- | Backend-specific rendering options.
  data Options b v n :: *

  -- | 'adjustDia' allows the backend to make adjustments to the final
  --   diagram (e.g. to adjust the size based on the options) before
  --   rendering it. It returns a modified options record, the
  --   transformation applied to the diagram (which can be used to
  --   convert attributes whose value is @Measure@, or transform
  --   /e.g./ screen coordinates back into local diagram coordinates),
  --   and the adjusted diagram itself.
  --
  --   See the diagrams-lib package (particularly the
  --   @Diagrams.TwoD.Adjust@ module) for some useful implementations.
  adjustDia :: (Additive v, Monoid' m, Num n) => b -> Options b v n
            -> QDiagram b v n m -> (Options b v n, Transformation v n, QDiagram b v n m)
  adjustDia _ o d = (o,mempty,d)

  -- | Given some options, take a representation of a diagram as a
  --   tree and render it.  The 'RTree' has already been simplified
  --   and has all measurements converted to @Output@ units.
  renderRTree :: b -> Options b v n -> RTree b v n Annotation -> Result b v n

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
--   width of an image (this example requires @diagrams-lib@):
--
--   @
--   ghci> width (image (uncheckedImageRef \"foo.png\" 200 200))
--   \<interactive\>:11:8:
--       No instance for (Renderable (DImage n0 External) b0)
--         arising from a use of `image'
--       The type variables `n0', `b0' are ambiguous
--       Possible fix: add a type signature that fixes these type variable(s)
--       Note: there is a potential instance available:
--         instance Fractional n => Renderable (DImage n a) NullBackend
--           -- Defined in `Diagrams.TwoD.Image'
--       Possible fix:
--         add an instance declaration for
--         (Renderable (DImage n0 External) b0)
--       In the first argument of `width', namely
--         `(image (uncheckedImageRef \"foo.png\" 200 200))'
--       In the expression:
--         width (image (uncheckedImageRef \"foo.png\" 200 200))
--       In an equation for `it':
--           it = width (image (uncheckedImageRef \"foo.png\" 200 200))
--   @
--
--   GHC complains that there is no instance for @Renderable (DImage n0
--   External) b0@; what is really going on is that it does not have enough
--   information to decide what backend to use (hence the
--   uninstantiated @n0@ and @b0@). This is annoying because /we/ know that the
--   choice of backend cannot possibly affect the width of the image
--   (it's 200! it's right there in the code!); /but/ there is no way
--   for GHC to know that.
--
--   The solution is to annotate the call to 'image' with the type
--   @'D' 'V2' 'Double'@, like so:
--
--   @
--   ghci> width (image (uncheckedImageRef \"foo.png\" 200 200) :: D V2 Double)
--   200.00000000000006
--   @
--
--   (It turns out the width wasn't 200 after all...)
--
--   As another example, here is the error we get if we try to compute
--   the width of a radius-1 circle:
--
--   @
--   ghci> width (circle 1)
--   \<interactive\>:12:1:
--       Couldn't match expected type `V2' with actual type `V a0'
--       The type variable `a0' is ambiguous
--       Possible fix: add a type signature that fixes these type variable(s)
--       In the expression: width (circle 1)
--       In an equation for `it': it = width (circle 1)
--   @
--
--   There's even more ambiguity here.  Whereas 'image' always returns
--   a 'Diagram', the 'circle' function can produce any 'TrailLike'
--   type, and the 'width' function can consume any 'Enveloped' type,
--   so GHC has no idea what type to pick to go in the middle.
--   However, the solution is the same:
--
--   @
--   ghci> width (circle 1 :: D V2 Double)
--   1.9999999999999998
--   @

type D v n = QDiagram NullBackend v n Any


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
  deriving Typeable

-- Note: we can't make a once-and-for-all instance
--
-- > instance Renderable a NullBackend where
-- >   render _ _ = mempty
--
-- because it overlaps with the Renderable instance for NullPrim.

instance Semigroup (Render NullBackend v n) where
  _ <> _ = NullBackendRender

instance Monoid (Render NullBackend v n) where
  mempty  = NullBackendRender
#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

instance Backend NullBackend v n where
  data Render NullBackend v n = NullBackendRender
  type Result NullBackend v n = ()
  data Options NullBackend v n

  renderRTree _ _ _ = ()

-- | The Renderable type class connects backends to primitives which
--   they know how to render.
class Transformable t => Renderable t b where
  render :: b -> t -> Render b (V t) (N t)
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
