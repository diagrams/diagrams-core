{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE EmptyDataDecls #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Types
-- Copyright   :  (c) 2011-2012 diagrams-core team (see LICENSE)
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
         UpAnnots, DownAnnots
       , QDiagram(..), mkQD, Diagram

         -- * Operations on diagrams
         -- ** Extracting information
       , prims
       , envelope, trace, subMap, names, query, sample
       , value, resetValue, clearValue

         -- ** Combining diagrams

         -- | For many more ways of combining diagrams, see
         -- "Diagrams.Combinators" from the diagrams-lib package.

       , atop

         -- ** Modifying diagrams
         -- *** Names
       , named
       , nameSub
       , namePoint
       , withName
       , withNameAll
       , withNames

         -- *** Other
       , freeze
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
import           Control.Arrow (first, second, (***))
import           Control.Monad (mplus)
import           Control.Newtype
import           Data.AffineSpace ((.-.))
import           Data.List (isSuffixOf)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe, fromMaybe)
import           Data.Semigroup
import qualified Data.Traversable as T
import           Data.Typeable
import           Data.VectorSpace

import           Data.Monoid.Action
import           Data.Monoid.Coproduct
import           Data.Monoid.Deletable
import           Data.Monoid.MList
import           Data.Monoid.Split
import           Data.Monoid.WithSemigroup
import qualified Data.Tree.DUAL as D

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

-- XXX TODO: add lots of actual diagrams to illustrate the
-- documentation!  Haddock supports \<\<inline image urls\>\>.

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
type UpAnnots b v m = Deletable (Envelope v)
                  ::: Deletable (Trace v)
                  ::: SubMap b v m
                  ::: Query v m
                  ::: ()

-- | Monoidal annotations which travel down the diagram tree,
--   /i.e./ which accumulate along each path to a leaf (and which can
--   act on the upwards-travelling annotations):
--
--   * transformations (split at the innermost freeze): see
--     "Diagrams.Core.Transform"
--
--   * styles (see "Diagrams.Core.Style")
--
--   * names (see "Diagrams.Core.Names")
type DownAnnots v = (Split (Transformation v) :+: Style v)
                ::: Name
                ::: ()

  -- Note that we have to put the transformations and styles together
  -- using a coproduct because the transformations can act on the
  -- styles.

-- | Inject a transformation into a default downwards annotation
--   value.
transfToAnnot :: Transformation v -> DownAnnots v
transfToAnnot
  = inj
  . (inL :: Split (Transformation v) -> Split (Transformation v) :+: Style v)
  . M

-- | Extract the (total) transformation from a downwards annotation
--   value.
transfFromAnnot :: HasLinearMap v => DownAnnots v -> Transformation v
transfFromAnnot = option mempty (unsplit . killR) . fst

-- | The fundamental diagram type is represented by trees of
--   primitives with various monoidal annotations.  The @Q@ in
--   @QDiagram@ stands for \"Queriable\", as distinguished from
--   'Diagram', a synonym for @QDiagram@ with the query type
--   specialized to 'Any'.
newtype QDiagram b v m
  = QD { unQD :: D.DUALTree (DownAnnots v) (UpAnnots b v m) () (Prim b v) }
  deriving (Typeable)

instance Newtype (QDiagram b v m)
                 (D.DUALTree (DownAnnots v) (UpAnnots b v m) () (Prim b v)) where
  pack   = QD
  unpack = unQD

type instance V (QDiagram b v m) = v

-- | The default sort of diagram is one where querying at a point
--   simply tells you whether the diagram contains that point or not.
--   Transforming a default diagram into one with a more interesting
--   query can be done via the 'Functor' instance of @'QDiagram' b@ or
--   the 'value' function.
type Diagram b v = QDiagram b v Any

-- | Create a \"point diagram\", which has no content, no trace, an
--   empty query, and a point envelope.
pointDiagram :: (Fractional (Scalar v), InnerSpace v)
             => Point v -> QDiagram b v m
pointDiagram p = QD $ D.leafU (inj . toDeletable $ pointEnvelope p)

-- | Extract a list of primitives from a diagram, together with their
--   associated transformations and styles.
prims :: HasLinearMap v
      => QDiagram b v m -> [(Prim b v, (Split (Transformation v), Style v))]
prims = (map . second) (untangle . option mempty id . fst)
      . D.flatten
      . unQD

-- | A useful variant of 'getU' which projects out a certain
--   component.
getU' :: (Monoid u', u :>: u') => D.DUALTree d u a l -> u'
getU' = maybe mempty (option mempty id . get) . D.getU

-- | Get the envelope of a diagram.
envelope :: (Ord (Scalar v))
         => QDiagram b v m -> Envelope v
envelope = unDelete . getU' . unQD

-- | Replace the envelope of a diagram.
setEnvelope :: forall b v m. (OrderedField (Scalar v), InnerSpace v, HasLinearMap v, Monoid' m)
          => Envelope v -> QDiagram b v m -> QDiagram b v m
setEnvelope e = over QD ( D.applyUpre (inj . toDeletable $ e)
                        . D.applyUpre (inj (deleteL :: Deletable (Envelope v)))
                        . D.applyUpost (inj (deleteR :: Deletable (Envelope v)))
                        )

-- | Get the trace of a diagram.
trace :: (Ord (Scalar v), VectorSpace v, HasLinearMap v) => QDiagram b v m -> Trace v
trace = unDelete . getU' . unQD

-- | Replace the trace of a diagram.
setTrace :: forall b v m. (OrderedField (Scalar v), InnerSpace v, HasLinearMap v, Semigroup m)
         => Trace v -> QDiagram b v m -> QDiagram b v m
setTrace t = over QD ( D.applyUpre (inj . toDeletable $ t)
                     . D.applyUpre (inj (deleteL :: Deletable (Trace v)))
                     . D.applyUpost (inj (deleteR :: Deletable (Trace v)))
                     )

-- | Get the subdiagram map (/i.e./ an association from names to
--   subdiagrams) of a diagram.
subMap :: QDiagram b v m -> SubMap b v m
subMap = getU' . unQD

-- | Get a list of names of subdiagrams and their locations.
names :: HasLinearMap v => QDiagram b v m -> [(Name, [Point v])]
names = (map . second . map) location . M.assocs . unpack . subMap

-- | Attach an atomic name to a diagram.
named :: ( IsName n
         , HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
      => n -> QDiagram b v m -> QDiagram b v m
named = nameSub mkSubdiagram

-- | Attach an atomic name to a certain point (which may be computed
--   from the given diagram), treated as a subdiagram with no content
--   and a point envelope.
namePoint :: ( IsName n
             , HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
          => (QDiagram b v m -> Point v) -> n -> QDiagram b v m -> QDiagram b v m
namePoint p = nameSub (subPoint . p)

-- | Attach an atomic name to a certain subdiagram, computed from the
--   given diagram.
nameSub :: ( IsName n
           , HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
        => (QDiagram b v m -> Subdiagram b v m) -> n -> QDiagram b v m -> QDiagram b v m
nameSub s n d = over QD (D.applyUpre . inj $ fromNames [(n,s d)]) d

-- | Given a name and a diagram transformation indexed by a
--   subdiagram, perform the transformation using the most recent
--   subdiagram associated with (some qualification of) the name,
--   or perform the identity transformation if the name does not exist.
withName :: IsName n
         => n -> (Subdiagram b v m -> QDiagram b v m -> QDiagram b v m)
         -> QDiagram b v m -> QDiagram b v m
withName n f d = maybe id f (lookupSub (toName n) (subMap d) >>= listToMaybe) d

-- | Given a name and a diagram transformation indexed by a list of
--   subdiagrams, perform the transformation using the
--   collection of all such subdiagrams associated with (some
--   qualification of) the given name.
withNameAll :: IsName n
            => n -> ([Subdiagram b v m] -> QDiagram b v m -> QDiagram b v m)
            -> QDiagram b v m -> QDiagram b v m
withNameAll n f d = f (fromMaybe [] (lookupSub (toName n) (subMap d))) d

-- | Given a list of names and a diagram transformation indexed by a
--   list of subdiagrams, perform the transformation using the
--   list of most recent subdiagrams associated with (some qualification
--   of) each name.  Do nothing (the identity transformation) if any
--   of the names do not exist.
withNames :: IsName n
          => [n] -> ([Subdiagram b v m] -> QDiagram b v m -> QDiagram b v m)
          -> QDiagram b v m -> QDiagram b v m
withNames ns f d = maybe id f (T.sequence (map ((listToMaybe=<<) . ($nd) . lookupSub . toName) ns)) d
  where nd = subMap d

-- | Get the query function associated with a diagram.
query :: Monoid m => QDiagram b v m -> Query v m
query = getU' . unQD

-- | Sample a diagram's query function at a given point.
sample :: Monoid m => QDiagram b v m -> Point v -> m
sample = runQuery . query

-- | Set the query value for 'True' points in a diagram (/i.e./ points
--   \"inside\" the diagram); 'False' points will be set to 'mempty'.
value :: Monoid m => m -> QDiagram b v Any -> QDiagram b v m
value m = fmap fromAny
  where fromAny (Any True)  = m
        fromAny (Any False) = mempty

-- | Reset the query values of a diagram to @True@/@False@: any values
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
--   trace, subdiagram map, and query function.
mkQD :: Prim b v -> Envelope v -> Trace v -> SubMap b v m -> Query v m -> QDiagram b v m
mkQD p e t n q = QD $ D.leaf (toDeletable e *: toDeletable t *: n *: q *: ()) p

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
instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
  => Monoid (QDiagram b v m) where
  mempty  = QD D.empty
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

instance Functor (QDiagram b v) where
  fmap f = (over QD . D.mapU . second . second)
             ( (first . fmap . fmap) f
             . (second . first . fmap . fmap) f
             )

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

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
      => HasStyle (QDiagram b v m) where
  applyStyle = over QD . D.applyD . inj
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
freeze :: forall v b m. (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
       => QDiagram b v m -> QDiagram b v m
freeze = over QD . D.applyD . inj
       . (inL :: Split (Transformation v) -> Split (Transformation v) :+: Style v)
       $ split

---- Juxtaposable

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
      => Juxtaposable (QDiagram b v m) where
  juxtapose = juxtaposeDefault

---- Enveloped

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v) )
         => Enveloped (QDiagram b v m) where
  getEnvelope = envelope

---- Traced

instance (HasLinearMap v, VectorSpace v, Ord (Scalar v))
         => Traced (QDiagram b v m) where
  getTrace = trace

---- HasOrigin

-- | Every diagram has an intrinsic \"local origin\" which is the
--   basis for all combining operations.
instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
      => HasOrigin (QDiagram b v m) where

  moveOriginTo = translate . (origin .-.)

---- Transformable

-- | Diagrams can be transformed by transforming each of their
--   components appropriately.
instance (HasLinearMap v, OrderedField (Scalar v), InnerSpace v, Semigroup m)
      => Transformable (QDiagram b v m) where
  transform = over QD . D.applyD . transfToAnnot

---- Qualifiable

-- | Diagrams can be qualified so that all their named points can
--   now be referred to using the qualification prefix.
instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
      => Qualifiable (QDiagram b v m) where
  (|>) = over QD . D.applyD . inj . toName


------------------------------------------------------------
--  Subdiagrams
------------------------------------------------------------

-- | A @Subdiagram@ represents a diagram embedded within the context
--   of a larger diagram.  Essentially, it consists of a diagram
--   paired with any accumulated information from the larger context
--   (transformations, attributes, etc.).

data Subdiagram b v m = Subdiagram (QDiagram b v m) (DownAnnots v)

type instance V (Subdiagram b v m) = v

-- | Turn a diagram into a subdiagram with no accumulated context.
mkSubdiagram :: QDiagram b v m -> Subdiagram b v m
mkSubdiagram d = Subdiagram d empty

-- | Create a \"point subdiagram\", that is, a 'pointDiagram' (with no
--   content and a point envelope) treated as a subdiagram with local
--   origin at the given point.  Note this is not the same as
--   @mkSubdiagram . pointDiagram@, which would result in a subdiagram
--   with local origin at the parent origin, rather than at the given
--   point.
subPoint :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
         => Point v -> Subdiagram b v m
subPoint p = Subdiagram
               (pointDiagram origin)
               (transfToAnnot $ translation (p .-. origin))

instance Functor (Subdiagram b v) where
  fmap f (Subdiagram d a) = Subdiagram (fmap f d) a

instance (OrderedField (Scalar v), InnerSpace v, HasLinearMap v)
      => Enveloped (Subdiagram b v m) where
  getEnvelope (Subdiagram d a) = transform (transfFromAnnot a) $ getEnvelope d

instance (Ord (Scalar v), VectorSpace v, HasLinearMap v)
      => Traced (Subdiagram b v m) where
  getTrace (Subdiagram d a) = transform (transfFromAnnot a) $ getTrace d

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
      => HasOrigin (Subdiagram b v m) where
  moveOriginTo = translate . (origin .-.)

instance ( HasLinearMap v, InnerSpace v, Floating (Scalar v))
    => Transformable (Subdiagram b v m) where
  transform t (Subdiagram d a) = Subdiagram d (transfToAnnot t <> a)

-- | Get the location of a subdiagram; that is, the location of its
--   local origin /with respect to/ the vector space of its parent
--   diagram.  In other words, the point where its local origin
--   \"ended up\".
location :: HasLinearMap v => Subdiagram b v m -> Point v
location (Subdiagram _ a) = transform (transfFromAnnot a) origin

-- | Turn a subdiagram into a normal diagram, including the enclosing
--   context.  Concretely, a subdiagram is a pair of (1) a diagram and
--   (2) a \"context\" consisting of an extra transformation and
--   attributes.  @getSub@ simply applies the transformation and
--   attributes to the diagram to get the corresponding \"top-level\"
--   diagram.
getSub :: ( HasLinearMap v, InnerSpace v
          , Floating (Scalar v), Ord (Scalar v)
          , Semigroup m
          )
       => Subdiagram b v m -> QDiagram b v m
getSub (Subdiagram d a) = over QD (D.applyD a) d

-- | Extract the \"raw\" content of a subdiagram, by throwing away the
--   context.
rawSub :: Subdiagram b v m -> QDiagram b v m
rawSub (Subdiagram d _) = d

------------------------------------------------------------
--  Subdiagram maps  ---------------------------------------
------------------------------------------------------------

-- | A 'SubMap' is a map associating names to subdiagrams. There can
--   be multiple associations for any given name.
newtype SubMap b v m = SubMap (M.Map Name [Subdiagram b v m])
  -- See Note [SubMap Set vs list]

instance Newtype (SubMap b v m) (M.Map Name [Subdiagram b v m]) where
  pack              = SubMap
  unpack (SubMap m) = m

-- ~~~~ [SubMap Set vs list]
-- In some sense it would be nicer to use
-- Sets instead of a list, but then we would have to put Ord
-- constraints on v everywhere. =P

type instance V (SubMap b v m) = v

instance Functor (SubMap b v) where
  fmap = over SubMap . fmap . map . fmap

instance Semigroup (SubMap b v m) where
  SubMap s1 <> SubMap s2 = SubMap $ M.unionWith (++) s1 s2

-- | 'SubMap's form a monoid with the empty map as the identity, and
--   map union as the binary operation.  No information is ever lost:
--   if two maps have the same name in their domain, the resulting map
--   will associate that name to the concatenation of the information
--   associated with that name.
instance Monoid (SubMap b v m) where
  mempty  = SubMap M.empty
  mappend = (<>)

instance (OrderedField (Scalar v), InnerSpace v, HasLinearMap v)
      => HasOrigin (SubMap b v m) where
  moveOriginTo = over SubMap . moveOriginTo

instance (InnerSpace v, Floating (Scalar v), HasLinearMap v)
  => Transformable (SubMap b v m) where
  transform = over SubMap . transform

-- | 'SubMap's are qualifiable: if @ns@ is a 'SubMap', then @a |>
--   ns@ is the same 'SubMap' except with every name qualified by
--   @a@.
instance Qualifiable (SubMap b v m) where
  a |> (SubMap m) = SubMap $ M.mapKeys (a |>) m

-- | Construct a 'SubMap' from a list of associations between names
--   and subdiagrams.
fromNames :: IsName a => [(a, Subdiagram b v m)] -> SubMap b v m
fromNames = SubMap . M.fromListWith (++) . map (toName *** (:[]))

-- | Add a name/diagram association to a submap.
rememberAs :: IsName a => a -> QDiagram b v m -> SubMap b v m -> SubMap b v m
rememberAs n b = over SubMap $ M.insertWith (++) (toName n) [mkSubdiagram b]

-- | A name acts on a name map by qualifying every name in it.
instance Action Name (SubMap b v m) where
  act = (|>)

-- | Names don't act on anything else.
instance Action Name a

-- | Look for the given name in a name map, returning a list of
--   subdiagrams associated with that name.  If no names match the
--   given name exactly, return all the subdiagrams associated with
--   names of which the given name is a suffix.
lookupSub :: IsName n => n -> SubMap b v m -> Maybe [Subdiagram b v m]
lookupSub a (SubMap m)
  = M.lookup n m `mplus`
    (flatten . filter ((n `nameSuffixOf`) . fst) . M.assocs $ m)
  where (Name n1) `nameSuffixOf` (Name n2) = n1 `isSuffixOf` n2
        flatten [] = Nothing
        flatten xs = Just . concatMap snd $ xs
        n = toName a

------------------------------------------------------------
--  Primitives  --------------------------------------------
------------------------------------------------------------

-- $prim
-- Ultimately, every diagram is essentially a list of /primitives/,
-- basic building blocks which can be rendered by backends.  However,
-- not every backend must be able to render every type of primitive;
-- the collection of primitives a given backend knows how to render is
-- determined by instances of 'Renderable'.

-- | A value of type @Prim b v@ is an opaque (existentially quantified)
--   primitive which backend @b@ knows how to render in vector space @v@.
data Prim b v where
  Prim :: Renderable p b => p -> Prim b (V p)

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
--   width of an image (this example requires @diagrams-lib@):
--
--   > ghci> width (image "foo.png" 200 200)
--   >
--   > <interactive>:8:8:
--   >     No instance for (Renderable Diagrams.TwoD.Image.Image b0)
--   >       arising from a use of `image'
--   >     Possible fix:
--   >       add an instance declaration for
--   >       (Renderable Diagrams.TwoD.Image.Image b0)
--   >     In the first argument of `width', namely
--   >       `(image "foo.png" 200 200)'
--   >     In the expression: width (image "foo.png" 200 200)
--   >     In an equation for `it': it = width (image "foo.png" 200 200)
--
--   GHC complains that there is no instance for @Renderable Image
--   b0@; what is really going on is that it does not have enough
--   information to decide what backend to use (hence the
--   uninstantiated @b0@). This is annoying because /we/ know that the
--   choice of backend cannot possibly affect the width of the image
--   (it's 200! it's right there in the code!); /but/ there is no way
--   for GHC to know that.
--
--   The solution is to annotate the call to 'image' with the type
--   @'D' 'R2'@, like so:
--
--   > ghci> width (image "foo.png" 200 200 :: D R2)
--   > 200.00000000000006
--
--   (It turns out the width wasn't 200 after all...)
--
--   As another example, here is the error we get if we try to compute
--   the width of a radius-1 circle:
--
--   > ghci> width (circle 1)
--   >
--   > <interactive>:4:1:
--   >     Couldn't match type `V a0' with `R2'
--   >     In the expression: width (circle 1)
--   >     In an equation for `it': it = width (circle 1)
--
--   There's even more ambiguity here.  Whereas 'image' always returns
--   a 'Diagram', the 'circle' function can produce any 'PathLike'
--   type, and the 'width' function can consume any 'Enveloped' type,
--   so GHC has no idea what type to pick to go in the middle.
--   However, the solution is the same:
--
--  > ghci> width (circle 1 :: D R2)
--  > 1.9999999999999998

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
  renderDias :: (InnerSpace v, OrderedField (Scalar v), Monoid' m)
             => b -> Options b v -> [QDiagram b v m] -> Result b v

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
