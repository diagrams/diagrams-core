{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
-- We have some orphan Action instances here, but since Action is a multi-param
-- class there is really no better place to put them.

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Types
-- Copyright   :  (c) 2011-2013 diagrams-core team (see LICENSE)
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
         Annotation(Href, OpacityGroup)
       , applyAnnotation, href, opacityGroup, groupOpacity

         -- *** Dynamic (monoidal) annotations
       , Summary, Context

         -- ** Basic type definitions
       , QDiagram(..), Diagram

         -- * Operations on diagrams
         -- ** Creating diagrams
       , leafS
       , mkQD, pointDiagram

         -- ** Extracting information
       , envelope, trace, query, sample
       , value, resetValue, clearValue

         -- ** Combining diagrams

         -- | For many more ways of combining diagrams, see
         -- "Diagrams.Combinators" from the diagrams-lib package.

       , atop

         -- *** Other
       , setEnvelope
       , setTrace

         -- * Subdiagrams

       , Subdiagram(..), mkSubdiagram
       --, getSub, rawSub
       --, location
       , subPoint

         -- * Subdiagram maps

       , SubMap(..)

       , fromNames, lookupSub

         -- * Primtives
         -- $prim

       , Prim(..)

         -- * Backends

       , Backend(..)
       , RNode(..)
       , RTree, emptyRTree

         -- ** Null backend

       , NullBackend, D

         -- ** Number classes
       , TypeableFloat

         -- * Renderable

       , Renderable(..)

       ) where

import           Control.Arrow             (first, second, (***))
import           Control.Lens              (Lens', Rewrapped, Wrapped (..), iso, lens, over, view,
                                            review, (^.), _Wrapped, _Wrapping, op)
import           Control.Monad             (mplus)
import           Data.Typeable
import           Data.Functor              ((<$>))
import           Data.List                 (isSuffixOf)
import qualified Data.List.NonEmpty        as NEL
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Semigroup
import qualified Data.Traversable          as T
import           Data.Tree

import           Data.Monoid.Action
import           Data.Monoid.Coproduct
import           Data.Monoid.Deletable
import           Data.Monoid.MList
import           Data.Monoid.WithSemigroup
import qualified Data.Tree.DUAL            as D

import           Diagrams.Core.Context
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

-- | Class of numbers that are 'RealFloat' and 'Typeable'. This class is used to
--   shorten type constraints.
class (Typeable n, RealFloat n) => TypeableFloat n
instance (Typeable n, RealFloat n) => TypeableFloat n
-- use class instead of type constraint so users don't need constraint kinds pragma

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

type Summary b v n m = Deletable (Envelope v n)
                   ::: Deletable (Trace v n)
                   ::: Query v n m
                   ::: ()

newtype QDiagram b v n m = QD (Contextual v n (RTree b v n Annotation, Summary b v n m))
#if __GLASGOW_HASKELL__ >= 707
  deriving Typeable
#else

instance forall b v. (Typeable b, Typeable1 v) => Typeable2 (QDiagram b v n) where
  typeOf2 _ = mkTyConApp (mkTyCon3 "diagrams-core" "Diagrams.Core.Types" "QDiagram") [] `mkAppTy`
              typeOf (undefined :: b)                                                   `mkAppTy`
              typeOf1 (undefined :: v n)
#endif

instance Wrapped (QDiagram b v n m) where
  type Unwrapped (QDiagram b v n m) =
    Contextual v n (RTree b v n Annotation, Summary b v n m)
  _Wrapped' = iso (\(QD d) -> d) QD

instance Rewrapped (QDiagram b v n m) (QDiagram b' v' n' m')

type instance V (QDiagram b v n m) = v
type instance N (QDiagram b v n m) = n

type Diagram b = QDiagram b (V b) (N b) Any

-- | XXX Add comment
(>>>=) :: Contextual v n a -> (a -> QDiagram b v n m) -> QDiagram b v n m
ca >>>= k = QD $ ca >>= \a -> (op QD) (k a)

-- | XXX Add comment
(=<<<) :: (a -> QDiagram b v n m) -> Contextual v n a -> QDiagram b v n m
(=<<<) = flip (>>>=)

-- | Create a \"leaf\" diagram with just a summary value and no
--   diagrammatic content.
leafS :: Summary b v n m -> QDiagram b v n m
leafS s = QD $ return (emptyRTree, s)

-- | Project out a component of the summary in a type-directed way.
getS :: (Monoid u, Summary b v n m :>: u) => QDiagram b v n m -> Contextual v n u
getS = fmap (option mempty id . get . snd) . op QD

applySpre :: (Semigroup m, Ord n) => Summary b v n m -> QDiagram b v n m -> QDiagram b v n m
applySpre = over _Wrapped . applySpre'

applySpre' :: (Functor f, Semigroup s) => s -> f (t,s) -> f (t,s)
applySpre' s = (fmap . second) (s<>)

applySpost :: (Semigroup m, Ord n) => Summary b v n m -> QDiagram b v n m -> QDiagram b v n m
applySpost = over _Wrapped . applySpost'

applySpost' :: (Functor f, Semigroup s) => s -> f (t,s) -> f (t,s)
applySpost' s = (fmap . second) (<>s)

-- | Static annotations which can be placed at a particular node of a
--   diagram tree.
data Annotation
  = Href String    -- ^ Hyperlink
  | OpacityGroup Double
  deriving Show

-- | Apply a static annotation at the root of a diagram.
applyAnnotation
  :: (Metric v, OrderedField n, Semigroup m)
  => Annotation -> QDiagram b v n m -> QDiagram b v n m
applyAnnotation an = over _Wrapped . fmap . first . over _Wrapped $ Node (RAnnot an) . (: [])

-- | Make a diagram into a hyperlink.  Note that only some backends
--   will honor hyperlink annotations.
href :: (Metric v, OrderedField n, Semigroup m)
  => String -> QDiagram b v n m -> QDiagram b v n m
href = applyAnnotation . Href

-- | Create a \"point diagram\", which has no content, no trace, an
--   empty query, and a point envelope.
pointDiagram :: (Metric v, Fractional n)
             => Point v n -> QDiagram b v n m
pointDiagram p = leafS (inj . toDeletable $ pointEnvelope p)

-- | Get the envelope of a diagram.
envelope :: forall b v n m. (OrderedField n, Metric v, Monoid' m)
         => Lens' (QDiagram b v n m) (Contextual v n (Envelope v n))
envelope = lens (fmap unDelete . getS)  ((=<<<) . flip setEnvelope)

-- | Replace the envelope of a diagram.
setEnvelope :: forall b v m n. (OrderedField n, Metric v, Monoid' m)
          => Envelope v n -> QDiagram b v n m -> QDiagram b v n m
setEnvelope e
  = applySpre (inj . toDeletable $ e)
  . applySpre (inj (deleteL :: Deletable (Envelope v n)))
  . applySpost (inj (deleteR :: Deletable (Envelope v n)))
 
-- | Get the trace of a diagram.
trace :: (Metric v, OrderedField n, Semigroup m) =>
         Lens' (QDiagram b v n m) (Contextual v n (Trace v n))
trace = lens (fmap unDelete . getS) ((=<<<) . flip setTrace)

-- | Replace the trace of a diagram.
setTrace :: forall b v n m. (OrderedField n, Metric v, Semigroup m)
         => Trace v n -> QDiagram b v n m -> QDiagram b v n m
setTrace t
  = applySpre (inj . toDeletable $ t)
  . applySpre (inj (deleteL :: Deletable (Trace v n)))
  . applySpost (inj (deleteR :: Deletable (Trace v n)))

-- | Get the query function associated with a diagram.
query :: Monoid m => QDiagram b v n m -> Contextual v n (Query v n m)
query = getS

-- | Sample a diagram's query function at a given point.
sample :: Monoid m => QDiagram b v n m -> Point v n -> Contextual v n m
sample d p = flip runQuery p <$> query d

-- | Set the query value for 'True' points in a diagram (/i.e./ points
--   \"inside\" the diagram); 'False' points will be set to 'mempty'.
value :: Monoid m => m -> QDiagram b v n Any -> QDiagram b v n m
value m = fmap fromAny
  where fromAny (Any True)  = m
        fromAny (Any False) = mempty

-- | Reset the query values of a diagram to @True@/@False@: any values
--   equal to 'mempty' are set to 'False'; any other values are set to
--   'True'.
resetValue :: (Eq m, Monoid m) => QDiagram b v n m -> QDiagram b v n Any
resetValue = fmap toAny
  where toAny m | m == mempty = Any False
                | otherwise   = Any True

-- | Set all the query values of a diagram to 'False'.
clearValue :: QDiagram b v n m -> QDiagram b v n Any
clearValue = fmap (const (Any False))

-- | Create a diagram from a single primitive, along with an envelope,
--   trace, subdiagram map, and query function.
mkQD :: Prim b v n -> Envelope v n -> Trace v n -> Query v n m
     -> QDiagram b v n m
mkQD p e t q = QD . review _Wrapped' 
               $ const (RTree (Node (RPrim p) [])
               , toDeletable e *: toDeletable t *: q *: ())

-- | Change the transparency of a 'Diagram' as a group.
opacityGroup, groupOpacity :: (Metric v, OrderedField n, Semigroup m)
  => Double -> QDiagram b v n m -> QDiagram b v n m
opacityGroup = applyAnnotation . OpacityGroup
groupOpacity = applyAnnotation . OpacityGroup


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
  mempty  = QD (return (emptyRTree, mempty))
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
--
instance Functor (QDiagram b v n) where
  fmap = over (_Wrapping QD) . fmap . second . fmap . second . first . fmap . fmap

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
--
instance (Metric v, OrderedField n, Semigroup m)
      => HasStyle (QDiagram b v n m) where
  applyStyle s = (over (_Wrapping QD) . fmap . first 
               . over (_Wrapping RTree)) (\t -> (Node (RStyle s) [t]))

---- Juxtaposable

instance (Metric v, OrderedField n, Monoid' m)
      => Juxtaposable (QDiagram b v n m) where
  juxtapose = juxtaposeDefault

---- Enveloped

instance (Metric v, OrderedField n, Monoid' m)
         => Enveloped (QDiagram b v n m) where
  getEnvelope = view envelope

 -- XXX hmm, this is a real problem.  We *can't* just return an
  -- Envelope for a diagram, we can only return a Contextual Envelope.
  -- Maybe the type of getEnvelope needs to change??
  --
  -- Note, simply changing the type of getEnvelope to Contextual would
  -- (unsurprisingly, perhaps) introduce an import loop, since
  -- Contextual is defined in Diagrams.Core.Types which imports
  -- Diagrams.Core.Envelope.  However, it looks like we could split
  -- the Context type and Contextual monad out into a new module;
  -- Context itself does not mention Envelope, only Summary.
  
---- Traced

instance (Metric v, OrderedField n, Semigroup m)
         => Traced (QDiagram b v n m) where
  getTrace = undefined --view trace

-- XXX of course Traced has the same problem as Enveloped, described
-- above

---- HasOrigin

-- | Every diagram has an intrinsic \"local origin\" which is the
--   basis for all combining operations.
instance (Metric v, OrderedField n, Semigroup m)
      => HasOrigin (QDiagram b v n m) where
  moveOriginTo = translate . (origin .-.)

---- Transformable

-- | Diagrams can be transformed by transforming each of their
--   components appropriately.
instance (Metric v, OrderedField n, Semigroup m)
      => Transformable (QDiagram b v n m) where
  transform = undefined -- over _Wrapped' . transform

  --- XXX not sure why (over _Wrapped' . transform) doesn't type check.
  --
  -- Yields the error "Could not deduce (v ~ V ())".  Not sure where
  -- the () is coming from.  Ah, I think it's coming from Summary,
  -- perhaps?  Anyway, I am no longer convinced this implementation
  -- even makes sense... Need to think about it more carefully.  How
  -- to transform a diagram under the new fixpoint semantics scheme?

---- Qualifiable

-- | Diagrams can be qualified so that all their named points can
--   now be referred to using the qualification prefix.
-- instance (Metric v, OrderedField n, Semigroup m)
      -- => Qualifiable (QDiagram b v n m) where
  -- (|>) = over _Wrapped' . D.applyD . inj . toName


------------------------------------------------------------
--  Subdiagrams
------------------------------------------------------------

-- | A @Subdiagram@ represents a diagram embedded within the context
--   of a larger diagram.  Essentially, it consists of a diagram
--   paired with any accumulated information from the larger context
--   (transformations, attributes, etc.).

data Subdiagram b v n m = Subdiagram (QDiagram b v n m) (Transformation v n) (Context v n)

type instance V (Subdiagram b v n m) = v
type instance N (Subdiagram b v n m) = n

-- | Turn a diagram into a subdiagram with no accumulated context.
mkSubdiagram :: QDiagram b v n m -> Subdiagram b v n m
mkSubdiagram d = undefined -- Subdiagram d empty empty

-- | Create a \"point subdiagram\", that is, a 'pointDiagram' (with no
--   content and a point envelope) treated as a subdiagram with local
--   origin at the given point.  Note this is not the same as
--   @mkSubdiagram . pointDiagram@, which would result in a subdiagram
--   with local origin at the parent origin, rather than at the given
--   point.
subPoint :: (Metric v, OrderedField n, Semigroup m)
         => Point v n -> Subdiagram b v n m
subPoint p = Subdiagram
               (pointDiagram origin)
               (translation (p .-. origin))
               empty

instance Functor (Subdiagram b v n) where
  fmap f (Subdiagram d t c) = Subdiagram (fmap f d) t c

-- instance (OrderedField n, Metric v, Monoid' m)
      -- => Enveloped (Subdiagram b v n m) where
  -- getEnvelope (Subdiagram d a) = transform (transfFromAnnot a) $ getEnvelope d

-- instance (OrderedField n, Metric v, Semigroup m)
      -- => Traced (Subdiagram b v n m) where
  -- getTrace (Subdiagram d a) = transform (transfFromAnnot a) $ getTrace d

instance (Metric v, OrderedField n)
      => HasOrigin (Subdiagram b v n m) where
  moveOriginTo = translate . (origin .-.)
  
instance (Metric v, Floating n)
    => Transformable (Subdiagram b v n m) where
  transform t' (Subdiagram d t c) = Subdiagram d (t' <> t) c

-- | Get the location of a subdiagram; that is, the location of its
--   local origin /with respect to/ the vector space of its parent
--   diagram.  In other words, the point where its local origin
--   \"ended up\".
-- location :: (Additive v, Num n) => Subdiagram b v n m -> Point v n
-- location (Subdiagram _ a) = transform (transfFromAnnot a) origin

-- | Turn a subdiagram into a normal diagram, including the enclosing
--   context.  Concretely, a subdiagram is a pair of (1) a diagram and
--   (2) a \"context\" consisting of an extra transformation and
--   attributes.  @getSub@ simply applies the transformation and
--   attributes to the diagram to get the corresponding \"top-level\"
--   diagram.
-- getSub :: ( Metric v
          -- , Floating n, Ord n
          -- , Semigroup m
          -- )
       -- => Subdiagram b v n m -> QDiagram b v n m
-- getSub (Subdiagram d a) = over _Wrapped' (D.applyD a) d

-- | Extract the \"raw\" content of a subdiagram, by throwing away the
--   context.
-- rawSub :: Subdiagram b v n m -> QDiagram b v n m
-- rawSub (Subdiagram d _) = d

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

instance (Metric v, Floating n)
  => Transformable (SubMap b v n m) where
  transform = over _Wrapped' . transform

-- | 'SubMap's are qualifiable: if @ns@ is a 'SubMap', then @a |>
--   ns@ is the same 'SubMap' except with every name qualified by
--   @a@.
instance Qualifiable (SubMap b v n m) where
  a |> (SubMap m) = SubMap $ M.mapKeys (a |>) m

-- | Construct a 'SubMap' from a list of associations between names
--   and subdiagrams.
fromNames :: IsName a => [(a, Subdiagram b v n m)] -> SubMap b v n m
fromNames = SubMap . M.fromListWith (++) . map (toName *** (:[]))

-- | A name acts on a name map by qualifying every name in it.
instance Action Name (SubMap b v n m) where
  act = (|>)

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

data RNode b v n a = RStyle (Style v n) -- ^ A style node.
                   | RAnnot a
                   | RPrim (Prim b v n) -- ^ A primitive.
                   | REmpty

-- | An 'RTree' is a compiled and optimized representation of a
--   'QDiagram', which can be used by backends.  They have the
--   following invariant which backends may rely upon:
--
--   * @RPrim@ nodes never have any children.
newtype RTree b v n a = RTree (Tree (RNode b v n a ))

instance Wrapped (RTree b v n a) where
  type Unwrapped (RTree b v n a) = Tree (RNode b v n a)
  _Wrapped' = iso (\(RTree t) -> t) RTree

instance Rewrapped (RTree b v n a) (RTree b' v' n' a')

type instance V (RTree b v n a) = v
type instance N (RTree b v n a) = n

instance Semigroup (RTree b v n a) where
  RTree t1 <> RTree t2 = RTree (Node REmpty[t1,t2])
  sconcat ts = RTree (Node REmpty . map (op RTree) . NEL.toList $ ts)

-- | The empty @RTree@.
emptyRTree :: RTree b v n a
emptyRTree = RTree (Node REmpty [])

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
--   a 'Diagram', the 'circle' function can produce any 'PathLike'
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

instance Monoid (Render NullBackend v n) where
  mempty      = NullBackendRender
  mappend _ _ = NullBackendRender

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
