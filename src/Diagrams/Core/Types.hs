{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
         Annotation(Href)
       , applyAnnotation, href

         -- *** Dynamic (monoidal) annotations
       , Summary, Context

         -- ** Basic type definitions
       , QDiagram(..), Diagram

         -- * Operations on diagrams
         -- ** Creating diagrams
       , leafS
--       , mkQD, mkQD', pointDiagram

         -- ** Extracting information
--       , envelope, trace, query, sample
--       , value, resetValue, clearValue

         -- ** Combining diagrams

         -- | For many more ways of combining diagrams, see
         -- "Diagrams.Combinators" from the diagrams-lib package.

       , atop

         -- ** Modifying diagrams
       , setEnvelope
       , setTrace

         -- * Measurements
       , Measure(..)
       , fromOutput
       , atMost, atLeast

         -- * Subdiagrams

       , Subdiagram(..), mkSubdiagram
--       , getSub, rawSub
--       , location
       , subPoint

         -- * Subdiagram maps

       , SubMap(..)

         -- * Primtives
         -- $prim

       , Prim(..)

         -- * Backends

       , Backend(..)
       , RNode(..)
       , RTree, emptyRTree

         -- ** Null backend

       , NullBackend, D

         -- * Renderable

       , Renderable(..)

       ) where

import           Control.Applicative       (Applicative)
import           Control.Arrow             (first, second, (***))
import           Control.Lens              (Lens', Rewrapped, Wrapped (..), iso,
                                            lens, over, view, (^.), _Wrapped,
                                            _Wrapping, Setter', sets, Ixed(..),
                                            At(..), Index, IxValue, Contains(..),
                                            (&), (.~), Traversal', review, op)
import           Control.Monad             (mplus)
import           Control.Monad.Reader
import           Data.AffineSpace          ((.-.))
import           Data.Data
import           Data.Functor              ((<$>))
import           Data.Functor.Identity
import           Data.List                 (isSuffixOf)
import qualified Data.List.NonEmpty        as NEL
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Semigroup
import qualified Data.Set                  as S
import           Data.Set.Lens             (setmapped)
import qualified Data.Traversable          as T
import           Data.Tree
import           Data.Tree.Lens            (branches)
import           Data.VectorSpace

import           Data.Monoid.Action
import           Data.Monoid.Coproduct
import           Data.Monoid.Deletable
import           Data.Monoid.MList
import           Data.Monoid.WithSemigroup

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

-- XXX TODO: add lots of actual diagrams to illustrate the
-- documentation!  Haddock supports \<\<inline image urls\>\>.

------------------------------------------------------------
--  Measurement Units  -------------------------------------
------------------------------------------------------------
-- | Type of measurement units for attributes.
data Measure v = Output (Scalar v)
               | Normalized (Scalar v)
               | Local (Scalar v)
               | Global (Scalar v)

               | MinM (Measure v) (Measure v)
               | MaxM (Measure v) (Measure v)
               | ZeroM
               | NegateM (Measure v)
               | PlusM (Measure v) (Measure v)
               | ScaleM (Scalar v) (Measure v)
  deriving (Typeable)

deriving instance (Eq (Scalar v)) => Eq (Measure v)
deriving instance (Ord (Scalar v)) => Ord (Measure v)
deriving instance (Show (Scalar v)) => Show (Measure v)
deriving instance (Typeable v, Data v, Data (Scalar v)) => Data (Measure v)

-- | Compute the larger of two 'Measure's.  Useful for setting lower
--   bounds.
atLeast :: Measure v -> Measure v -> Measure v
atLeast = MaxM

-- | Compute the smaller of two 'Measure's.  Useful for setting upper
--   bounds.
atMost :: Measure v -> Measure v -> Measure v
atMost = MinM

instance AdditiveGroup (Measure v) where
  zeroV = ZeroM
  negateV (NegateM m) = m
  negateV m = NegateM m
  ZeroM ^+^ m = m
  m ^+^ ZeroM = m
  m1 ^+^ m2 = PlusM m1 m2

instance VectorSpace (Measure v) where
  type Scalar (Measure v) = Scalar v
  s *^ m = ScaleM s m

type instance V (Measure v) = v

instance (HasLinearMap v, Floating (Scalar v)) => Transformable (Measure v) where
  transform tr (Local x) = Local (avgScale tr * x)
  transform tr (MinM m1 m2) = MinM (transform tr m1) (transform tr m2)
  transform tr (MaxM m1 m2) = MaxM (transform tr m1) (transform tr m2)
  transform tr (NegateM m') = NegateM (transform tr m')
  transform tr (PlusM m1 m2) = PlusM (transform tr m1) (transform tr m2)
  transform tr (ScaleM s m') = ScaleM s (transform tr m')
  transform _ y = y

-- | Retrieve the 'Output' value of a 'Measure v' or throw an exception.
--   Only 'Ouput' measures should be left in the 'RTree' passed to the backend.
fromOutput :: Measure v -> Scalar v
fromOutput (Output w)     = w
fromOutput (Normalized _) = fromOutputErr "Normalized"
fromOutput (Local _)      = fromOutputErr "Local"
fromOutput (Global _)     = fromOutputErr "Global"
fromOutput (MinM _ _)     = fromOutputErr "MinM"
fromOutput (MaxM _ _)     = fromOutputErr "MaxM"
fromOutput (ZeroM)        = fromOutputErr "ZeroM"
fromOutput (NegateM _)    = fromOutputErr "NegateM"
fromOutput (PlusM _ _)    = fromOutputErr "PlusM"
fromOutput (ScaleM _ _)   = fromOutputErr "ScaleM"

fromOutputErr :: String -> a
fromOutputErr s = error $ "fromOutput: Cannot pass " ++ s ++ " to backends, must be Output."

--   Eventually we may use a GADT like:
--
--     data Measure o v where
--       Output     :: Scalar v -> Measure O v
--       Normalized :: Scalar v -> Measure A v
--       Global     :: Scalar v -> Measure A v
--       Local      :: Scale v  -> Measure A v
--
--   to check this at compile time. But for now we throw a runtime error.
--
--   [BAY 4 April 2014] I tried switching to such a GADT.  One tricky
--   bit is that you have to use Output :: Scalar v -> Measure o v,
--   not Measure O v: the reason is that operations like addition have
--   to take two values of the same type, so in order to be able to
--   add Output to something else, Output must be able to have an A
--   annotation.  That all works fine.  The problem is with gmapAttrs,
--   which has to preserve type: so we can't generically convert from
--   Measure A to Measure O.


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
type Summary b v m = Deletable (Envelope v)
                 ::: Deletable (Trace v)
                 ::: Query v m
                 ::: ()

-- | The (monoidal) context in which a diagram is interpreted.
--   Contexts can be thought of as accumulating along each path to a
--   leaf:
--
--   * styles (see "Diagrams.Core.Style")
type Context b v m = Style v
                 ::: Environment b v a
                 ::: ()

--------------------------------------------------
-- Context monad

{-
newtype Contextual (Context b v) a = Contextual (Reader (Context v) a)
  deriving (Functor, Applicative, Monad, MonadReader (Context v))

instance Semigroup a => Semigroup (Contextual (Context b v) a) where
  Contextual r1 <> Contextual r2 = Contextual . reader $ \ctx -> runReader r1 ctx <> runReader r2 ctx

instance (Semigroup a, Monoid a) => Monoid (Contextual (Context b v) a) where
  mappend = (<>)
  mempty  = Contextual . reader $ const mempty
-}

-- Environments

newtype Environment b v a = Environment { getEnvironment :: [([RTree b v a],[RTree b v a])] }
  deriving (Semigroup, Monoid)

stepRight, stepLeft :: RTree b v a -> Environment b v a
stepRight t = Environment [([t],[])]
stepLeft  t = Environment [([],[t])]

above, below :: Environment b v a -> RTree b v a
above = foldMap (mconcat . fst) ts . getEnvironment
below = foldMap (mconcat . snd) ts . getEnvironment

--------------------------------------------------
-- QDiagram

-- | The fundamental diagram type is represented by trees of
--   primitives with various monoidal annotations.  The @Q@ in
--   @QDiagram@ stands for \"Queriable\", as distinguished from
--   'Diagram', a synonym for @QDiagram@ with the query type
--   specialized to 'Any'.
newtype QDiagram b v m = QD (Contextual (Context b v) (RTree b v, Summary b v m))
  deriving (Typeable)

instance Wrapped (QDiagram b v m) where
  type Unwrapped (QDiagram b v m) = Contextual (Context b v) (RTree b v, Summary b v m)
  _Wrapped' = iso (\(QD d) -> d) QD

instance Rewrapped (QDiagram b v m) (QDiagram b' v' m')

type instance V (QDiagram b v m) = v

-- | The default sort of diagram is one where querying at a point
--   simply tells you whether the diagram contains that point or not.
--   Transforming a default diagram into one with a more interesting
--   query can be done via the 'Functor' instance of @'QDiagram' b@ or
--   the 'value' function.
type Diagram b v = QDiagram b v Any

-- | XXX comment me
(>>>=) :: Contextual (Context b v) a -> (a -> QDiagram b v m) -> QDiagram b v m
ca >>>= k = QD $ ca >>= \a -> (op QD) (k a)

-- | XXX comment me
(=<<<) :: (a -> QDiagram b v m) ->  Contextual (Context b v) a -> QDiagram b v m
(=<<<) = flip (>>>=)

-- | Create a \"leaf\" diagram with just a summary value and no
--   diagrammatic content.
leafS :: Summary b v m -> QDiagram b v m
leafS s = QD $ return (emptyRTree, s)

-- | Project out a component of the summary in a type-directed way.
getS :: (Monoid u, Summary b v m :>: u) => QDiagram b v m -> Contextual (Context b v) u
getS = fmap (option mempty id . get . snd) . op QD

applySpre :: (Semigroup m, Ord (Scalar v)) => Summary b v m -> QDiagram b v m -> QDiagram b v m
applySpre = over _Wrapped . applySpre'

applySpre' :: (Functor f, Semigroup s) => s -> f (t,s) -> f (t,s)
applySpre' s = (fmap . second) (s<>)

applySpost :: (Semigroup m, Ord (Scalar v)) => Summary b v m -> QDiagram b v m -> QDiagram b v m
applySpost = over _Wrapped . applySpost'

applySpost' :: (Functor f, Semigroup s) => s -> f (t,s) -> f (t,s)
applySpost' s = (fmap . second) (<>s)

-- | Static annotations which can be placed at a particular node of a
--   diagram tree.
data Annotation
  = Href String    -- ^ Hyperlink
  deriving Show

-- | Apply a static annotation at the root of a diagram.
applyAnnotation
  :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
  => Annotation -> QDiagram b v m -> QDiagram b v m
applyAnnotation an = over _Wrapped . fmap . first . over _Wrapped $ Node (RAnnot an) . (: [])

-- | Make a diagram into a hyperlink.  Note that only some backends
--   will honor hyperlink annotations.
href :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m) => String -> QDiagram b v m -> QDiagram b v m
href = applyAnnotation . Href

-- | Create a \"point diagram\", which has no content, no trace, an
--   empty query, and a point envelope.
pointDiagram :: (Fractional (Scalar v), InnerSpace v)
             => Point v -> QDiagram b v m
pointDiagram p = leafS (inj . toDeletable $ pointEnvelope p)

-- | Get the envelope of a diagram.
envelope :: forall b v m. (OrderedField (Scalar v), InnerSpace v
                          , HasLinearMap v, Monoid' m)
         => Lens' (QDiagram b v m) (Contextual (Context b v) (Envelope v))
envelope = lens (fmap unDelete . getS)  ((=<<<) . flip setEnvelope)

-- | Replace the envelope of a diagram.
setEnvelope :: forall b v m. (OrderedField (Scalar v), InnerSpace v
                             , HasLinearMap v, Monoid' m)
          => Envelope v -> QDiagram b v m -> QDiagram b v m
setEnvelope e
  = applySpre (inj . toDeletable $ e)
  . applySpre (inj (deleteL :: Deletable (Envelope v)))
  . applySpost (inj (deleteR :: Deletable (Envelope v)))

-- | Get the trace of a diagram.
trace :: (InnerSpace v, HasLinearMap v, OrderedField (Scalar v), Semigroup m) =>
         Lens' (QDiagram b v m) (Contextual (Context b v) (Trace v))
trace = lens (fmap unDelete . getS) ((=<<<) . flip setTrace)

-- | Replace the trace of a diagram.
setTrace :: forall b v m. (OrderedField (Scalar v), InnerSpace v
                          , HasLinearMap v, Semigroup m)
         => Trace v -> QDiagram b v m -> QDiagram b v m
setTrace t
  = applySpre (inj . toDeletable $ t)
  . applySpre (inj (deleteL :: Deletable (Trace v)))
  . applySpost (inj (deleteR :: Deletable (Trace v)))

-- | Get the query function associated with a diagram.
query :: Monoid m => QDiagram b v m -> Contextual (Context b v) (Query v m)
query = getS

-- | Sample a diagram's query function at a given point.
sample :: Monoid m => QDiagram b v m -> Point v -> Contextual (Context b v) m
sample d p = flip runQuery p <$> query d

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
mkQD :: Prim b v -> Envelope v -> Trace v -> Query v m
     -> QDiagram b v m
mkQD p e t q = QD . review _Wrapped' $ const (RTree (Node (RPrim p) []), toDeletable e *: toDeletable t *: q *: ())

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
  mempty  = QD (return (emptyRTree, mempty))
  mappend = (<>)

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
  => Semigroup (QDiagram b v m) where
  (QD d1) <> (QD d2) = QD $ \c ->
    let (t1,s1) = d1 $ c & val' <>~ stepLeft  t2
        (t2,s2) = d2 $ c & val' <>~ stepRight t1
    in  (t2 <> t1, s2 <> s2)
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
  fmap = over (_Wrapping QD) . fmap . second . fmap . second . first . fmap . fmap

-- ---- Applicative

-- -- XXX what to do with this?
-- -- A diagram with queries of result type @(a -> b)@ can be \"applied\"
-- --   to a diagram with queries of result type @a@, resulting in a
-- --   combined diagram with queries of result type @b@.  In particular,
-- --   all components of the two diagrams are combined as in the
-- --   @Monoid@ instance, except the queries which are combined via
-- --   @(<*>)@.

-- -- instance (Backend b v, s ~ Scalar v, AdditiveGroup s, Ord s)
-- --            => Applicative (QDiagram b v) where
-- --   pure a = Diagram mempty mempty mempty (Query $ const a)

-- --   (Diagram ps1 bs1 ns1 smp1) <*> (Diagram ps2 bs2 ns2 smp2)
-- --     = Diagram (ps1 <> ps2) (bs1 <> bs2) (ns1 <> ns2) (smp1 <*> smp2)

-- ---- HasStyle

-- instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
--       => HasStyle (QDiagram b v m) where
--   applyStyle = over _Wrapped' . D.applyD . inj
--              . (inR :: Style v -> Transformation v :+: Style v)

-- ---- Juxtaposable

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid' m)
      => Juxtaposable (QDiagram b v m) where
  juxtapose = juxtaposeDefault

-- ---- Enveloped

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid' m)
         => Enveloped (QDiagram b v m) where
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

-- ---- Traced

instance (HasLinearMap v, VectorSpace v, Ord (Scalar v), InnerSpace v
         , Semigroup m, Fractional (Scalar v), Floating (Scalar v))
         => Traced (QDiagram b v m) where
  getTrace = undefined  -- view trace
 -- XXX of course Traced has the same problem as Enveloped, described
  -- above

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
  transform t = undefined -- (over _Wrapped' . transform) t
  -- XXX not sure why (over _Wrapped' . transform) doesn't type check

-- ---- Qualifiable

-- -- | Diagrams can be qualified so that all their named points can
-- --   now be referred to using the qualification prefix.
-- instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
--       => Qualifiable (QDiagram b v m) where
--   (|>) = over _Wrapped' . D.applyD . inj . toName


------------------------------------------------------------
--  Subdiagrams
------------------------------------------------------------

-- | A @Subdiagram@ represents a diagram embedded within the context
--   of a larger diagram.  Essentially, it consists of a diagram
--   paired with any accumulated information from the larger context
--   (transformations, attributes, etc.).

data Subdiagram b v m = Subdiagram (QDiagram b v m) (Transformation v) (Context b v)

type instance V (Subdiagram b v m) = v

-- | Turn a diagram into a subdiagram with no accumulated context.
mkSubdiagram :: HasLinearMap v => QDiagram b v m -> Subdiagram b v m
mkSubdiagram d = Subdiagram d mempty empty

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
               (translation (p .-. origin))
               empty

instance Functor (Subdiagram b v) where
  fmap f (Subdiagram d t c) = Subdiagram (fmap f d) t c

-- instance (OrderedField (Scalar v), InnerSpace v, HasLinearMap v, Monoid' m)
--       => Enveloped (Subdiagram b v m) where
--   getEnvelope (Subdiagram d a) = transform (transfFromAnnot a) $ getEnvelope d

-- instance (OrderedField (Scalar v), HasLinearMap v, InnerSpace v, Semigroup m)
--       => Traced (Subdiagram b v m) where
--   getTrace (Subdiagram d a) = transform (transfFromAnnot a) $ getTrace d

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
      => HasOrigin (Subdiagram b v m) where
  moveOriginTo = translate . (origin .-.)

instance ( HasLinearMap v, InnerSpace v, Floating (Scalar v))
    => Transformable (Subdiagram b v m) where
  transform t' (Subdiagram d t c) = Subdiagram d (t' <> t) c

-- -- | Get the location of a subdiagram; that is, the location of its
-- --   local origin /with respect to/ the vector space of its parent
-- --   diagram.  In other words, the point where its local origin
-- --   \"ended up\".
-- location :: HasLinearMap v => Subdiagram b v m -> Point v
-- location (Subdiagram _ a) = transform (transfFromAnnot a) origin

-- -- | Turn a subdiagram into a normal diagram, including the enclosing
-- --   context.  Concretely, a subdiagram is a pair of (1) a diagram and
-- --   (2) a \"context\" consisting of an extra transformation and
-- --   attributes.  @getSub@ simply applies the transformation and
-- --   attributes to the diagram to get the corresponding \"top-level\"
-- --   diagram.
-- getSub :: ( HasLinearMap v, InnerSpace v
--           , Floating (Scalar v), Ord (Scalar v)
--           , Semigroup m
--           )
--        => Subdiagram b v m -> QDiagram b v m
-- getSub (Subdiagram d a) = over _Wrapped' (D.applyD a) d

-- -- | Extract the \"raw\" content of a subdiagram, by throwing away the
-- --   context.
-- rawSub :: Subdiagram b v m -> QDiagram b v m
-- rawSub (Subdiagram d _) = d

------------------------------------------------------------
--  Subdiagram maps  ---------------------------------------
------------------------------------------------------------

-- | A 'SubMap' is a map associating names to subdiagrams. There can
--   be multiple associations for any given name.
newtype SubMap = SubMap (S.Set Name)
  deriving (Semigroup, Monoid)

instance Wrapped SubMap where
    type Unwrapped SubMap = S.Set Name
    _Wrapped' = iso (\(SubMap m) -> m) SubMap

type instance Index SubMap = Name
type instance IxValue SubMap = ()

instance Ixed     SubMap where ix i       = _Wrapped'.ix i
instance At       SubMap where at i       = _Wrapped'.at i
instance Contains SubMap where contains i = _Wrapped'.contains i

-- Transformations have no action on SubMaps
instance Action (Transformation v) SubMap where
  act _ = id

-- | 'SubMap's are qualifiable: if @ns@ is a 'SubMap', then @a |>
--   ns@ is the same 'SubMap' except with every name qualified by
--   @a@.
instance Qualifiable SubMap where
  (|>) a = _Wrapped'.setmapped |>~ a

-- | Construct a 'SubMap' from a list of associations between names
--   and subdiagrams.
-- fromNames :: IsName a => [(a, Subdiagram b v m)] -> SubMap b v m
-- fromNames = SubMap . M.fromListWith (++) . map (toName *** (:[]))

-- | Look for the given name in a name map, returning a list of
--   subdiagrams associated with that name.  If no names match the
--   given name exactly, return all the subdiagrams associated with
--   names of which the given name is a suffix.
{-
lookupSub :: IsName n => n -> SubMap b v m -> Maybe [Subdiagram b v m]
lookupSub a (SubMap m)
  = M.lookup n m `mplus`
    (flattenNames . filter ((n `nameSuffixOf`) . fst) . M.assocs $ m)
  where (Name n1) `nameSuffixOf` (Name n2) = n1 `isSuffixOf` n2
        flattenNames [] = Nothing
        flattenNames xs = Just . concatMap snd $ xs
        n = toName a
-}

------------------------------------------------------------
--  Paths  -------------------------------------------------
------------------------------------------------------------

-- Trivial representation of a path, for now
type Path = [Int]

-- instance Monoid Path

-- Index a tree based on a 'Path' into it.
ixPath :: Path -> Traversal' (Tree a) (Tree a)
ixPath []     = id
ixPath (i:is) = branches.ix i.ixPath is

-- Path construction DSL

newtype PathM s a = PathM (ReaderT (Tree s) a)
  deriving (Functor, Applicative, Monad, MonadReader (Tree s))

runPathM :: PathM s [Path] -> Tree s -> [Path]
runPathM (PathM r) = runReaderT r

here :: PathM s [Path]
here = return [mempty]

-- Find tree nodes using a 'Getter'. It will return paths into the topmost
-- nodes that first satisfy it.
findPath :: Getting Any s t a b -> PathM s [Path]
findPath l = reader $ \(Node x ts) -> case
  | has l x   = [mempty]
  | otherwise = do
      (i,t) <- zip [0..] ts
      map (i:) $ findPath l t

editTree :: PathM s [Path] -> (Tree s -> Tree s) -> Tree s -> Tree s
editTree pm f t = foldr (\p -> ixPath p %~ f) t $ runPathM pm t

------------------------------------------------------------
--  Primitives  --------------------------------------------
------------------------------------------------------------

-- $prim
-- Ultimately, every diagram is essentially a tree whose leaves are /primitives/,
-- basic building blocks which can be rendered by backends.  However,
-- not every backend must be able to render every type of primitive;
-- the collection of primitives a given backend knows how to render is
-- determined by instances of 'Renderable'.

-- | A value of type @Prim b v@ is an opaque (existentially quantified)
--   primitive which backend @b@ knows how to render in vector space @v@.
data Prim b v where
  Prim :: (Transformable p, Typeable p, Renderable p b) => p -> Prim b (V p)

type instance V (Prim b v) = v

-- | The 'Transformable' instance for 'Prim' just pushes calls to
--   'transform' down through the 'Prim' constructor.
instance HasLinearMap v => Transformable (Prim b v) where
  transform v (Prim p) = Prim (transform v p)

-- | The 'Renderable' instance for 'Prim' just pushes calls to
--   'render' down through the 'Prim' constructor.
instance HasLinearMap v => Renderable (Prim b v) b where
  render b (Prim p) = render b p

-- ------------------------------------------------------------
-- -- Backends  -----------------------------------------------
-- ------------------------------------------------------------

data RNode b v a =  RStyle (Style v)
                    -- ^ A style node.
                  | RAnnot a
                  | RPrim (Prim b v)
                    -- ^ A primitive.
                  | REmpty

-- | An 'RTree' is a compiled and optimized representation of a
--   'QDiagram', which can be used by backends.  They have the
--   following invariant which backends may rely upon:
--
--   * @RPrim@ nodes never have any children.
newtype RTree b v a = RTree (Tree (RNode b v a ))

instance Wrapped (RTree b v a) where
  type Unwrapped (RTree b v a) = Tree (RNode b v a)
  _Wrapped' = iso (\(RTree t) -> t) RTree

instance Rewrapped (RTree b v a) (RTree b' v' a')

type instance V (RTree b v a) = v

instance Semigroup (RTree b v a) where
  RTree t1 <> RTree t2 = RTree (Node REmpty [t1,t2])
  sconcat ts = RTree (Node REmpty . map (op RTree) . NEL.toList $ ts)

-- | The empty @RTree@.
emptyRTree :: RTree b v a
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
class HasLinearMap v => Backend b v where

  -- | An intermediate representation used for rendering primitives.
  --   (Typically, this will be some sort of monad, but it need not
  --   be.)  The 'Renderable' class guarantees that a backend will be
  --   able to convert primitives into this type; how these rendered
  --   primitives are combined into an ultimate 'Result' is completely
  --   up to the backend.
  data Render b v :: *

  -- | The result of running/interpreting a rendering operation.
  type Result  b v :: *

  -- | Backend-specific rendering options.
  data Options b v :: *

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
  adjustDia :: (Monoid' m, Num (Scalar v)) => b -> Options b v
            -> QDiagram b v m -> (Options b v, Transformation v, QDiagram b v m)
  adjustDia _ o d = (o,mempty,d)

  -- | Given some options, take a representation of a diagram as a
  --   tree and render it.  The 'RTree' has already been simplified
  --   and has all measurements converted to @Output@ units.
  renderRTree :: b -> Options b v -> RTree b v Annotation -> Result b v

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
--   ghci> width (image \"foo.png\" 200 200)
--   \<interactive\>:8:8:
--       No instance for (Renderable Diagrams.TwoD.Image.Image b0)
--         arising from a use of `image'
--       Possible fix:
--         add an instance declaration for
--         (Renderable Diagrams.TwoD.Image.Image b0)
--       In the first argument of `width', namely
--         `(image \"foo.png\" 200 200)'
--       In the expression: width (image \"foo.png\" 200 200)
--       In an equation for `it': it = width (image \"foo.png\" 200 200)
--   @
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
--   @
--   ghci> width (image \"foo.png\" 200 200 :: D R2)
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
--   \<interactive\>:4:1:
--       Couldn't match type `V a0' with `R2'
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
--   ghci> width (circle 1 :: D R2)
--   1.9999999999999998
--   @

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

  renderRTree _ _ _ = ()

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
