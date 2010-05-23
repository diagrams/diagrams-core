{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, UndecidableInstances, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- The core library of primitives forming the basis of an embedded
-- domain-specific language for describing and rendering diagrams.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams
       ( module Graphics.Rendering.Diagrams.Transform
       , module Graphics.Rendering.Diagrams.Expressions

         -- * Backends

       , Backend(..)
       , Renderable(..)

         -- * Primtives

       , Prim(..)

         -- * Diagrams

       , Bounds(..)
       , Diagram(..)

         -- ** Primitive operations

       , rebase
       , atop
       , beside
       ) where

import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Expressions

import Data.VectorSpace
import Data.Basis
import Data.MemoTrie

import qualified Data.Map as M
import Data.Monoid
import Control.Applicative hiding (Const)

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

------------------------------------------------------------
-- Backends  -----------------------------------------------
------------------------------------------------------------

-- | Abstract diagrams are rendered to particular formats by /backends/.
--   Each backend must be an instance of the 'Backend' class, and comes
--   with an associated vector space and rendering environment.
class (HasLinearMap (BSpace b), HasLinearMap (Scalar (BSpace b)))
    => Backend b where
  type BSpace b :: *           -- The vector space associated with this backend
  type Render b :: *           -- The rendering environment used by this backend
  type Result b :: *           -- The result of the rendering operation
  data Option b :: *           -- The rendering options for this backend
  -- | 'renderDia' is used to render a diagram using this backend.
  renderDia     :: b          -- ^ Backend token
                -> [Option b] -- ^ Backend-specific rendering options
                -> Diagram b  -- ^ Diagram to render
                -> Result b   -- ^ Output of the rendering operation

-- | The 'Renderable' type class connects backends to primitives which
--   they know how to render.

class (Backend b, Transformable t) => Renderable t b where
  render :: b -> t -> Render b
  -- ^ Given a token representing the backend and a transformable
  --   object, render it in the appropriate rendering context.

------------------------------------------------------------
--  Primitives  --------------------------------------------
------------------------------------------------------------

-- | A value of type @Prim b@ is an opaque (existentially quantified)
--   primitive which backend @b@ knows how to render.
data Prim b where
  Prim :: (BSpace b ~ TSpace t, Renderable t b) => t -> Prim b

instance Backend b => Transformable (Prim b) where
  type TSpace (Prim b) = BSpace b
  transform v (Prim p) = Prim (transform v p)

instance Backend b => Renderable (Prim b) b where
  render b (Prim p) = render b p

------------------------------------------------------------
--  Diagrams  ----------------------------------------------
------------------------------------------------------------

-- | The bounding function for a diagram tells us how far we have to
--   go in a given direction to get to a hyperplane entirely
--   containing the diagram on one side of it.  XXX write more about
--   this.
newtype Bounds v = Bounds (v -> Scalar v)

instance (Ord (Scalar v), AdditiveGroup (Scalar v)) => Monoid (Bounds v) where
  mempty = Bounds $ const zeroV
  mappend (Bounds b1) (Bounds b2) = Bounds $ max <$> b1 <*> b2

-- | The basic 'Diagram' data type.  A diagram consists of a list of
--   primitives, a functional convex bounding region, and a set of
--   named (local) points.
data Diagram b = Diagram { prims  :: [Prim b]
                         , bounds :: Bounds (BSpace b)
                         , names  :: NameSet (BSpace b)
                         }

------------------------------------------------------------
--  Primitive operations  ----------------------------------
------------------------------------------------------------

-- | @'rebase' u d@ is the same as @d@, except with the local origin
--   moved to @u@.
rebase :: ( Backend b, v ~ BSpace b
          , InnerSpace v, HasLinearMap v, HasLinearMap (Scalar v)
          , AdditiveGroup (Scalar v), Fractional (Scalar v)
          , Scalar (Scalar v) ~ Scalar v)
       => LExpr v -> Diagram b -> Diagram b
rebase e d = Diagram { prims  = map (translate (negateV u))
                                    (prims d)
                     , bounds = rebaseBounds u (bounds d)
                     , names  = M.map (^-^ u) (names d)
                     }
  where u = evalLExpr e (names d)

rebaseBounds :: (InnerSpace v, AdditiveGroup (Scalar v), Fractional (Scalar v))
             => v -> Bounds v -> Bounds v
rebaseBounds u (Bounds f) = Bounds $ \v -> f v ^-^ ((u ^/ (v <.> v)) <.> v)

instance ( Backend b, HasLinearMap (BSpace b)
         , Scalar (Scalar (BSpace b)) ~ Scalar (BSpace b)
         , Fractional (Scalar (BSpace b)))
    => Transformable (Diagram b) where
  type TSpace (Diagram b) = BSpace b
  transform t (Diagram ps bs ns) = Diagram (map (transform t) ps)
                                           (Bounds $ \v -> undefined)    -- XXX
                                           (M.map (papply t) ns)

-- | Compose two diagrams by aligning their respective local origins,
--   putting the first on top of the second.
atop :: (s ~ Scalar (BSpace b), Ord s, AdditiveGroup s)
     => Diagram b -> Diagram b -> Diagram b
atop (Diagram ps1 bs1 ns1) (Diagram ps2 bs2 ns2) =
  Diagram (ps1 <> ps2) (bs1 <> bs2) (ns1 <> ns2)

instance (s ~ Scalar (BSpace b), Ord s, AdditiveGroup s) => Monoid (Diagram b) where
  mempty  = Diagram mempty mempty mempty
  mappend = atop

-- XXX should this be moved to the standard library?
-- | Place two diagrams next to each other along the given vector.
beside :: ( Backend b
          , v ~ BSpace b
          , HasLinearMap v
          , HasLinearMap (Scalar v)
          , InnerSpace v
          , AdditiveGroup (Scalar v)
          , Fractional (Scalar v)
          , Ord (Scalar v)
          , Scalar (Scalar v) ~ Scalar v)
       => v -> Diagram b -> Diagram b -> Diagram b
beside v d1@(Diagram _ (Bounds b1) _)
         d2@(Diagram _ (Bounds b2) _)
  = rebase (Const (b1 v *^ v)) d1 `atop`
    rebase (Const (b2 (negateV v) *^ negateV v)) d2
