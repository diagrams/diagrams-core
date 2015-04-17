{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.V
-- Copyright   :  (c) 2011-2015 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Type family for identifying associated vector spaces.
--
-----------------------------------------------------------------------------

module Diagrams.Core.V
  ( V , N , Vn
  , InSpace, SameSpace
  ) where

import           Data.Map
import           Data.Monoid.Coproduct
import           Data.Monoid.Deletable
import           Data.Monoid.Split
import           Data.Semigroup
import           Data.Set
import           Data.IntMap
import           Data.Tree
import           Data.HashMap.Lazy
import           Data.HashSet
import           Data.Sequence

import           Linear.Vector

------------------------------------------------------------
-- Vector spaces -------------------------------------------
------------------------------------------------------------

-- | Many sorts of objects have an associated vector space in which
--   they \"live\".  The type function @V@ maps from object types to
--   the associated vector space. The resulting vector space has kind @* -> *@
--   which means it takes another value (a number) and returns a concrete
--   vector. For example 'V2' has kind @* -> *@ and @V2 Double@ is a vector.
type family V a :: * -> *

-- Note, to use these instances one often needs a constraint of the form
--   V a ~ V b, etc.
type instance V (a,b)     = V a
type instance V (a,b,c  ) = V a
type instance V (a,b,c,d) = V a

type instance V (a -> b)      = V b
type instance V [a]           = V a
type instance V (Option a)    = V a
type instance V (Set a)       = V a
type instance V (Map k a)     = V a
type instance V (IntMap a)    = V a
type instance V (Seq a)       = V a
type instance V (Tree a)      = V a
type instance V (Maybe a)     = V a
type instance V (HashMap k a) = V a
type instance V (HashSet a)   = V a

type instance V (Deletable m) = V m
type instance V (Split m)     = V m
type instance V (m :+: n)     = V m

-- | The numerical field for the object, the number type used for calculations.
type family N a :: *

type instance N (a,b)     = N a
type instance N (a,b,c)   = N a
type instance N (a,b,c,d) = N a

type instance N (a -> b)   = N b
type instance N [a]        = N a
type instance N (Option a) = N a
type instance N (Set a)    = N a
type instance N (Map k a)  = N a
type instance N (IntMap a) = N a
type instance N (Seq a)    = N a
type instance N (Tree a)   = N a
type instance N (Maybe a)  = N a
type instance N (HashMap k a) = N a
type instance N (HashSet a)   = N a

type instance N (Deletable m) = N m
type instance N (Split m)     = N m
type instance N (m :+: n)     = N m

-- | Conveient type alias to retrieve the vector type associated with an
--   object's vector space. This is usually used as @Vn a ~ v n@ where @v@ is
--   the vector space and @n@ is the numerical field.
type Vn a = V a (N a)

-- | @InSpace v n a@ means the type @a@ belongs to the vector space @v n@,
--   where @v@ is 'Additive' and @n@ is a 'Num'.
class (V a ~ v, N a ~ n, Additive v, Num n) => InSpace v n a
instance (V a ~ v, N a ~ n, Additive v, Num n) => InSpace v n a

-- | @SameSpace a b@ means the types @a@ and @b@ belong to the same
--   vector space @v n@.
class (V a ~ V b, N a ~ N b) => SameSpace a b
instance (V a ~ V b, N a ~ N b) => SameSpace a b

