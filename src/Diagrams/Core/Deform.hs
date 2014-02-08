{-# LANGUAGE TypeOperators
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , ScopedTypeVariables
  #-}

module Diagrams.Core.Deform where

import Data.Basis
import Data.VectorSpace
import Data.MemoTrie
import Data.Monoid hiding ((<>))
import Data.Semigroup

import Diagrams.Core.Points
import Diagrams.Core.Transform
import Diagrams.Core.V

------------------------------------------------------------
-- Deformations

-- | @Deformations@ are a superset of the affine transformations
-- represented by the 'Transformation' type.  In general they are not
-- invertable.  @Deformation@s include projective transformations.
-- @Deformation@ can represent other functions from points to points
-- which are "well-behaved", in that they do not introduce small wiggles.
data Deformation v = Deformation (Point v -> Point v)

instance Semigroup (Deformation v) where
    (Deformation p1) <> (Deformation p2) = Deformation (p1 . p2)

instance Monoid (Deformation v) where
    mappend = (<>)
    mempty = Deformation id

class Deformable a where
    -- | @deform' epsilon d a@ transforms @a@ by the deformation @d@.
    -- If the type of @a@ is not closed under projection, approximate
    -- to accuracy @epsilon@.
    deform' :: Scalar (V a) -> Deformation (V a) -> a -> a

    -- | @deform d a@ transforms @a@ by the deformation @d@.
    -- If the type of @a@ is not closed under projection, @deform@
    -- should call @deform'@ with some reasonable default value of
    -- @epsilon@.
    deform  :: Deformation (V a) -> a -> a

-- | @asDeformation@ converts a 'Transformation' to a 'Deformation' by
-- discarding the inverse transform.  This allows reusing
-- @Transformation@s in the construction of @Deformation@s.
asDeformation
  ::  ( HasTrie (Basis v), HasBasis v) => Transformation v -> Deformation v
asDeformation t = Deformation f' where
      f' = papply t

instance Deformable (Point v) where
    deform' = const deform
    deform (Deformation l) = l
