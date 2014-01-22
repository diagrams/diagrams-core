{-# LANGUAGE TypeOperators
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , ScopedTypeVariables
  #-}

module Diagrams.Core.Project where

import Data.Basis
import Data.VectorSpace hiding (project)
import Data.MemoTrie
import Data.Monoid hiding ((<>))
import Data.Semigroup

import Diagrams.Core.Points
import Diagrams.Core.Transform
import Diagrams.Core.V

------------------------------------------------------------
-- Projections

-- | Projective transformations are a superset of the affine
-- transformations represented by the 'Transformation' type.  In
-- general they are not invertable.
data Projection v = Projection (Point v -> Point v)

instance Semigroup (Projection v) where
    (Projection p1) <> (Projection p2) = Projection (p1 . p2)

instance Monoid (Projection v) where
    mappend = (<>)
    mempty = Projection id

class Projectable a where
    -- | @project' epsilon p a@ transforms @a@ by the projective
    -- transform @p@.  If the type of @a@ is not closed under
    -- projection, approximate to accuracy @epsilon@.
    project' :: Scalar (V a) -> Projection (V a) -> a -> a

    -- | @project p a@ transforms @a@ by the projective transform @p@.
    -- If the type of @a@ is not closed under projection, @project@
    -- should call @project'@ with some reasonable default value of
    -- @epsilon@.
    project  :: Projection (V a) -> a -> a

-- | @asProjection@ converts a 'Transformation' to a 'Projection' by
-- discarding the inverse transform.  This allows reusing
-- @Transformation@s in the construction of @Projection@s.
asProjection
  ::  ( HasTrie (Basis v), HasBasis v) => Transformation v -> Projection v
asProjection t = Projection f' where
      f' = papply t

instance Projectable (Point v) where
    project' = const project
    project (Projection l) = l
