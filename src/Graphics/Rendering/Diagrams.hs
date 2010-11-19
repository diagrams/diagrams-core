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
-- For most library-level and user-level code, it should be sufficient
-- to import this module, which simply re-exports useful functionality
-- from other modules in the core library.  Library writers needing
-- finer-grained access or functionality may occasionally find it
-- useful to directly import one of the constituent core libraries:
--
--   * "Graphics.Rendering.Diagrams.Transform" for invertible linear
--     and projective transformations
--
--   * "Graphics.Rendering.Diagrams.Points" for points (as distinct from vectors)
--
--   * "Graphics.Rendering.Diagrams.Names" for names used to identify locations
--     within diagrams
--
--   * "Graphics.Rendering.Diagrams.Basics" for backends, the
--     'Diagram' type itself, and primitive diagram operations.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams
       ( -- * Points

         Point(..), origin, (.-.), (.+^), (*.)

         -- * Transformations
       , Transformation
       , inv
       , apply
       , fromLinear

         -- ** Some specific transformations
       , translation, translate
       , scaling, scale

         -- ** The 'Transformable' class

       , Transformable(..)

         -- * Names

       , Name, IsName(..), Qualifiable(..)
       , NameSet
       , fromNames
       , rememberAs

         -- * Backends

       , Backend(..)
       , MultiBackend(..)
       , Renderable(..)

         -- * Attributes and styles

       , AttributeClass(..)
       , Attribute, mkAttr, unwrapAttr, attrTransformation
       , freezeAttr, thawAttr

       , Style
       , getAttr, setAttr, addAttr
       , applyAttr
       , applyStyle
       , freezeStyle, thawStyle

         -- * Primtives

       , Prim(..), prim

         -- * Bounds

       , Bounds(..)

         -- * Diagrams

       , AnnDiagram(..), Diagram

         -- ** Primitive operations

         -- | There are two fundamental operations for manipulating
         --   and composing diagrams. All other convenient diagram
         --   composition methods can be implemented in terms of
         --   these, with the help of 'NameSet's, expressions, and
         --   bounding functions.  See the standard library in the
         --   diagrams-lib package for a number of higher-level
         --   diagram composition operators implemented in terms of
         --   these.

       , rebase
       , atop

       -- ** Other operations

       , freeze, thaw

       ) where

import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.Names
import Graphics.Rendering.Diagrams.Diagrams


