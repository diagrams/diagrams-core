-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Diagrams
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The core library of primitives forming the basis of an embedded
-- domain-specific language for describing and rendering diagrams.
-- Normal users of the diagrams library should almost never need to
-- import anything from this package directly; instead, import modules
-- (especially "Diagrams.Prelude") from the diagrams-lib package,
-- which re-exports most things of value to users.
--
-- For most library code needing access to core internals, it should
-- be sufficient to import this module, which simply re-exports useful
-- functionality from other modules in the core library.  Library
-- writers needing finer-grained access or functionality may
-- occasionally find it useful to directly import one of the
-- constituent core modules.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Diagrams
       ( -- * Associated vector spaces

         V

         -- * Points

       , Point(..), origin, (*.)

         -- * Vectors

       , withLength

         -- * Transformations
         -- ** Invertible linear transformations
       , (:-:), (<->), linv, lapp

         -- ** General transformations
       , Transformation
       , inv, transp, transl
       , apply
       , papply
       , fromLinear

         -- ** Some specific transformations
       , translation, translate, moveTo, place
       , scaling, scale

         -- ** The Transformable class

       , Transformable(..)

         -- ** Translational invariance

       , TransInv(..)

         -- * Names

       , AName
       , Name, IsName(..)
       , Qualifiable(..), (.>)
       , NameMap
       , fromNames, fromNamesB
       , rememberAs

       , lookupN

         -- * Attributes and styles

       , AttributeClass
       , Attribute, mkAttr, mkTAttr, unwrapAttr

       , Style, HasStyle(..)
       , getAttr, combineAttr
       , applyAttr, applyTAttr

         -- * Bounding regions

       , Bounds(..)
       , Boundable(..)
       , boundaryV, boundary, boundaryFrom
       , diameter, radius

         -- * Things with local origins

       , HasOrigin(..), moveOriginBy

         -- * Queries

       , Query(..)

         -- * Primtives

       , Prim(..), nullPrim

         -- * Diagrams

       , AnnDiagram, mkAD, Diagram
       , prims
       , bounds, names, query, sample
       , value, resetValue, clearValue

       , named, namePoint
       , withName
       , withNameAll
       , withNames

       , freeze, setBounds

       , atop

         -- * Backends

       , Backend(..)
       , MultiBackend(..)
       , Renderable(..)

         -- * Convenience classes

       , HasLinearMap
       , OrderedField

       ) where

import Graphics.Rendering.Diagrams.V
import Graphics.Rendering.Diagrams.Util
import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Bounds
import Graphics.Rendering.Diagrams.HasOrigin
import Graphics.Rendering.Diagrams.Query
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.Names
import Graphics.Rendering.Diagrams.Style
import Graphics.Rendering.Diagrams.Core