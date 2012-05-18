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

       , Point, origin, (*.)

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

         -- * Envelopes

       , Envelope
       , inEnvelope, appEnvelope, onEnvelope, mkEnvelope
       , Enveloped(..)
       , envelopeV, envelopeP
       , diameter, radius

       , LocatedEnvelope(..)
       , location, locateEnvelope

         -- * Traces

       , Trace(..)
       , inTrace, mkTrace
       , Traced(..)
       , traceV, traceP
       , maxTraceV, maxTraceP

         -- * Things with local origins

       , HasOrigin(..), moveOriginBy

         -- * Juxtaposable things

       , Juxtaposable(..), juxtaposeDefault

         -- * Queries

       , Query(..)

         -- * Primtives

       , Prim(..), nullPrim

         -- * Diagrams

       , QDiagram, mkQD, Diagram
       , prims
       , envelope, trace, names, query, sample
       , value, resetValue, clearValue

       , named, namePoint
       , withName
       , withNameAll
       , withNames

       , freeze, setEnvelope, setTrace

       , atop

         -- * Backends

       , Backend(..)
       , MultiBackend(..)
       , Renderable(..)

         -- ** The null backend

       , NullBackend, D

         -- * Convenience classes

       , HasLinearMap
       , OrderedField
       , Monoid'

       ) where

import Graphics.Rendering.Diagrams.Core
import Graphics.Rendering.Diagrams.Envelope
import Graphics.Rendering.Diagrams.HasOrigin
import Graphics.Rendering.Diagrams.Juxtapose
import Graphics.Rendering.Diagrams.Names
import Graphics.Rendering.Diagrams.Points
import Graphics.Rendering.Diagrams.Query
import Graphics.Rendering.Diagrams.Style
import Graphics.Rendering.Diagrams.Trace
import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.V

import Data.Monoid.WithSemigroup (Monoid')