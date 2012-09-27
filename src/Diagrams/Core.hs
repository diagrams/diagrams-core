-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core
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

module Diagrams.Core
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

         -- ** Subdiagram maps

       , SubMap(..)
       , fromNames
       , rememberAs

       , lookupSub

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
       , envelopeVMay, envelopeV, envelopePMay, envelopeP
       , diameter, radius

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
       , envelope, trace, subMap, names, query, sample
       , value, resetValue, clearValue

       , named, nameSub, namePoint
       , withName
       , withNameAll
       , withNames

       , freeze, setEnvelope, setTrace

       , atop

         -- ** Subdiagrams

       , Subdiagram(..), mkSubdiagram
       , getSub, rawSub
       , location
       , subPoint

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

import Diagrams.Core.Types
import Diagrams.Core.Envelope
import Diagrams.Core.HasOrigin
import Diagrams.Core.Juxtapose
import Diagrams.Core.Names
import Diagrams.Core.Points
import Diagrams.Core.Query
import Diagrams.Core.Style
import Diagrams.Core.Trace
import Diagrams.Core.Transform
import Diagrams.Core.V

import Data.Monoid.WithSemigroup (Monoid')