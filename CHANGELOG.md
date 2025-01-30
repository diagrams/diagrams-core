## [v1.5.1.1-r3](https://github.com/diagrams/diagrams-core/tree/v1.5.1.1-r3) (2025-01-30)

- Allow `base-4.21` (GHC 9.12)

## [v1.5.1.1-r2](https://github.com/diagrams/diagrams-core/tree/v1.5.1.1-r2) (2024-05-20)

- Allow:
  - `base-4.20` (GHC 9.10)
  - `lens-5.3`
  - `containers-0.7`
- Test on GHC 9.10

## [v1.5.1.1-r1](https://github.com/diagrams/diagrams-core/tree/v1.5.1.1-r1) (2024-04-20)

* Allow `linear-1.23`

## [v1.5.1.1](https://github.com/diagrams/diagrams-core/tree/v1.5.1.1) (2023-11-15)

* Allow `base-4.19` and test on GHC 9.8
* Fix more warnings

## [v1.5.1](https://github.com/diagrams/diagrams-core/tree/v1.5.1) (2023-05-11)

* Allow `base-4.18` and test on GHC 9.6 (thanks to @sergv)
* Fix some warnings (thanks to @sergv)
* Fix some documentation typos (thanks to @mchav)

## [v1.5.0.1-r1](https://github.com/diagrams/diagrams-core/tree/v1.5.0.1-r1) (2022-11-30)

* Allow `linear-1.22`

## [v1.5.0.1](https://github.com/diagrams/diagrams-core/tree/v1.5.0.1) (2022-08-27)

* Test with up to `base-4.17` and GHC 9.4
* Allow `lens-5.2`
* Fix documentation for `atLeast` and `atMost` (thanks to Igor Moreno)

## [v1.5.0](https://github.com/diagrams/diagrams-core/tree/v1.5.0) (2021-05-13)

* Updates for GHC 8.10 and 9.0
* Drop support for GHC < 8.4
* Remove deprecated `Option` type in favor of `Maybe`.  This is a
  breaking API change.

## [v1.4.2-r1](https://github.com/diagrams/diagrams-core/tree/v1.4.2-r1) (2020-02-10)

* Allow `lens-4.19` and `linear-1.21`

## [v1.4.2](https://github.com/diagrams/diagrams-core/tree/v1.4.2) (2019-10-19)

* New `KeyVal` constructor for `Annotation` ([PR](https://github.com/diagrams/diagrams-core/pull/104))
* Updates for GHC 8.8
* Drop support for GHC 7.6 and 7.8

## [v1.4.1.1](https://github.com/diagrams/diagrams-core/tree/v1.4.1.1) (2018-06-17)

* Add some `ConstraintKinds` pragmas to allow compilation on GHC 7.8 and 7.6

## [v1.4.1](https://github.com/diagrams/diagrams-core/tree/v1.4.1) (2018-04-10)

* Allow `base-4.11`
* Allow `lens-4.16`
* Add `Semigroup` instance to build on GHC 8.4

## v1.4.0.1

* Allow base-4.10

## [v1.4](https://github.com/diagrams/diagrams-core/tree/v1.4) (2016-10-26)

* **New features**

    - New `eachName` traversal, for traversing over parts of a `Name`
      that match a given type

    - More documentation explaining `HasOrigin` and `Transformable`
      instances for `Envelope`

* **Dependency/version changes**

    - Allow `lens-4.15`
    - Many other upper bounds bumped; see minor release changelogs below.

* **New instances**

    - `Transformable` instance for `Measured`

    - A bunch more instances for `Query` (`Distributive`,
      `Representable`, `Profunctor`, `Coseive`, `Closed`, `Costrong`,
      `Corepresentable`)

* **API changes**

    - Move some `Query`-related functions to `diagrams-lib` (`sample`,
      `value`, `resetValue`, `clearValue`)

    - Remove some redundant constraints in type signatures (should not
      actually affect API)

## [v1.3.0.8](https://github.com/diagrams/diagrams-core/tree/v1.3.0.8) (2016-06-05)

- allow `base-4.9`
- build warning-free on GHC 8.0.1

## [v1.3.0.7](https://github.com/diagrams/diagrams-core/tree/v1.3.0.7) (2016-05-01)

- allow `lens-4.14`

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.3.0.6...v1.3.0.7)

## [v1.3.0.6](https://github.com/diagrams/diagrams-core/tree/v1.3.0.6) (2016-02-19)

  - allow `unordered-containers-0.2.*`

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.3.0.5...v1.3.0.6)

## [v1.3.0.5](https://github.com/diagrams/diagrams-core/tree/v1.3.0.5) (2016-01-14)

  - allow `unordered-containers-0.2.6`

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.3.0.4...v1.3.0.5)

## [v1.3.0.4](https://github.com/diagrams/diagrams-core/tree/v1.3.0.4) (2015-11-10)

  - allow `semigroups-0.18`

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.3.0.3...v1.3.0.4)

## [v1.3.0.3](https://github.com/diagrams/diagrams-core/tree/v1.3.0.3) (2015-09-17)

  - allow `lens-4.13`
  - allow `linear-1.20`
  - allow `semigroups-0.17`

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.3.0.2...v1.3.0.3)

## [v1.3.0.2](https://github.com/diagrams/diagrams-core/tree/v1.3.0.2) (2015-07-19)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.3.0.1...v1.3.0.2)

## [v1.3.0.1](https://github.com/diagrams/diagrams-core/tree/v1.3.0.1) (2015-05-26)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.3...v1.3.0.1)

## [v1.3](https://github.com/diagrams/diagrams-core/tree/v1.3) (2015-04-19)

* **New features**

    - Update for ghc-7.10.

    - Switch from `vector-space` to `linear` for linear algebra.

    - `OpacityGroup` annotation for setting the opacity of diagrams as
      a group. Opacity groups can be applied with the `opacityGroup` or
      `groupOpacity` functions.

    - Added `atAttr`, `atMAttr` and `atTAttr` lenses onto the attributes
      of styles.

    - `InSpace` and `SameSpace` synonyms.

    - `size` function for computing the range of an enveloped object in
      the basis vectors.

    - "Grouping" for transparent things [\#21](https://github.com/diagrams/diagrams-core/issues/21)

* **Dependency/version changes**

    - Allow `base-4.8`
    - Allow `lens-4.9`

* **New instances**

    - `Show` instances for `Attribute` and `Style`.
    - `Each`, `Ixed` and `At` instances for and `Style`.

* **API changes**

    - `Measure` has a new internal representation. `Local`, `Global`,
      `Normalized`, and `Output` have been renamed to `local`, `global`,
      `normalized` and `output` respectivly. `Measure` is now defined in
      `Diagrams.Core.Measure`.

    - `GTAttribute` has been removed. `MAttribute` now holds measured
      attributes and no longer requires a `Data` instance.

    - `V` is now a `* -> *` kind type family.

    - New type family `N` for the number type of an object, `Scalar`
      type family no longer exists.

    - `(|>)` has moved to `(.>>)` to make room for lens's snoc operator.

    - `Style`'s internal representation now uses a hashmap of the
      `TypeRep`.

**Merged pull requests:**

- Pre 1.3 [\#82](https://github.com/diagrams/diagrams-core/pull/82) ([cchalmers](https://github.com/cchalmers))

- update for GHC-7.10, -Wall [\#81](https://github.com/diagrams/diagrams-core/pull/81) ([bergey](https://github.com/bergey))

- Style lenses [\#80](https://github.com/diagrams/diagrams-core/pull/80) ([cchalmers](https://github.com/cchalmers))

- Add isReflection [\#79](https://github.com/diagrams/diagrams-core/pull/79) ([byorgey](https://github.com/byorgey))

- Linear update [\#77](https://github.com/diagrams/diagrams-core/pull/77) ([cchalmers](https://github.com/cchalmers))

- Bump lens upper version bounds [\#74](https://github.com/diagrams/diagrams-core/pull/74) ([RyanGlScott](https://github.com/RyanGlScott))

- Add Diagram B synonym for Diagram b v n [\#73](https://github.com/diagrams/diagrams-core/pull/73) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- New stuff [\#72](https://github.com/diagrams/diagrams-core/pull/72) ([cchalmers](https://github.com/cchalmers))

- Linear [\#71](https://github.com/diagrams/diagrams-core/pull/71) ([cchalmers](https://github.com/cchalmers))

- Bump linear upper version bounds [\#75](https://github.com/diagrams/diagrams-core/pull/75) ([RyanGlScott](https://github.com/RyanGlScott))

- Change Measure back to not using Scalar v [\#65](https://github.com/diagrams/diagrams-core/pull/65) ([Mathnerd314](https://github.com/Mathnerd314))

- Remove gratuitous Data constraints [\#69](https://github.com/diagrams/diagrams-core/pull/69) ([Mathnerd314](https://github.com/Mathnerd314))

## [v1.2.0.6](https://github.com/diagrams/diagrams-core/tree/v1.2.0.6) (2015-04-03)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2.0.5...v1.2.0.6)

**Closed issues:**

- Please add support for recent versions of vector-space [\#78](https://github.com/diagrams/diagrams-core/issues/78)

## [v1.2.0.5](https://github.com/diagrams/diagrams-core/tree/v1.2.0.5) (2015-01-13)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2.0.4...v1.2.0.5)

## [v1.2.0.4](https://github.com/diagrams/diagrams-core/tree/v1.2.0.4) (2014-12-04)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2.0.3...v1.2.0.4)

## [v1.2.0.3](https://github.com/diagrams/diagrams-core/tree/v1.2.0.3) (2014-11-17)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2.0.2...v1.2.0.3)

## [v1.2.0.2](https://github.com/diagrams/diagrams-core/tree/v1.2.0.2) (2014-08-22)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2.0.1...v1.2.0.2)

**Closed issues:**

- Warn against GND for IsName [\#67](https://github.com/diagrams/diagrams-core/issues/67)

## [v1.2.0.1](https://github.com/diagrams/diagrams-core/tree/v1.2.0.1) (2014-06-04)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2...v1.2.0.1)

**Merged pull requests:**

- Propogate transformations into the terms of Measure [\#66](https://github.com/diagrams/diagrams-core/pull/66) ([bergey](https://github.com/bergey))

## [v1.2](https://github.com/diagrams/diagrams-core/tree/v1.2) (2014-06-02)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.1...v1.2)

* **New features**

    - New function `matrixHomRep` to convert a transformation to a
      homogeneous matrix representation.

    - New function `dropTransl` to drop the translation component
      from a transformation.

    - A mini-DSL for Measures.

    - New `extent` function, used in `diameter`.

    - New `dimension` function to return the dimension of a vector space.

    - New `_relative` iso between points and vectors.

    - `avgScale` function (for computing the average scaling factor of
      a transformation) has been moved from `diagrams-lib` to
      `diagrams-core` and generalized to work over any vector space.

* **Dependency/version changes**

    - Allow `semigroups-0.15`
    - Allow `lens-4.2`

* **API changes**

    - Major refactoring which removes `freeze` (and hence `Split` transforms,
      etc.) and adds units of `Measure`.

    - Refactoring and simplification of the `Backend` class.

    - Remove `Multibackend`.

    - Remove `nullPrim`, `IsPrim` and simplify `RPrim` so that it does not
      carry a transformation.

    - Update `adjustDia` to return a transformation, not just a scale factor.
      Add `renderDiaT` which returns a transformation (for use by end
      users, e.g. to convert output coordinates back into local coordinates).
      
**Implemented enhancements:**

- Extracting things from Prim wrappers [\#42](https://github.com/diagrams/diagrams-core/issues/42)

**Closed issues:**

- Incomplete comment on Backend class [\#64](https://github.com/diagrams/diagrams-core/issues/64)

- Please add support for Lens 4.x [\#56](https://github.com/diagrams/diagrams-core/issues/56)

**Merged pull requests:**

- A mini-DSL for Measures. [\#61](https://github.com/diagrams/diagrams-core/pull/61) ([byorgey](https://github.com/byorgey))

- Clean-slate redesign/simplification of `Backend` class [\#60](https://github.com/diagrams/diagrams-core/pull/60) ([byorgey](https://github.com/byorgey))

- Rework units [\#59](https://github.com/diagrams/diagrams-core/pull/59) ([byorgey](https://github.com/byorgey))

- Avg scale [\#58](https://github.com/diagrams/diagrams-core/pull/58) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Preliminary implementation of Measure [\#55](https://github.com/diagrams/diagrams-core/pull/55) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- No mco [\#62](https://github.com/diagrams/diagrams-core/pull/62) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v1.1](https://github.com/diagrams/diagrams-core/tree/v1.1) (2014-03-09)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.0.0.1...v1.1)

* **New features**

    - New `basis` function
	- New `determinant` function for computing the determinant of a
      `Transformation`
    - Add `Typeable` constraint on `Prim`s, making it possible to
      extract things back out of a `Prim` wrapper using `cast`
	- Raw `Trace`s now return a *sorted list* of intersections,
      instead of only the smallest.  This is used to implement a new
      family of functions `rayTraceV`, `rayTraceP`, `maxRayTraceV`,
      `maxRayTraceP`, which work similarly to the parallel versions
      without `Ray`, but return the first intersection in the
      *positive* direction from the given point, rather than the
      smallest in absolute terms.
    - New `Annotation` type and corresponding `applyAnnotation`
      function, for attaching uninterpreted annotations at specific
      points in a diagram tree.  Currently this is used for
      hyperlinks; more annotation types will be added in the future.

* **Dependency/version changes**

    - Require `lens-4.0`
	- Allow `vector-space-points-0.2`

* **Bug fixes**

    - Looking up a subdiagram by name now results in a diagram which
      still has that name (#43)
      
**Closed issues:**

- Named subdiagrams lose their names after being looked up [\#43](https://github.com/diagrams/diagrams-core/issues/43)

**Merged pull requests:**

- Hyperlinks [\#57](https://github.com/diagrams/diagrams-core/pull/57) ([tdox](https://github.com/tdox))

- Added `basis`, simplified `onBasis` [\#54](https://github.com/diagrams/diagrams-core/pull/54) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Determinants [\#53](https://github.com/diagrams/diagrams-core/pull/53) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Introduce Typeable constraint on Prims \(see \#42\) [\#52](https://github.com/diagrams/diagrams-core/pull/52) ([byorgey](https://github.com/byorgey))

- Update Wrapped instances for lens-4.0 [\#51](https://github.com/diagrams/diagrams-core/pull/51) ([bergey](https://github.com/bergey))

- return list of traces [\#48](https://github.com/diagrams/diagrams-core/pull/48) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Projections rebase [\#50](https://github.com/diagrams/diagrams-core/pull/50) ([bergey](https://github.com/bergey))

## [v1.0.0.1](https://github.com/diagrams/diagrams-core/tree/v1.0.0.1) (2013-11-28)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.0...v1.0.0.1)

## [v1.0](https://github.com/diagrams/diagrams-core/tree/v1.0) (2013-11-25)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v0.7.0.1...v1.0)

* **New features**

    * Delayed subtrees: instead of a primitive, one can now also have
      a delayed subtree at a leaf, containing a continuation which
      generates a `QDiagram` when given the accumulated d-annotation
      at that point in the tree.  Useful for things which need to know
      the final transformation applied to them before deciding what
      diagram to generate.  The prototypical use case is arrows: see
      https://github.com/diagrams/diagrams-lib/issues/112 .  However,
      this may be useful for other things as well: for example,
      diagrams which scale normally until hitting some maximum or
      minimum size, at which point they refuse to scale any further
      (or more generally diagrams which scale as some non-linear
      function of the transformation applied to them).

      The only downside is that the u-annotation must be fixed ahead
      of time---doing otherwise requires a more general solution for
      constraint solving.

    * New function `lookupName` for doing a simple lookup of a named
      subdiagram

    * New module `Diagrams.Core.Compile`, containing a framework for
      compiling `QDiagrams` into a simpler tree type `RTree`, which
      may be used by backends for rendering.

* **New instances**

    * `Qualifiable` instances for `(,)`, `(,,)`, `[]`, `Set`, `Map k`,
      and `(->) e`.

    * `(->) e` instance for `Juxtaposable` (thanks to Carlos Scheidegger)

* **API changes**

    * Export `pointDiagram` function, which creates an otherwise empty
      diagram with a point (not empty) envelope

    * A bunch of stuff now uses machinery from the `lens` library.
	    * `envelope`, `trace`, and `subMap` are now `Lens'`es
        * `Wrapped` instances for `Trace`, `TransInv`, `QDiagram`,
          `SubMap`, `Envelope`, `Style`, `Query`, and `Name` (replaces
          `Newtype` instances)
	    * `Iso`s for `Query`, `Envelope`, `QDiagram`, `SubMap`, `TransInv`
	    
**Implemented enhancements:**

- Tree structure in Backends [\#19](https://github.com/diagrams/diagrams-core/issues/19)

**Merged pull requests:**

- Delayed subtrees [\#47](https://github.com/diagrams/diagrams-core/pull/47) ([byorgey](https://github.com/byorgey))

- Trees for backends [\#46](https://github.com/diagrams/diagrams-core/pull/46) ([byorgey](https://github.com/byorgey))

- add b-\>a instance for Juxtaposable [\#45](https://github.com/diagrams/diagrams-core/pull/45) ([cscheid](https://github.com/cscheid))

- Lens [\#44](https://github.com/diagrams/diagrams-core/pull/44) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v0.7.0.1](https://github.com/diagrams/diagrams-core/tree/v0.7.0.1) (2013-09-27)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v0.7...v0.7.0.1)

**Merged pull requests:**

- Add lookupName function. [\#41](https://github.com/diagrams/diagrams-core/pull/41) ([cmears](https://github.com/cmears))

## [v0.7](https://github.com/diagrams/diagrams-core/tree/v0.7) (2013-08-09)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v0.6.0.2...v0.7)

* **New features**

    - new function `onBasis`, to extract the matrix equivalent of a `Transformation`
    - `SubMap`s are now `Deletable`
    - new function `localize` for hiding/deleting names from scope
    - new `IsPrim` class, containing `transformWithFreeze` function.
        This is primarily intended to support scale-invariant primitives
        (*e.g.* arrowheads) but may be useful for other stuff as well.
	The default implementation of `renderDia` now uses
	`transformWithFreeze`.
    - optimized `Transformable` instance for `TransInv`

* **New instances**

    - `Eq`, `Ord`, `Enveloped`, `Traced`, and `Qualifiable` instances
      for `TransInv`

    - `Transformable` instance for functions, which acts by conjugation

* **API changes**

    - `named` and `namePoint` have moved to the `diagrams-lib` package.

* **Dependency/version changes**

    - allow `base-4.7`
    - upgrade to `monoid-extras-0.3`
    
**Implemented enhancements:**

- Function to extract matrix coefficients from a Transformation [\#22](https://github.com/diagrams/diagrams-core/issues/22)

**Closed issues:**

- Support for monoid-extras-0.3.0.0 [\#38](https://github.com/diagrams/diagrams-core/issues/38)

**Merged pull requests:**

- New IsPrim class for supporting ScaleInv [\#37](https://github.com/diagrams/diagrams-core/pull/37) ([byorgey](https://github.com/byorgey))

- onBasis gets the matrix equivalent of the Transformation [\#36](https://github.com/diagrams/diagrams-core/pull/36) ([bergey](https://github.com/bergey))

## [v0.6.0.2](https://github.com/diagrams/diagrams-core/tree/v0.6.0.2) (2013-03-06)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v0.6.0.1...v0.6.0.2)

**Fixed bugs:**

- radius is wrong [\#35](https://github.com/diagrams/diagrams-core/issues/35)

**Merged pull requests:**

- make SubMaps deletable, and add a new function 'localize' for hiding/deleting names [\#34](https://github.com/diagrams/diagrams-core/pull/34) ([byorgey](https://github.com/byorgey))

## [v0.6.0.1](https://github.com/diagrams/diagrams-core/tree/v0.6.0.1) (2013-01-07)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v0.6...v0.6.0.1)

**Fixed bugs:**

- "type instance V \(Point v\) = v" is not visible without explicit import. [\#17](https://github.com/diagrams/diagrams-core/issues/17)

**Merged pull requests:**

- Transformable instance for functions \(by conjugation\) [\#32](https://github.com/diagrams/diagrams-core/pull/32) ([conal](https://github.com/conal))

## [v0.6](https://github.com/diagrams/diagrams-core/tree/v0.6) (2012-12-12)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v0.5...v0.6)

* **New features**

    - Proper support for subdiagrams: previous versions of
      diagrams-core had a mechanism for associating names with a pair
      of a location and an envelope.  Now, names are associated with
      actual subdiagrams (including their location and envelope, along
      with all the other information stored by a diagram).

        See
        [`Diagrams.Core.Types`](https://github.com/diagrams/diagrams-core/blob/27b275f45cad514caefcd3035e4e261f1b4adf6f/src/Diagrams/Core/Types.hs#L493).

    - Traces: in addition to an envelope, each diagram now stores a
      "trace", which is like an embedded raytracer: given any ray
      (represented by a base point and a vector), the trace computes
      the closest point of intersection with the diagram along the
      ray.  This is useful for determining points on the boundary of a
      diagram, *e.g.* when drawing arrows between diagrams.

        See [`Diagrams.Core.Trace`](https://github.com/diagrams/diagrams-core/blob/2f8727fdfa60cdf46456a23f358c8a771b2cd90d/src/Diagrams/Core/Trace.hs).

* **API changes**

    - The modules have all been renamed to be more consistent with the
      module naming scheme in the rest of the diagrams universe.  In
      particular:

        `Graphics.Rendering.Diagrams`       -->  `Diagrams.Core`
        `Grahpics.Rendering.Diagrams.Core`  -->  `Diagrams.Core.Types`
        `Graphics.Rendering.Diagrams.*`     -->  `Diagrams.Core.*`

    - `Graphics.Rendering.Diagrams.UDTree` has been split out into a
      separate
      [`dual-tree`](http://hackage.haskell.org/package/dual%2Dtree)
      package (which has also been substantially rewritten).

    - `Graphics.Rendering.Diagrams.{Monoids,MList}` have been split
      out into a separate [`monoid-extras`](http://hackage.haskell.org/package/monoid%2Dextras) package.

    - The `names` function now returns a list of names and their
      associated locations, instead of the associated subdiagrams.  In
      particular the output is suitable to be rendered to a `String`
      using `show`.

    - The new `subMap` function fills a similar role that `names` used
      to play, returning the entire mapping from names to subdiagrams.

    - New functions `envelope[VP]May`

        `envelopeV` and `envelopeP` return the zero vector and origin,
        respectively, when called on an empty envelope.  However,
        sometimes it's useful to actually know whether the envelope was
        empty or not (the zero vector and the origin are legitimate
        outputs from non-empty envelopes).  The new functions have their
        return type wrapped in `Maybe` for this purpose.

    - New functions `envelopeS` and `envelopeSMay`

        Like `envelope[VP](May)`, but returning a scalar multiple of
		the input vector.

    - The `Graphics.Rendering.Diagrams.Util` module has been removed,
      along with the `withLength` function.  Calls to `withLength` can
      be replaced using

        `withLength s v = s *^ normalized v`

    - Add needed constraints `(InnerSpace v, OrderedField (Scalar v),
      Monoid' m)` to the type of the `renderDias` method in the
      `MultiBackend` class.

    - Generalized `Transformable` instances for pairs and tuples

		Previously, the components of the tuples were required to have
		the same type; but everything still works as long as they all
		share the same vector space.  This is actually useful in
		practice: say, if we wanted to pair a diagram with a path and
		then apply the same transformation to both.

* **Improvements**

    - More efficient implementation of `diameter`

* **Dependency/version changes**

    - Tested with GHC 7.6.1
    - allow `base-4.6`
    - allow `containers-0.5.*`
    - allow `MemoTrie-0.6.1`

* **Bug fixes**

    - juxtaposeDefault now correctly handles empty envelopes (#37)

        `juxtaposeDefault` is now the identity on the second object if
        either one has an empty envelope.  In particular this means that
        `mempty` is now an identity element for `beside` and friends.
        
**Implemented enhancements:**

- Turn R2 into D2 \(Generalize R2 to any numeric type\) [\#20](https://github.com/diagrams/diagrams-core/issues/20)

- Terminology: rename "bounding function" -\> "envelope"; "boundary function" -\> "boundary" [\#16](https://github.com/diagrams/diagrams-core/issues/16)

- Refactor: rename AnnDiagram to QDiagram [\#15](https://github.com/diagrams/diagrams-core/issues/15)

- Combine \(point, bounds\) pairs stored in NameMap into a single "located bounding function" data structure [\#14](https://github.com/diagrams/diagrams-core/issues/14)

- Remember more structure when building diagrams [\#12](https://github.com/diagrams/diagrams-core/issues/12)

- Diagram-building service library + executable [\#7](https://github.com/diagrams/diagrams-core/issues/7)

**Fixed bugs:**

- setBounds is incorrect -- throws away bounds of subsequent diagrams too [\#13](https://github.com/diagrams/diagrams-core/issues/13)

- Start developing test suites [\#10](https://github.com/diagrams/diagrams-core/issues/10)

- Freezing does not appear to work with the SVG backend [\#9](https://github.com/diagrams/diagrams-core/issues/9)

- Silent failure on other image types than .png [\#6](https://github.com/diagrams/diagrams-core/issues/6)

- Tutorial contains links to old version of package [\#5](https://github.com/diagrams/diagrams-core/issues/5)

- space is not left for empty diagram when using e.g. hcat' with {sep = ... } [\#3](https://github.com/diagrams/diagrams-core/issues/3)

- Ellipse rotated incorrectly in test file with ellipse next to a square [\#2](https://github.com/diagrams/diagrams-core/issues/2)

**Closed issues:**

- Rename core modules to remove Graphics.Rendering prefix. [\#28](https://github.com/diagrams/diagrams-core/issues/28)

- Improve haddock documentation [\#11](https://github.com/diagrams/diagrams-core/issues/11)

- Improve description of Envelope in Haddock documentation [\#1](https://github.com/diagrams/diagrams-core/issues/1)

**Merged pull requests:**

- Add envelopeS / envelopeSMay for querying scalar displacements from envelopes [\#31](https://github.com/diagrams/diagrams-core/pull/31) ([mgsloan](https://github.com/mgsloan))

- Better definition for diameter [\#30](https://github.com/diagrams/diagrams-core/pull/30) ([mgsloan](https://github.com/mgsloan))

- Added needed constraints for MultiBackend. [\#29](https://github.com/diagrams/diagrams-core/pull/29) ([fryguybob](https://github.com/fryguybob))

- Rename `names` to `subMap`, and add new function `names` [\#26](https://github.com/diagrams/diagrams-core/pull/26) ([byorgey](https://github.com/byorgey))

- Fixes to work with rewritten dual-tree [\#25](https://github.com/diagrams/diagrams-core/pull/25) ([byorgey](https://github.com/byorgey))

- Fix for juxtaposeDefault to correctly handle empty envelopes [\#24](https://github.com/diagrams/diagrams-core/pull/24) ([byorgey](https://github.com/byorgey))

- dep bumps - fixes for GHC7.6 [\#23](https://github.com/diagrams/diagrams-core/pull/23) ([mgsloan](https://github.com/mgsloan))

## [v0.5](https://github.com/diagrams/diagrams-core/tree/v0.5) (2012-03-09)

* New features:
    - New `Juxtaposable` class
    - New `NullBackend` and `D` types, for conveniently giving a
      monomorphic type to diagrams when we don't care which one it is.
    - [\#27](http://code.google.com/p/diagrams/issues/detail?id=27): Change type of `adjustDia` to return a new options record
      (with an explicitly filled-in size)

* New instances:
    - `Enveloped`, `HasOrigin`, `Juxtaposable`, `HasStyle`, and `Transformable`
      instances for `Set`s and tuples
    - `V Double = Double`
    - `Juxtaposable` and `Boundable` instances for `Map`

* API changes
    - `AnnDiagram` renamed to `QDiagram`
    - [\#61](http://code.google.com/p/diagrams/issues/detail?id=61): terminology change from "bounds" to "envelope"
        + `boundary` -> `envelopeP`
        + "bounding region" -> "envelope"
        + `Bounds` -> `Envelope`
        + `Boundable` -> `Enveloped`
        + `getBounds` -> `getEnvelope`
        + *etc.*
    - Split out definition of `Point` into separate package
      ([`vector-space-points`](http://hackage.haskell.org/package/vector%2Dspace%2Dpoints))
    - The `Point` constructor `P` is no longer exported from
      `Graphics.Rendering.Diagrams`.  See the `Diagrams.TwoD.Types` module
      from `diagrams-lib` for new tools for working with abstract 2D
      points.  If you really need the `P` constructor, import
      `Graphics.Rendering.Diagrams.Points`.
    - Name-related functions now return "located bounding functions"
      instead of pairs of points and bounds, to allow for future
      expansion.

* Dependency/version changes:
    - `vector-space` 0.8 is now required.
    - Bump base upper bound to allow 4.5; now tested with GHC 7.4.1.

* Bug fixes:
    - Bug fix related to empty envelopes

0.4: 23 October 2011
--------------------

* improved documentation
* a few new instances (Newtype Point, Boundable Point)
* new functions (value, clearValue, resetValue) for working with
  alternate query monoids

0.3: 18 June 2011
-----------------

* big overhaul of name maps:
    - allow arbitrary types as atomic names
    - carry along bounding functions as well as names in NameMaps
    - additional functions for querying information associated with names
* fix for issue #34 (fix behavior of setBounds)
* Transformable and HasOrigin instances for Transformations

0.2: 3 June 2011
----------------

* bounding regions can now be overridden
* new namePoint function for more flexibly assigning names to arbitrary points
* add HasStyle, Boundable, and HasOrigin instances for lists
* add a "trivial backend"
* transformable attributes

0.1.1: 18 May 2011
------------------

* link to new website

0.1: 17 May 2011
----------------

* initial preview release

\* *This Change Log was automatically generated by (and hand edited) [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
