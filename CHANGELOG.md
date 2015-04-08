# Change Log

## [v1.3]() (2015-04-08)

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
      
## [v1.2.0.6](https://github.com/diagrams/diagrams-core/tree/v1.2.0.6) (2015-04-03)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2.0.5...v1.2.0.6)

**Implemented enhancements:**

- "Grouping" for transparent things [\#21](https://github.com/diagrams/diagrams-core/issues/21)

**Closed issues:**

- Please add support for recent versions of vector-space [\#78](https://github.com/diagrams/diagrams-core/issues/78)

**Merged pull requests:**

- Pre 1.3 [\#82](https://github.com/diagrams/diagrams-core/pull/82) ([cchalmers](https://github.com/cchalmers))

- update for GHC-7.10, -Wall [\#81](https://github.com/diagrams/diagrams-core/pull/81) ([bergey](https://github.com/bergey))

- Style lenses [\#80](https://github.com/diagrams/diagrams-core/pull/80) ([cchalmers](https://github.com/cchalmers))

- Add isReflection [\#79](https://github.com/diagrams/diagrams-core/pull/79) ([byorgey](https://github.com/byorgey))

## [v1.2.0.5](https://github.com/diagrams/diagrams-core/tree/v1.2.0.5) (2015-01-13)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2.0.4...v1.2.0.5)

## [v1.2.0.4](https://github.com/diagrams/diagrams-core/tree/v1.2.0.4) (2014-12-04)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2.0.3...v1.2.0.4)

## [v1.2.0.3](https://github.com/diagrams/diagrams-core/tree/v1.2.0.3) (2014-11-17)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2.0.2...v1.2.0.3)

**Merged pull requests:**

- Linear update [\#77](https://github.com/diagrams/diagrams-core/pull/77) ([cchalmers](https://github.com/cchalmers))

- Bump lens upper version bounds [\#74](https://github.com/diagrams/diagrams-core/pull/74) ([RyanGlScott](https://github.com/RyanGlScott))

- Add Diagram B synonym for Diagram b v n [\#73](https://github.com/diagrams/diagrams-core/pull/73) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- New stuff [\#72](https://github.com/diagrams/diagrams-core/pull/72) ([cchalmers](https://github.com/cchalmers))

- Linear [\#71](https://github.com/diagrams/diagrams-core/pull/71) ([cchalmers](https://github.com/cchalmers))

- Bump linear upper version bounds [\#75](https://github.com/diagrams/diagrams-core/pull/75) ([RyanGlScott](https://github.com/RyanGlScott))

- Port to linear [\#70](https://github.com/diagrams/diagrams-core/pull/70) ([cchalmers](https://github.com/cchalmers))

- Change Measure back to not using Scalar v [\#65](https://github.com/diagrams/diagrams-core/pull/65) ([Mathnerd314](https://github.com/Mathnerd314))

- dynamic typing [\#63](https://github.com/diagrams/diagrams-core/pull/63) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v1.2.0.2](https://github.com/diagrams/diagrams-core/tree/v1.2.0.2) (2014-08-22)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2.0.1...v1.2.0.2)

**Closed issues:**

- Out of date git repo? [\#68](https://github.com/diagrams/diagrams-core/issues/68)

- Warn against GND for IsName [\#67](https://github.com/diagrams/diagrams-core/issues/67)

**Merged pull requests:**

- Remove gratuitous Data constraints [\#69](https://github.com/diagrams/diagrams-core/pull/69) ([Mathnerd314](https://github.com/Mathnerd314))

## [v1.2.0.1](https://github.com/diagrams/diagrams-core/tree/v1.2.0.1) (2014-06-04)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.2...v1.2.0.1)

**Merged pull requests:**

- Propogate transformations into the terms of Measure [\#66](https://github.com/diagrams/diagrams-core/pull/66) ([bergey](https://github.com/bergey))

## [v1.2](https://github.com/diagrams/diagrams-core/tree/v1.2) (2014-06-02)

[Full Changelog](https://github.com/diagrams/diagrams-core/compare/v1.1...v1.2)

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



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
