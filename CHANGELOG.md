# CHANGELOG

## HEAD

### Added

* Additional builds with GHC 8.0.2 and 8.2.1
* Compatibility with GHC 8.4.1

## 3.0.0

* GHC 8 compatible release

### Removed

* `open-union` dependency

## 2.0.0

### Added

* `NameMatcher` and `ElemMatcher` as more flexible mechanisms for
  matching names and elements instead of just matching by name.
* Class `FromAttribute` and some default instances

### Changed

* `DomTraversable` renamed to `Buildable`
* Combinators use `NameMatcher` and `ElemMatcher`
* Documentation improved

## 1.0.0

* Added support for parsing attributes
* Changed public interface for 'parseContent'
* Added useful functions
* Dropped unnecessary dependency from shakespeare

## 0.1.0

* Public release
