### next [????.??.??]
* Fix bug in which infix record selectors weren't shown with parentheses

### 0.3.3 [2016.09.11]
* Add `Data.Bounded.Deriving`, which allows deriving `Bounded` with TH.
* Add `Data.Enum.Deriving`, which allows deriving `Enum` with TH.
* Add `Data.Ix.Deriving`, which allows deriving `Ix` with TH.
* Fix bug in which derived `Show` instance would parenthesize the output too eagerly

### 0.3.2
* Incorporate a fix to GHC Trac #10858, which will be introduced in GHC 8.2
* Fix bug in which derived `Ord` instances accidentally swapped their less-than(-or-equal-to) and greater-than(-or-equal-to) methods
* Fix GHC HEAD build

### 0.3.1
* Allow deriving `Functor` and `Foldable` instances for datatypes containing unboxed tuples
* Microoptimization in derived instances of higher-order versions of `Eq`, `Ord`, `Read`, and `Show`

## 0.3
* Added `Data.Eq.Deriving`, which allows deriving `Eq`, `Eq1`, and `Eq2` with TH.
* Added `Data.Ord.Deriving`, which allows deriving `Ord`, `Ord1`, and `Ord2` with TH.
* Added `Data.Read.Deriving`, which allows deriving `Read`, `Read1`, and `Eq2` with TH.
* Renamed `Text.Show.Deriving.Options` to `ShowOptions` so as to disambiguate it from the options datatypes in other `deriving-compat` modules.

### 0.2.2
* Fixed a bug in `Text.Show.Deriving`'s treatment of unlifted types

### 0.2.1
* Added `Text.Show.Deriving`, which allows deriving `Show`, `Show1`, and `Show2` with TH.

## 0.2
* Added support for GHC 8.0
* Added `Data.Functor.Deriving` and `Data.Traversable.Deriving`, which allow deriving `Functor` and `Traversable` with TH.
* Added `Data.Deriving`, which reexports all other modules

## 0.1
* Initial commit
