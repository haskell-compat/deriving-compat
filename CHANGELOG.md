### next [????.??.??]
* Add `Data.Deriving.Via`, which allows emulating the behavior of the
  `GeneralizedNewtypeDeriving` and `DerivingVia` extensions.

## 0.4 [2017.12.07]
* Incorporate changes from the `EmptyDataDeriving` proposal (which is in GHC
  as of 8.4):
  * For derived `Eq` and `Ord` instances for empty data types, simply return
    `True` and `EQ`, respectively, without inspecting the arguments.
  * For derived `Read` instances for empty data types, simply return `pfail`
    (without `parens`).
  * For derived `Show` instances for empty data types, inspect the argument
    (instead of `error`ing). In addition, add `showEmptyCaseBehavior` to
    `ShowOptions`, which configures whether derived instances for empty data
    types should use the `EmptyCase` extension (this is disabled by default).
  * For derived `Functor` and `Traversable` instances for empty data
    types, make `fmap` and `traverse` strict in its argument.
  * For derived `Foldable` instances, do not error on empty data types.
    Instead, simply return the folded state (for `foldr`) or `mempty` (for
    `foldMap`), without inspecting the arguments.
  * Add `FFTOptions` (`Functor`/`Foldable`/`Traversable` options) to
    `Data.Functor.Deriving`, along with variants of existing functions that
    take `FFTOptions` as an argument. For now, the only configurable option is
    whether derived instances for empty data types should use the `EmptyCase`
    extension (this is disabled by default).
* Backport the fix to #13328. That is, when deriving `Functor` or
  `Traversable` instances for data types where the last type variable is at
  phantom role, generated `fmap`/`traverse` implementations now use `coerce`
  for efficiency.
* Rename `emptyCaseBehavior` from `Data.Functor.Deriving` to
  `fftEmptyCaseBehavior`.

### 0.3.6 [2017.04.10]
* Make `deriveTraversable` use `liftA2` in derived implementations of
  `traverse` when possible, now that `liftA2` is a class method of
  `Applicative` (as of GHC 8.2)
* Make `deriveShow` use `showCommaSpace`, a change introduced in GHC 8.2

### 0.3.5 [2016.12.12]
* Fix bug in which derived `Ord` instances for datatypes with many constructors
  would fail to typecheck

### 0.3.4 [2016.10.20]
* Fix bug in which infix record selectors weren't shown with parentheses in derived `Show` instances
* Fix bug in which record selectors weren't parsed correctly in derived `Read` instances

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
