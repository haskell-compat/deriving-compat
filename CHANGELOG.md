## 0.3
* Added `Data.Eq.Deriving`, which allows deriving `Eq`, `Eq1`, and `Eq2` with TH.
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
