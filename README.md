# `deriving-compat`
[![Hackage](https://img.shields.io/hackage/v/deriving-compat.svg)][Hackage: deriving-compat]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/deriving-compat.svg)](http://packdeps.haskellers.com/reverse/deriving-compat)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/haskell-compat/deriving-compat/workflows/Haskell-CI/badge.svg)](https://github.com/haskell-compat/deriving-compat/actions?query=workflow%3AHaskell-CI)

[Hackage: deriving-compat]:
  http://hackage.haskell.org/package/deriving-compat
  "deriving-compat package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

`deriving-compat` provides Template Haskell functions that mimic `deriving` extensions that were introduced or modified in recent versions of GHC. Currently, the following typeclasses/extensions are covered:

* Deriving `Bounded`
* Deriving `Enum`
* Deriving `Ix`
* Deriving `Eq`
* Deriving `Ord`
* Deriving `Read`
* Deriving `Show`
* `DeriveFoldable`
* `DeriveFunctor`
* `DeriveTraversable`
* `GeneralizedNewtypeDeriving` (with GHC 8.2 or later)
* `DerivingVia` (with GHC 8.2 or later)

See the `Data.Deriving` module for a full list of backported changes.

In addition, `deriving-compat` also provides some additional `deriving` functionality that has not yet been merged into upstream GHC. Aside from the GHC `deriving` extensions mentioned above, `deriving-compat` also permits deriving instances of classes in the `Data.Functor.Classes` module, covering the `Eq1`, `Eq2`, `Ord1`, `Ord2`, `Read1`, `Read2`, `Show1`, and `Show2` classes. This extra functionality is outside of the main scope of `deriving-compat`, as it does not backport extensions that exist in today's GHC. Nevertheless, the underlying Template Haskell machinery needed to derive `Eq` and friends extends very naturally to `Eq1` and friends, so this extra functionality is included in `deriving-compat` as a convenience.

Note that some recent GHC typeclasses/extensions are not covered by this package:

* `DeriveDataTypeable`
* `DeriveGeneric`, which was introducted in GHC 7.2 for deriving `Generic` instances, and modified in GHC 7.6 to allow derivation of `Generic1` instances. Use `Generics.Deriving.TH` from [`generic-deriving`](http://hackage.haskell.org/package/generic-deriving) to derive `Generic(1)` using Template Haskell.
* `DeriveLift`, which was introduced in GHC 8.0 for deriving `Lift` instances. Use `Language.Haskell.TH.Lift` from [`th-lift`](http://hackage.haskell.org/package/th-lift) to derive `Lift` using Template Haskell.
* The `Bifunctor` typeclass, which was introduced in GHC 7.10, as well as the `Bifoldable` and `Bitraversable` typeclasses, which were introduced in GHC 8.2. Use `Data.Bifunctor.TH` from [`bifunctors`](http://hackage.haskell.org/package/bifunctors) to derive these typeclasses using Template Haskell.
