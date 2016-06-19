# `deriving-compat`
[![Hackage](https://img.shields.io/hackage/v/deriving-compat.svg)][Hackage: deriving-compat]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/deriving-compat.svg)](http://packdeps.haskellers.com/reverse/deriving-compat)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/haskell-compat/deriving-compat.svg)](https://travis-ci.org/haskell-compat/deriving-compat)

[Hackage: deriving-compat]:
  http://hackage.haskell.org/package/deriving-compat
  "deriving-compat package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

Provides Template Haskell functions that mimic deriving extensions that were introduced or modified in recent versions of GHC. Currently, the following extensions are covered:

* `DeriveFoldable`
* `DeriveFunctor`
* `DeriveTraversable`

The following changes have been backported:

* In GHC 8.0, `DeriveFoldable` was changed to allow folding over data types with existential constraints.
* In GHC 8.0, `DeriveFoldable` and `DeriveTraversable` were changed so as not to generate superfluous `mempty` or `pure` expressions in generated code. As a result, this allows deriving `Traversable` instances for datatypes with unlifted argument types.
* In GHC 8.0, deriving `Show` was changed so that constructor fields with unlifted types are no longer shown with parentheses, and the output of showing an unlifted type is suffixed with the same number of hash signs as the corresponding primitive literals.

Note that some recent GHC extensions are not covered by this package:

* `DeriveGeneric`, which was introducted in GHC 7.2 for deriving `Generic` instances, and modified in GHC 7.6 to allow derivation of `Generic1` instances. Use `Generics.Deriving.TH` from [`generic-deriving`](http://hackage.haskell.org/package/generic-deriving) to derive `Generic(1)` using Template Haskell.
* `DeriveLift`, which was introduced in GHC 8.0 for deriving `Lift` instances. Use `Language.Haskell.TH.Lift` from [`th-lift`](http://hackage.haskell.org/package/th-lift) to derive `Lift` using Template Haskell.
