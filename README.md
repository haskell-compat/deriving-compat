# `deriving-compat` [![Hackage version](https://img.shields.io/hackage/v/deriving-compat.svg?style=flat)](http://hackage.haskell.org/package/deriving-compat) [![Build Status](https://img.shields.io/travis/haskell-compat/deriving-compat.svg?style=flat)](https://travis-ci.org/haskell-compat/deriving-compat)

Provides Template Haskell functions that mimic deriving extensions that were introduced or modified in recent versions of GHC. Currently, the following extensions are covered:

* `DeriveFoldable`, which was changed in GHC 7.12 to allow folding over data types with existential constraints
