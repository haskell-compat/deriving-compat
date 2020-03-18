{-|
Module:      Data.Functor.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Functor' instances.

For more info on how deriving @Functor@ works, see
<https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor this GHC wiki page>.
-}
module Data.Functor.Deriving (
      -- * 'Functor'
      deriveFunctor
    , deriveFunctorOptions
    , makeFmap
    , makeFmapOptions
    , makeReplace
    , makeReplaceOptions
      -- * 'FFTOptions'
    , FFTOptions(..)
    , defaultFFTOptions
      -- * 'deriveFunctor' limitations
      -- $constraints
    ) where

import Data.Functor.Deriving.Internal

{- $constraints

Be aware of the following potential gotchas:

* Type variables of kind @* -> *@ are assumed to have 'Functor' constraints.
  If this is not desirable, use 'makeFmap'.
-}
