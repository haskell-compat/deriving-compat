{-|
Module:      Data.Foldable.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Foldable' instances in a way that mimics
how the @-XDeriveFoldable@ extension works since GHC 8.0.

These changes make it possible to derive @Foldable@ instances for data types with
existential constraints, e.g.,

@
data WrappedSet a where
    WrapSet :: Ord a => a -> WrappedSet a

deriving instance Foldable WrappedSet -- On GHC 8.0  on later
$(deriveFoldable ''WrappedSet)        -- On GHC 7.10 and earlier
@

In addition, derived 'Foldable' instances from this module do not generate
superfluous 'mempty' expressions in its implementation of 'foldMap'. One can
verify this by compiling a module that uses 'deriveFoldable' with the
@-ddump-splices@ GHC flag.

For more info on these changes, see
<https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor this GHC wiki page>.
-}
module Data.Foldable.Deriving (
      -- * 'Foldable'
      deriveFoldable
    , deriveFoldableOptions
    , makeFoldMap
    , makeFoldMapOptions
    , makeFoldr
    , makeFoldrOptions
    , makeFold
    , makeFoldOptions
    , makeFoldl
    , makeFoldlOptions
    , makeNull
    , makeNullOptions
      -- * 'FFTOptions'
    , FFTOptions(..)
    , defaultFFTOptions
      -- * 'deriveFoldable' limitations
      -- $constraints
    ) where

import Data.Functor.Deriving.Internal

{- $constraints

Be aware of the following potential gotchas:

* If you are using the @-XGADTs@ or @-XExistentialQuantification@ extensions, an
  existential constraint cannot mention the last type variable. For example,
  @data Illegal a = forall a. Show a => Illegal a@ cannot have a derived
  'Functor' instance.

* Type variables of kind @* -> *@ are assumed to have 'Foldable' constraints.
  If this is not desirable, use 'makeFoldr' or 'makeFoldMap'.
-}
