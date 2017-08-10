{-|
Module:      Data.Traversable.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Traversable' instances in a way that mimics
how the @-XDeriveTraversable@ extension works since GHC 8.0.

Derived 'Traversable' instances from this module do not generate
superfluous 'pure' expressions in its implementation of 'traverse'. One can
verify this by compiling a module that uses 'deriveTraversable' with the
@-ddump-splices@ GHC flag.

These changes make it possible to derive @Traversable@ instances for data types with
unlifted argument types, e.g.,

@
data IntHash a = IntHash Int# a

deriving instance Traversable IntHash -- On GHC 8.0  on later
$(deriveTraversable ''IntHash)        -- On GHC 7.10 and earlier
@

For more info on these changes, see
<https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor this GHC wiki page>.
-}
module Data.Traversable.Deriving (
      -- * 'Traversable'
      deriveTraversable
    , deriveTraversableOptions
    , makeTraverse
    , makeTraverseOptions
    , makeSequenceA
    , makeSequenceAOptions
    , makeMapM
    , makeMapMOptions
    , makeSequence
    , makeSequenceOptions
      -- * 'FFTOptions'
    , FFTOptions(..)
    , defaultFFTOptions
      -- * 'deriveTraversable' limitations
      -- $constraints
    ) where

import Data.Functor.Deriving.Internal

{- $constraints

Be aware of the following potential gotchas:

* If you are using the @-XGADTs@ or @-XExistentialQuantification@ extensions, an
  existential constraint cannot mention the last type variable. For example,
  @data Illegal a = forall a. Show a => Illegal a@ cannot have a derived
  'Traversable' instance.

* Type variables of kind @* -> *@ are assumed to have 'Traversable' constraints.
  If this is not desirable, use 'makeTraverse'.
-}
