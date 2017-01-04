{-# LANGUAGE CPP #-}
{-|
Module:      Data.Ord.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Ord', 'Ord1', and 'Ord2' instances.
-}
module Data.Ord.Deriving (
      -- * 'Ord'
      deriveOrd
    , makeCompare
    , makeLT
    , makeLE
    , makeGT
    , makeGE
    , makeMax
    , makeMin
      -- * 'Ord1'
    , deriveOrd1
#if defined(NEW_FUNCTOR_CLASSES)
    , makeLiftCompare
#endif
    , makeCompare1
#if defined(NEW_FUNCTOR_CLASSES)
      -- * 'Ord2'
    , deriveOrd2
    , makeLiftCompare2
    , makeCompare2
#endif
      -- * 'deriveOrd' limitations
      -- $constraints
    ) where

import Data.Ord.Deriving.Internal

{- $constraints

Be aware of the following potential gotchas:

* Type variables of kind @*@ are assumed to have 'Ord' constraints.
  Type variables of kind @* -> *@ are assumed to have 'Ord1' constraints.
  Type variables of kind @* -> * -> *@ are assumed to have 'Ord2' constraints.
  If this is not desirable, use 'makeCompare' or one of its cousins.

* The 'Ord1' class had a different definition in @transformers-0.4@, and as a result,
  'deriveOrd1' implements different instances for the @transformers-0.4@ 'Ord1' than
  it otherwise does. Also, 'makeLiftCompare' is not available
  when this library is built against @transformers-0.4@, only 'makeCompare1.

* The 'Ord2' class is not available in @transformers-0.4@, and as a
  result, neither are Template Haskell functions that deal with 'Ord2' when this
  library is built against @transformers-0.4@.
-}
