{-# LANGUAGE CPP #-}
{-|
Module:      Data.Eq.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Eq', 'Eq1', and 'Eq2' instances.
-}
module Data.Eq.Deriving (
      -- * 'Eq'
      deriveEq
    , makeEq
    , makeNotEq
      -- * 'Eq1'
    , deriveEq1
#if defined(NEW_FUNCTOR_CLASSES)
    , makeLiftEq
#endif
    , makeEq1
#if defined(NEW_FUNCTOR_CLASSES)
      -- * 'Eq2'
    , deriveEq2
    , makeLiftEq2
    , makeEq2
#endif
      -- * 'deriveEq' limitations
      -- $constraints
    ) where

import Data.Eq.Deriving.Internal

{- $constraints

Be aware of the following potential gotchas:

* Type variables of kind @*@ are assumed to have 'Eq' constraints.
  Type variables of kind @* -> *@ are assumed to have 'Eq1' constraints.
  Type variables of kind @* -> * -> *@ are assumed to have 'Eq2' constraints.
  If this is not desirable, use 'makeEq' or one of its cousins.

* The 'Eq1' class had a different definition in @transformers-0.4@, and as a result,
  'deriveEq1' implements different instances for the @transformers-0.4@ 'Eq1' than
  it otherwise does. Also, 'makeLiftEq' is not available
  when this library is built against @transformers-0.4@, only 'makeEq1.

* The 'Eq2' class is not available in @transformers-0.4@, and as a
  result, neither are Template Haskell functions that deal with 'Eq2' when this
  library is built against @transformers-0.4@.
-}
