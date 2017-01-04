{-# LANGUAGE CPP #-}
{-|
Module:      Text.Read.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Read', 'Read1', and 'Read2' instances.
-}
module Text.Read.Deriving (
      -- * 'Read'
      deriveRead
    , deriveReadOptions
    , makeReadsPrec
--     , makeReadsPrecOptions
--     , makeReadList
--     , makeReadListOptions
    , makeReadPrec
--     , makeReadPrecOptions
--     , makeReadListPrec
--     , makeReadListPrecOptions
      -- * 'Read1'
    , deriveRead1
    , deriveRead1Options
#if defined(NEW_FUNCTOR_CLASSES)
    , makeLiftReadsPrec
--     , makeLiftReadsPrecOptions
--     , makeLiftReadList
--     , makeLiftReadListOptions
# if __GLASGOW_HASKELL__ >= 801
    , makeLiftReadPrec
--     , makeLiftReadPrecOptions
--     , makeLiftReadListPrec
--     , makeLiftReadListPrecOptions
    , makeReadPrec1
--     , makeReadPrec1Options
# endif
#endif
    , makeReadsPrec1
--     , makeReadsPrec1Options
#if defined(NEW_FUNCTOR_CLASSES)
      -- * 'Read2'
    , deriveRead2
    , deriveRead2Options
    , makeLiftReadsPrec2
--     , makeLiftReadsPrec2Options
--     , makeLiftReadList2
--     , makeLiftReadList2Options
# if __GLASGOW_HASKELL__ >= 801
    , makeLiftReadPrec2
--     , makeLiftReadPrec2Options
--     , makeLiftReadListPrec2
--     , makeLiftReadListPrec2Options
    , makeReadPrec2
--     , makeReadPrec2Options
# endif
    , makeReadsPrec2
--     , makeReadsPrec2Options
#endif
      -- * 'ReadOptions'
    , ReadOptions(..)
    , defaultReadOptions
      -- * 'deriveRead' limitations
      -- $constraints
    ) where

import Text.Read.Deriving.Internal

{- $constraints

Be aware of the following potential gotchas:

* Type variables of kind @*@ are assumed to have 'Read' constraints.
  Type variables of kind @* -> *@ are assumed to have 'Read1' constraints.
  Type variables of kind @* -> * -> *@ are assumed to have 'Read2' constraints.
  If this is not desirable, use 'makeReadsPrec' or one of its cousins.

* The 'Read1' class had a different definition in @transformers-0.4@, and as a result,
  'deriveRead1' implements different instances for the @transformers-0.4@ 'Read1' than
  it otherwise does. Also, 'makeLiftReadsPrec' and 'makeLiftReadList' are not available
  when this library is built against @transformers-0.4@, only 'makeReadsPrec1.

* The 'Read2' class is not available in @transformers-0.4@, and as a
  result, neither are Template Haskell functions that deal with 'Read2' when this
  library is built against @transformers-0.4@.

* The 'Read1' and 'Read2' classes have new methods ('liftReadPrec'/'liftReadListPrec'
  and 'liftReadPrec2'/'liftReadListPrec2', respectively) that were introduced in
  @base-4.10@. For now, these methods are only defined when deriving 'Read1'/'Read2'
  if built against @base-4.10@ (until @transformers-compat@ catches up), and
  the corresponding @make-@ functions are also only available when built against
  @base-4.10@.
-}
