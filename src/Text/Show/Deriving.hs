{-|
Module:      Text.Show.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Show', 'Show1', and 'Show2' instances.
Note that upstream GHC does not have the ability to derive 'Show1' or 'Show2'
instances, but since the functionality to derive 'Show' extends very naturally
'Show1' and 'Show2', the ability to derive the latter two classes is provided as a
convenience.
-}
module Text.Show.Deriving (
      -- * 'Show'
      deriveShow
    , deriveShowOptions
    , makeShowsPrec
    , makeShowsPrecOptions
    , makeShow
    , makeShowOptions
    , makeShowList
    , makeShowListOptions
      -- * 'Show1'
    , deriveShow1
    , deriveShow1Options
    , makeLiftShowsPrec
    , makeLiftShowsPrecOptions
    , makeLiftShowList
    , makeLiftShowListOptions
    , makeShowsPrec1
    , makeShowsPrec1Options
      -- * 'Show2'
    , deriveShow2
    , deriveShow2Options
    , makeLiftShowsPrec2
    , makeLiftShowsPrec2Options
    , makeLiftShowList2
    , makeLiftShowList2Options
    , makeShowsPrec2
    , makeShowsPrec2Options
      -- * 'ShowOptions'
    , ShowOptions(..)
    , defaultShowOptions
    , legacyShowOptions
      -- * 'deriveShow' limitations
      -- $constraints
    ) where

import Text.Show.Deriving.Internal

{- $constraints

Be aware of the following potential gotchas:

* Type variables of kind @*@ are assumed to have 'Show' constraints.
  Type variables of kind @* -> *@ are assumed to have 'Show1' constraints.
  Type variables of kind @* -> * -> *@ are assumed to have 'Show2' constraints.
  If this is not desirable, use 'makeShowsPrec' or one of its cousins.

* The 'Show1' class had a different definition in @transformers-0.4@, and as a result,
  'deriveShow1' implements different instances for the @transformers-0.4@ 'Show1' than
  it otherwise does. Also, 'makeLiftShowsPrec' and 'makeLiftShowList' are not available
  when this library is built against @transformers-0.4@, only 'makeShowsPrec1.

* The 'Show2' class is not available in @transformers-0.4@, and as a
  result, neither are Template Haskell functions that deal with 'Show2' when this
  library is built against @transformers-0.4@.
-}
