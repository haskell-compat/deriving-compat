{-|
Module:      Data.Enum.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Enum' instances.
-}
module Data.Enum.Deriving (
      -- * 'Enum'
      deriveEnum
    , makeSucc
    , makePred
    , makeToEnum
    , makeFromEnum
    , makeEnumFrom
    , makeEnumFromThen
      -- * 'deriveEnum' limitations
      -- $constraints
    ) where

import Data.Enum.Deriving.Internal

{- $constraints

Be aware of the following potential gotchas:

* Type variables of kind @*@ are assumed to have 'Enum' constraints.
  If this is not desirable, use 'makeToEnum' or one of its cousins.

* Generated 'Enum' instances for poly-kinded data family instances are likely
  to require the use of the @TypeInType@ extension on GHC 8.0, 8.2, or 8.4.
-}
