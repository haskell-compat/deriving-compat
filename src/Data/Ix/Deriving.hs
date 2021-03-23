{-|
Module:      Data.Ix.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Ix' instances.
-}
module Data.Ix.Deriving (
      -- * 'Ix'
      deriveIx
    , makeRange
    , makeUnsafeIndex
    , makeInRange
      -- * 'deriveIx' limitations
      -- $constraints
    ) where

import Data.Ix.Deriving.Internal

{- $constraints

Be aware of the following potential gotchas:

* Type variables of kind @*@ are assumed to have 'Ix' constraints.
  If this is not desirable, use 'makeRange' or one of its cousins.

* Generated 'Ix' instances for poly-kinded data family instances are likely
  to require the use of the @TypeInType@ extension on GHC 8.0, 8.2, or 8.4.
-}
