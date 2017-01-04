{-|
Module:      Data.Bounded.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Bounded' instances.
-}
module Data.Bounded.Deriving (
      -- * 'Bounded'
      deriveBounded
    , makeMinBound
    , makeMaxBound
      -- * 'deriveBounded' limitations
      -- $constraints
    ) where

import Data.Bounded.Deriving.Internal

{- $constraints

Be aware of the following potential gotchas:

* Type variables of kind @*@ are assumed to have 'Bounded' constraints.
  If this is not desirable, use 'makeMinBound' or one of its cousins.
-}
