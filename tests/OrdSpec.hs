{-|
Module:      OrdSpec
Copyright:   (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Ord', 'Ord1', and 'Ord2' instances.
-}
module OrdSpec where

import Prelude ()
import Prelude.Compat

import Test.Hspec

import Types.EqOrd ()

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()

