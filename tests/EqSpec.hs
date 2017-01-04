{-|
Module:      EqSpec
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Eq', 'Eq1', and 'Eq2' instances.
-}
module EqSpec where

import Prelude ()
import Prelude.Compat

import Test.Hspec

import Types.EqOrd ()

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()
