{-# LANGUAGE TemplateHaskell #-}

{-|
Module:      GH6Spec
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

A regression test for
https://github.com/haskell-compat/deriving-compat/issues/6.
-}
module GH6Spec (main, spec) where

import Data.Deriving (deriveEq1, deriveOrd1)
import Data.Proxy (Proxy(..))

import OrdSpec (ordSpec)

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck (Arbitrary(..), oneof)

data Foo a
 = Foo1  a
 | Foo2  a
 | Foo3  a
 | Foo4  a
 | Foo5  a
 deriving (Eq, Ord, Show)

deriveEq1  ''Foo
deriveOrd1 ''Foo

instance Arbitrary a => Arbitrary (Foo a) where
  arbitrary = oneof $ map (<$> arbitrary) [Foo1, Foo2, Foo3, Foo4, Foo5]

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ describe "GH6" $ ordSpec (Proxy :: Proxy (Foo Int))
