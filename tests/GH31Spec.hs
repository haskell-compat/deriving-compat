{-# LANGUAGE TemplateHaskell #-}

{-|
Module:      GH31Spec
Copyright:   (C) 2020 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

A regression test for
https://github.com/haskell-compat/deriving-compat/issues/31.
-}
module GH31Spec (main, spec) where

import Data.Deriving (deriveEq1, deriveOrd1)
import Data.Functor.Classes (compare1)
import Data.Proxy (Proxy(..))
import Data.Void (Void)

import OrdSpec (ordSpec)

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)
import Test.QuickCheck (Arbitrary(..), oneof)

data T a
  = A
  | B Int
  | C Int
  | D
  | E Int
  | F
  deriving (Eq, Ord, Show)

deriveEq1 ''T
deriveOrd1 ''T

instance Arbitrary (T a) where
  arbitrary = oneof [ pure A
                    , B <$> arbitrary
                    , C <$> arbitrary
                    , pure D
                    , E <$> arbitrary
                    , pure F
                    ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
  describe "GH31" $ do
    ordSpec (Proxy :: Proxy (T Void))
    it "obeys reflexivity" $
      let x :: T Void
          x = E 0
      in compare1 x x `shouldBe` EQ
