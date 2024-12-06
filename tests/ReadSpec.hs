{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module:      ReadSpec
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Read', 'Read1', and 'Read2' instances.
-}
module ReadSpec where

import Data.Deriving
import Data.Functor.Classes (Read1, readsPrec1)
import Data.Proxy

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(..))

import Text.Read (minPrec)

import Types.ReadShow ()

-------------------------------------------------------------------------------

-- Plain data types

data TyCon# a b = TyCon# {
    tcA# :: a
  , tcB# :: b
} deriving (Eq, Show)

data Empty a b

-- Data families

data family TyFamily# y z :: *

data instance TyFamily# a b = TyFamily# {
    tfA# :: a
  , tfB# :: b
} deriving (Eq, Show)

-------------------------------------------------------------------------------

-- Plain data types

$(deriveRead  ''TyCon#)
$(deriveRead1 ''TyCon#)
$(deriveRead2 ''TyCon#)

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyCon# a b) where
  arbitrary = TyCon# <$> arbitrary <*> arbitrary

$(deriveRead  ''Empty)
$(deriveRead1 ''Empty)
$(deriveRead2 ''Empty)

-- Data families

$(deriveRead  'TyFamily#)
$(deriveRead1 'TyFamily#)
$(deriveRead2 'TyFamily#)

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamily# a b) where
  arbitrary = TyFamily# <$> arbitrary <*> arbitrary

-------------------------------------------------------------------------------

prop_Read :: forall f a. (Read a, Read (f a), Read1 f,
                          Eq (f a), Show (f a))
          => f a -> Expectation
prop_Read x = readArb readsPrec `shouldBe` readArb readsPrec1
  where
    readArb :: (Int -> ReadS (f a)) -> f a
    readArb = read' (show x)

readSpec :: forall f a. (Arbitrary (f a), Eq (f a), Show (f a),
                         Read a, Read (f a), Read1 f)
         => Proxy (f a) -> Spec
readSpec _ = prop "has a valid Read1 instance" (prop_Read :: f a -> Expectation)

-- Adapted from the definition of readEither
readEither' :: String -> (Int -> ReadS a) -> Either String a
readEither' s rs =
  case [ x | (x,"") <- rs minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"

read' :: String -> (Int -> ReadS a) -> a
read' s = either error id . readEither' s

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "TyCon#" $
    readSpec (Proxy :: Proxy (TyCon# Char Int))
  describe "TyFamily#" $
    readSpec (Proxy :: Proxy (TyFamily# Char Int))
