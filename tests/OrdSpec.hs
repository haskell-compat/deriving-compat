{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module:      OrdSpec
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Ord', 'Ord1', and 'Ord2' instances.
-}
module OrdSpec where

import Data.Functor.Classes

import Prelude ()
import Prelude.Compat

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary)

import Types.EqOrd ()

-------------------------------------------------------------------------------

prop_Ord :: (Ord a, Ord (f a), Ord1 f) => f a -> f a -> Bool
prop_Ord x y = compare x y == compare1 x y

ordSpec :: forall proxy f a. (Arbitrary (f a), Show (f a),
                              Ord a, Ord (f a), Ord1 f)
        => proxy (f a) -> Spec
ordSpec _ = prop "has a valid Ord1 instance" (prop_Ord :: f a -> f a -> Bool)

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()
