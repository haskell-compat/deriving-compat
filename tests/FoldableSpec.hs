{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{-|
Module:      FoldableSpec
Copyright:   (C) 2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for the "Data.Foldable.Deriving" module.
-}
module FoldableSpec where

import Data.Foldable (fold)
import Data.Foldable.Deriving
import Data.Monoid

import Prelude.Compat

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary)

-------------------------------------------------------------------------------

-- Adapted from the test cases from
-- https://ghc.haskell.org/trac/ghc/attachment/ticket/2953/deriving-functor-tests.patch

data Strange a b c
    = T1 a b c
    | T2 [a] [b] [c]         -- lists
    | T3 [[a]] [[b]] [[c]]   -- nested lists
    | T4 (c,(b,b),(c,c))     -- tuples
    | T5 ([c],Strange a b c) -- tycons

data StrangeGADT a b where
    T10 :: Ord b            => b        -> StrangeGADT a b
    T11 ::                     Int      -> StrangeGADT a Int
    T12 :: c ~ Int          => c        -> StrangeGADT a Int
    T13 :: b ~ Int          => Int      -> StrangeGADT a b
    T14 :: b ~ Int          => b        -> StrangeGADT a b
    T15 :: (b ~ c, c ~ Int) => Int -> c -> StrangeGADT a b

data NotPrimitivelyRecursive a b
    = S1 (NotPrimitivelyRecursive (a,a) (b, a))
    | S2 a
    | S3 b

newtype Compose f g a = Compose (f (g a))
  deriving (Arbitrary, Eq, Show)

newtype ComplexConstraint f g a b = ComplexConstraint (f Int Int (g a, a, b))

type Flip f a b = f b a
data Existential a b
    = forall a. ExistentialList [a]
    | forall f. Foldable (f a) => ExistentialFoldable (Flip f b a)
    | forall b. SneakyUseSameName (Maybe b)

-------------------------------------------------------------------------------

$(deriveFoldable ''Strange)
$(deriveFoldable ''StrangeGADT)
$(deriveFoldable ''NotPrimitivelyRecursive)
$(deriveFoldable ''Compose)

instance (Foldable (f Int Int), Foldable g) =>
  Foldable (ComplexConstraint f g a) where
    foldr   = $(makeFoldr ''ComplexConstraint)
    foldMap = $(makeFoldMap ''ComplexConstraint)

$(deriveFoldable ''Existential)

-------------------------------------------------------------------------------

prop_FoldableLaws :: (Eq a, Eq b, Eq z, Monoid a, Monoid b, Foldable f)
                => (a -> b) -> (a -> z -> z) -> z -> f a -> Bool
prop_FoldableLaws f h z x =
       fold      x == foldMap id x
    && foldMap f x == foldr (mappend . f) mempty x
    && foldr h z x == appEndo (foldMap (Endo . h) x) z

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Compose Maybe (Either Char) [Int]" $
        prop "satisfies the Foldable laws"
            (prop_FoldableLaws
                reverse
                ((+) . length)
                0
                :: Compose Maybe (Either Char) [Int] -> Bool)
