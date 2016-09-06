{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module:      BoundedSpec
Copyright:   (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Bounded' instances.
-}
module BoundedSpec where

import Data.Deriving

import Prelude ()
import Prelude.Compat

import Test.Hspec

-------------------------------------------------------------------------------

-- Plain data types

data TyConEnum = TyConEnum1 | TyConEnum2 | TyConEnum3
  deriving (Eq, Show)

data TyConProduct a b c = TyConProduct a b c
  deriving (Eq, Show)

data TyConUnit = TyConUnit
  deriving (Eq, Show)

data TyConExQuant a = Show a => TyConExQuant a
deriving instance Eq   a => Eq   (TyConExQuant a)
deriving instance Show a => Show (TyConExQuant a)

data TyConGADT a where
    TyConGADT :: Show a => a -> TyConGADT a
deriving instance Eq   a => Eq   (TyConGADT a)
deriving instance Show a => Show (TyConGADT a)

-- Data families

data family TyFamilyEnum :: *
data instance TyFamilyEnum = TyFamilyEnum1 | TyFamilyEnum2 | TyFamilyEnum3
  deriving (Eq, Show)

data family TyFamilyProduct x y z :: *
data instance TyFamilyProduct a b c = TyFamilyProduct a b c
  deriving (Eq, Show)

data family TyFamilyUnit :: *
data instance TyFamilyUnit = TyFamilyUnit
  deriving (Eq, Show)

data family TyFamilyExQuant x :: *
data instance TyFamilyExQuant a = Show a => TyFamilyExQuant a
deriving instance Eq   a => Eq   (TyFamilyExQuant a)
deriving instance Show a => Show (TyFamilyExQuant a)

data family TyFamilyGADT x :: *
data instance TyFamilyGADT a where
    TyFamilyGADT :: Show a => a -> TyFamilyGADT a
deriving instance Eq   a => Eq   (TyFamilyGADT a)
deriving instance Show a => Show (TyFamilyGADT a)

-------------------------------------------------------------------------------

-- Plain data types

$(deriveBounded ''TyConEnum)
$(deriveBounded ''TyConProduct)
$(deriveBounded ''TyConUnit)
instance (Bounded a, Show a) => Bounded (TyConExQuant a) where
    minBound = $(makeMinBound ''TyConExQuant)
    maxBound = $(makeMaxBound ''TyConExQuant)
instance (Bounded a, Show a) => Bounded (TyConGADT a) where
    minBound = $(makeMinBound ''TyConGADT)
    maxBound = $(makeMaxBound ''TyConGADT)

#if MIN_VERSION_template_haskell(2,7,0)
-- Data families

$(deriveBounded 'TyFamilyEnum1)
$(deriveBounded 'TyFamilyProduct)
$(deriveBounded 'TyFamilyUnit)
instance (Bounded a, Show a) => Bounded (TyFamilyExQuant a) where
    minBound = $(makeMinBound 'TyFamilyExQuant)
    maxBound = $(makeMaxBound 'TyFamilyExQuant)
instance (Bounded a, Show a) => Bounded (TyFamilyGADT a) where
    minBound = $(makeMinBound 'TyFamilyGADT)
    maxBound = $(makeMaxBound 'TyFamilyGADT)
#endif

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyConEnum" $
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConEnum1
            maxBound `shouldBe` TyConEnum3
    describe "TyConProduct Bool Bool Bool" $
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConProduct False False False
            maxBound `shouldBe` TyConProduct True  True  True
    describe "TyConUnit" $
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConUnit
            maxBound `shouldBe` TyConUnit
    describe "TyConExQuant Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConExQuant False
            maxBound `shouldBe` TyConExQuant True
    describe "TyConGADT Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConGADT False
            maxBound `shouldBe` TyConGADT True
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamilyEnum" $
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyEnum1
            maxBound `shouldBe` TyFamilyEnum3
    describe "TyFamilyProduct Bool Bool Bool" $
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyProduct False False False
            maxBound `shouldBe` TyFamilyProduct True  True  True
    describe "TyFamilyUnit" $
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyUnit
            maxBound `shouldBe` TyFamilyUnit
    describe "TyFamilyExQuant Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyExQuant False
            maxBound `shouldBe` TyFamilyExQuant True
    describe "TyFamilyGADT Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyGADT False
            maxBound `shouldBe` TyFamilyGADT True
#endif
