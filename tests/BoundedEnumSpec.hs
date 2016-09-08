{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module:      BoundedEnumSpec
Copyright:   (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Bounded' and 'Enum' instances.
-}
module BoundedEnumSpec where

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

data TyConUnit a = TyConUnit
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

data family TyFamilyUnit x :: *
data instance TyFamilyUnit a = TyFamilyUnit
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
instance Bounded (TyConUnit a) where
    minBound = $(makeMinBound ''TyConUnit)
    maxBound = $(makeMaxBound ''TyConUnit)
instance (Bounded a, Show a) => Bounded (TyConExQuant a) where
    minBound = $(makeMinBound ''TyConExQuant)
    maxBound = $(makeMaxBound ''TyConExQuant)
instance (Bounded a, Show a) => Bounded (TyConGADT a) where
    minBound = $(makeMinBound ''TyConGADT)
    maxBound = $(makeMaxBound ''TyConGADT)

$(deriveEnum ''TyConEnum)
instance Enum (TyConUnit a) where
    toEnum   = $(makeToEnum   ''TyConUnit)
    fromEnum = $(makeFromEnum ''TyConUnit)

#if MIN_VERSION_template_haskell(2,7,0)
-- Data families

$(deriveBounded 'TyFamilyEnum1)
$(deriveBounded 'TyFamilyProduct)
instance Bounded (TyFamilyUnit a) where
    minBound = $(makeMinBound 'TyFamilyUnit)
    maxBound = $(makeMaxBound 'TyFamilyUnit)
instance (Bounded a, Show a) => Bounded (TyFamilyExQuant a) where
    minBound = $(makeMinBound 'TyFamilyExQuant)
    maxBound = $(makeMaxBound 'TyFamilyExQuant)
instance (Bounded a, Show a) => Bounded (TyFamilyGADT a) where
    minBound = $(makeMinBound 'TyFamilyGADT)
    maxBound = $(makeMaxBound 'TyFamilyGADT)

$(deriveEnum 'TyFamilyEnum1)
instance Enum (TyFamilyUnit a) where
    toEnum   = $(makeToEnum   'TyFamilyUnit)
    fromEnum = $(makeFromEnum 'TyFamilyUnit)
#endif

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyConEnum" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConEnum1
            maxBound `shouldBe` TyConEnum3

        it "has a sensible Enum instance" $
            [minBound .. maxBound] `shouldBe` [TyConEnum1, TyConEnum2, TyConEnum3]
    describe "TyConProduct Bool Bool Bool" $
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConProduct False False False
            maxBound `shouldBe` TyConProduct True  True  True
    describe "TyConUnit Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConUnit
            maxBound `shouldBe` TyConUnit

        it "has a sensible Enum instance" $
            [minBound .. maxBound] `shouldBe` [TyConUnit]
    describe "TyConExQuant Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConExQuant False
            maxBound `shouldBe` TyConExQuant True
    describe "TyConGADT Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConGADT False
            maxBound `shouldBe` TyConGADT True
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamilyEnum" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyEnum1
            maxBound `shouldBe` TyFamilyEnum3

        it "has a sensible Enum instance" $
            [minBound .. maxBound] `shouldBe` [TyFamilyEnum1, TyFamilyEnum2, TyFamilyEnum3]
    describe "TyFamilyProduct Bool Bool Bool" $
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyProduct False False False
            maxBound `shouldBe` TyFamilyProduct True  True  True
    describe "TyFamilyUnit Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyUnit
            maxBound `shouldBe` TyFamilyUnit

        it "has a sensible Enum instance" $
            [minBound .. maxBound] `shouldBe` [TyFamilyUnit]
    describe "TyFamilyExQuant Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyExQuant False
            maxBound `shouldBe` TyFamilyExQuant True
    describe "TyFamilyGADT Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyGADT False
            maxBound `shouldBe` TyFamilyGADT True
#endif
