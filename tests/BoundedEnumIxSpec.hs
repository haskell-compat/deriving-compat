{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

{-|
Module:      BoundedEnumSpec
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Bounded', 'Enum', and 'Ix' instances.
-}
module BoundedEnumIxSpec where

import Data.Deriving

import GHC.Arr (Ix(..))

import Prelude ()
import Prelude.Compat

import Test.Hspec

-------------------------------------------------------------------------------

-- Plain data types

data TyConEnum = TyConEnum1 | TyConEnum2 | TyConEnum3
  deriving (Eq, Ord, Show)

data TyConProduct a b c = TyConProduct a b c
  deriving (Eq, Ord, Show)

data TyConUnit
#if __GLASGOW_HASKELL__ >= 706
    (f :: k -> *) (a :: k)
#else
    (f :: * -> *) (a :: *)
#endif
    = TyConUnit
  deriving (Eq, Ord, Show)

data TyConExQuant a = Show a => TyConExQuant
deriving instance Eq   (TyConExQuant a)
deriving instance Ord  (TyConExQuant a)
deriving instance Show (TyConExQuant a)

data TyConGADT a where
    TyConGADT :: Show a => a -> TyConGADT a
deriving instance Eq   a => Eq   (TyConGADT a)
deriving instance Ord  a => Ord  (TyConGADT a)
deriving instance Show a => Show (TyConGADT a)

-- Data families

data family TyFamilyEnum :: *
data instance TyFamilyEnum = TyFamilyEnum1 | TyFamilyEnum2 | TyFamilyEnum3
  deriving (Eq, Ord, Show)

data family TyFamilyProduct x y z :: *
data instance TyFamilyProduct a b c = TyFamilyProduct a b c
  deriving (Eq, Ord, Show)

data family TyFamilyUnit
#if __GLASGOW_HASKELL__ >= 706
    (f :: k -> *) (a :: k)
#else
    (f :: * -> *) (a :: *)
#endif
    :: *
data instance TyFamilyUnit f a = TyFamilyUnit
  deriving (Eq, Ord, Show)

data family TyFamilyExQuant x :: *
data instance TyFamilyExQuant a = Show a => TyFamilyExQuant
deriving instance Eq   (TyFamilyExQuant a)
deriving instance Ord  (TyFamilyExQuant a)
deriving instance Show (TyFamilyExQuant a)

data family TyFamilyGADT x :: *
data instance TyFamilyGADT a where
    TyFamilyGADT :: Show a => a -> TyFamilyGADT a
deriving instance Eq   a => Eq   (TyFamilyGADT a)
deriving instance Ord  a => Ord  (TyFamilyGADT a)
deriving instance Show a => Show (TyFamilyGADT a)

-------------------------------------------------------------------------------

-- Plain data types

$(deriveBounded ''TyConEnum)
$(deriveBounded ''TyConProduct)
instance Bounded (TyConUnit f a) where
    minBound = $(makeMinBound ''TyConUnit)
    maxBound = $(makeMaxBound ''TyConUnit)
instance Show a => Bounded (TyConExQuant a) where
    minBound = $(makeMinBound ''TyConExQuant)
    maxBound = $(makeMaxBound ''TyConExQuant)
instance (Bounded a, Show a) => Bounded (TyConGADT a) where
    minBound = $(makeMinBound ''TyConGADT)
    maxBound = $(makeMaxBound ''TyConGADT)

$(deriveEnum ''TyConEnum)
instance Enum (TyConUnit f a) where
    toEnum   = $(makeToEnum   ''TyConUnit)
    fromEnum = $(makeFromEnum ''TyConUnit)

$(deriveIx ''TyConEnum)
$(deriveIx ''TyConProduct)
instance Ix (TyConUnit f a) where
    range       = $(makeRange       ''TyConUnit)
    unsafeIndex = $(makeUnsafeIndex ''TyConUnit)
    inRange     = $(makeInRange     ''TyConUnit)
instance Ix (TyConExQuant a) where
    range       = $(makeRange       ''TyConExQuant)
    unsafeIndex = $(makeUnsafeIndex ''TyConExQuant)
    inRange     = $(makeInRange     ''TyConExQuant)
instance Ix a => Ix (TyConGADT a) where
    range       = $(makeRange       ''TyConGADT)
    unsafeIndex = $(makeUnsafeIndex ''TyConGADT)
    inRange     = $(makeInRange     ''TyConGADT)

#if MIN_VERSION_template_haskell(2,7,0)
-- Data families

$(deriveBounded 'TyFamilyEnum1)
$(deriveBounded 'TyFamilyProduct)
instance Bounded (TyFamilyUnit f a) where
    minBound = $(makeMinBound 'TyFamilyUnit)
    maxBound = $(makeMaxBound 'TyFamilyUnit)
instance Show a => Bounded (TyFamilyExQuant a) where
    minBound = $(makeMinBound 'TyFamilyExQuant)
    maxBound = $(makeMaxBound 'TyFamilyExQuant)
instance (Bounded a, Show a) => Bounded (TyFamilyGADT a) where
    minBound = $(makeMinBound 'TyFamilyGADT)
    maxBound = $(makeMaxBound 'TyFamilyGADT)

$(deriveEnum 'TyFamilyEnum1)
instance Enum (TyFamilyUnit f a) where
    toEnum   = $(makeToEnum   'TyFamilyUnit)
    fromEnum = $(makeFromEnum 'TyFamilyUnit)

$(deriveIx 'TyFamilyEnum1)
$(deriveIx 'TyFamilyProduct)
instance Ix (TyFamilyUnit f a) where
    range       = $(makeRange       'TyFamilyUnit)
    unsafeIndex = $(makeUnsafeIndex 'TyFamilyUnit)
    inRange     = $(makeInRange     'TyFamilyUnit)
instance Ix (TyFamilyExQuant a) where
    range       = $(makeRange       'TyFamilyExQuant)
    unsafeIndex = $(makeUnsafeIndex 'TyFamilyExQuant)
    inRange     = $(makeInRange     'TyFamilyExQuant)
instance Ix a => Ix (TyFamilyGADT a) where
    range       = $(makeRange       'TyFamilyGADT)
    unsafeIndex = $(makeUnsafeIndex 'TyFamilyGADT)
    inRange     = $(makeInRange     'TyFamilyGADT)
#endif

-------------------------------------------------------------------------------

-- | Verifies an 'Ix' instance satisfies the laws.
ixLaws :: (Ix a, Show a) => a -> a -> a -> Expectation
ixLaws l u i = do
    inRange (l,u) i                 `shouldBe` elem i (range (l,u))
    range (l,u) !! index (l,u) i    `shouldBe` i
    map (index (l,u)) (range (l,u)) `shouldBe` [0..rangeSize (l,u)-1]
    rangeSize (l,u)                 `shouldBe` length (range (l,u))

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

        it "has a sensible Ix instance" $
            ixLaws minBound maxBound TyConEnum2
    describe "TyConProduct Bool Bool Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConProduct False False False
            maxBound `shouldBe` TyConProduct True  True  True

        it "has a sensible Ix instance" $
            ixLaws minBound maxBound (TyConProduct False False False)
    describe "TyConUnit Maybe Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConUnit
            maxBound `shouldBe` TyConUnit

        it "has a sensible Enum instance" $
            [minBound .. maxBound] `shouldBe` [TyConUnit]

        it "has a sensible Ix instance" $
            ixLaws minBound maxBound TyConUnit
    describe "TyConExQuant Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` (TyConExQuant :: TyConExQuant Bool)
            maxBound `shouldBe` (TyConExQuant :: TyConExQuant Bool)

        it "has a sensible Ix instance" $
            ixLaws minBound maxBound (TyConExQuant :: TyConExQuant Bool)
    describe "TyConGADT Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyConGADT False
            maxBound `shouldBe` TyConGADT True

        it "has a sensible Ix instance" $
            ixLaws minBound maxBound (TyConGADT False)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamilyEnum" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyEnum1
            maxBound `shouldBe` TyFamilyEnum3

        it "has a sensible Enum instance" $
            [minBound .. maxBound] `shouldBe` [TyFamilyEnum1, TyFamilyEnum2, TyFamilyEnum3]

        it "has a sensible Ix instance" $
            ixLaws minBound maxBound TyFamilyEnum2
    describe "TyFamilyProduct Bool Bool Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyProduct False False False
            maxBound `shouldBe` TyFamilyProduct True  True  True

        it "has a sensible Ix instance" $
            ixLaws minBound maxBound (TyFamilyProduct False False False)
    describe "TyFamilyUnit Maybe Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyUnit
            maxBound `shouldBe` TyFamilyUnit

        it "has a sensible Enum instance" $
            [minBound .. maxBound] `shouldBe` [TyFamilyUnit]

        it "has a sensible Ix instance" $
            ixLaws minBound maxBound TyFamilyUnit
    describe "TyFamilyExQuant Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` (TyFamilyExQuant :: TyFamilyExQuant Bool)
            maxBound `shouldBe` (TyFamilyExQuant :: TyFamilyExQuant Bool)

        it "has a sensible Ix instance" $
            ixLaws minBound maxBound (TyFamilyExQuant :: TyFamilyExQuant Bool)
    describe "TyFamilyGADT Bool" $ do
        it "has a sensible Bounded instance" $ do
            minBound `shouldBe` TyFamilyGADT False
            maxBound `shouldBe` TyFamilyGADT True

        it "has a sensible Ix instance" $
            ixLaws minBound maxBound (TyFamilyGADT False)
#endif
