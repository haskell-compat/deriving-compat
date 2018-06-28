{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_template_haskell(2,12,0)
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
#endif

{-|
Module:      DerivingViaSpec
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for 'deriveGND' and 'deriveVia'.
-}
module DerivingViaSpec where

import Prelude ()
import Prelude.Compat

import Test.Hspec

#if MIN_VERSION_template_haskell(2,12,0)
import Data.Deriving.Via

class Container (f :: * -> *) where
  type Inside f a
  peekInside :: f a -> Inside f a

instance Container (Either a) where
  type Inside (Either a) b = Maybe b
  peekInside (Left{})  = Nothing
  peekInside (Right x) = Just x

newtype Down a = MkDown a deriving Show
$(deriveGND [t| forall a. Eq a => Eq (Down a) |])

instance Ord a => Ord (Down a) where
  compare (MkDown x) (MkDown y) = y `compare` x

newtype Id a = MkId a deriving Show
$(deriveGND [t| forall a. Eq a => Eq (Id a) |])
$(deriveVia [t| forall a. Ord a => Ord (Id a) `Via` Down a |])

instance Container Id where
  type Inside Id a = a
  peekInside (MkId x) = x

newtype MyEither a b = MkMyEither (Either a b) deriving Show
$(deriveGND [t| forall a. Functor (MyEither a) |])
$(deriveVia [t| forall a b. (Eq a, Eq b) => Eq (MyEither a b) `Via` Id (Either a b) |])
$(deriveVia [t| forall a. Applicative (MyEither a) `Via` (Either a) |])
$(deriveVia [t| forall a. Container (MyEither a) `Via` (Either a) |])

newtype Wrap f a = MkWrap (f a) deriving Show
$(deriveGND [t| forall f. Container f => Container (Wrap f) |])

class MFunctor (t :: (* -> *) -> * -> *) where
  hoist :: (forall a. m a -> n a) -> t m b -> t n b

newtype TaggedTrans tag trans (m :: * -> *) a = MkTaggedTrans (trans m a) deriving Show
$(deriveGND [t| forall tag trans. MFunctor trans => MFunctor (TaggedTrans tag trans) |])
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_template_haskell(2,12,0)
  describe "Id" $
    it "should compare items in reverse order" $
      compare (MkId "hello") (MkId "world") `shouldBe` GT
#endif
  pure ()
