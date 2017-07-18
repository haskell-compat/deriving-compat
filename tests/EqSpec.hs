{-# LANGUAGE TemplateHaskell #-}
{-|
Module:      EqSpec
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Eq', 'Eq1', and 'Eq2' instances.
-}
module EqSpec where

import Data.Deriving
import Data.Functor.Classes

import Prelude ()
import Prelude.Compat

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Types.EqOrd ()

-------------------------------------------------------------------------------
data T2 a b = T2 a b

$(deriveEq ''T2)
$(deriveEq1 ''T2)
$(deriveEq2 ''T2)

data T3 a b c = T3 a b c

$(deriveEq ''T3)
$(deriveEq1 ''T3)
$(deriveEq2 ''T3)

data TypeVar12 a = TypeVar12 (T2 a ())
data TypeVar22 a = TypeVar22 (T2 () a)
data TypeVarBoth2 a = TypeVarBoth2 (T2 a a)
data TypeVar23 a = TypeVar23 (T3 () a ())
data TypeVar33 a = TypeVar33 (T3 () () a)
data TypeVarBoth3 a = TypeVarBoth3 (T3 () a a)
data TypeVarNested1 a = TypeVarNested1 (T2 (T2 a ()) ())
data TypeVarNested2 a = TypeVarNested2 (T2 () (T2 a ()))
data TypeVarNested3 a = TypeVarNested3 (T3 () () (T2 a ()))

$(deriveEq1 ''TypeVar12)
$(deriveEq1 ''TypeVar22)
$(deriveEq1 ''TypeVarBoth2)
$(deriveEq1 ''TypeVar23)
$(deriveEq1 ''TypeVar33)
$(deriveEq1 ''TypeVarBoth3)
$(deriveEq1 ''TypeVarNested1)
$(deriveEq1 ''TypeVarNested2)
$(deriveEq1 ''TypeVarNested3)

-------------------------------------------------------------------------------

prop_WorksLikeUnderlyingEquality12 :: Int -> Int -> Bool
prop_WorksLikeUnderlyingEquality12 i j =
    liftEq (==) (TypeVar12 (T2 i ())) (TypeVar12 (T2 j ()))
    ==
    (T2 i () == T2 j ())

prop_WorksLikeUnderlyingEquality22 :: Int -> Int -> Bool
prop_WorksLikeUnderlyingEquality22 i j =
    liftEq (==) (TypeVar22 (T2 () i)) (TypeVar22 (T2 () j))
    ==
    (T2 () i == T2 () j)

prop_WorksLikeUnderlyingEqualityBoth2 :: Int -> Int -> Int -> Int -> Bool
prop_WorksLikeUnderlyingEqualityBoth2 i j k l =
    liftEq (==) (TypeVarBoth2 (T2 i j)) (TypeVarBoth2 (T2 k l))
    ==
    (T2 i j == T2 k l)

prop_WorksLikeUnderlyingEquality23 :: Int -> Int -> Bool
prop_WorksLikeUnderlyingEquality23 i j =
    liftEq (==) (TypeVar23 (T3 () i ())) (TypeVar23 (T3 () j ()))
    ==
    (T3 () i () == T3 () j ())

prop_WorksLikeUnderlyingEquality33 :: Int -> Int -> Bool
prop_WorksLikeUnderlyingEquality33 i j =
    liftEq (==) (TypeVar33 (T3 () () i)) (TypeVar33 (T3 () () j))
    ==
    (T3 () () i == T3 () () j)

prop_WorksLikeUnderlyingEqualityBoth3 :: Int -> Int -> Int -> Int -> Bool
prop_WorksLikeUnderlyingEqualityBoth3 i j k l =
    liftEq (==) (TypeVarBoth3 (T3 () i j)) (TypeVarBoth3 (T3 () k l))
    ==
    (T3 () i j == T3 () k l)

prop_WorksLikeUnderlyingEqualityNested1 :: Int -> Int -> Bool
prop_WorksLikeUnderlyingEqualityNested1 i j =
    liftEq (==) (TypeVarNested1 (T2 (T2 i ()) ())) (TypeVarNested1 (T2 (T2 j ()) ()))
    ==
    (T2 (T2 i ()) () == T2 (T2 j ()) ())

prop_WorksLikeUnderlyingEqualityNested2 :: Int -> Int -> Bool
prop_WorksLikeUnderlyingEqualityNested2 i j =
    liftEq (==) (TypeVarNested2 (T2 () (T2 i ()))) (TypeVarNested2 (T2 () (T2 j ())))
    ==
    (T2 () (T2 i ()) == (T2 () (T2 j ())))

prop_WorksLikeUnderlyingEqualityNested3 :: Int -> Int -> Bool
prop_WorksLikeUnderlyingEqualityNested3 i j =
    liftEq (==) (TypeVarNested3 (T3 () () (T2 i ()))) (TypeVarNested3 (T3 () () (T2 j ())))
    ==
    (T3 () () (T2 i ()) == T3 () () (T2 j ()))

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "derived Eq1 instances use Eq2 appropriately" $ do
  prop "instance for type variables in position 1 of 2 works like underlying equality"
      prop_WorksLikeUnderlyingEquality12
  prop "instance for type variables in position 2 of 2 works like underlying equality"
      prop_WorksLikeUnderlyingEquality22
  prop "instance for type variables in position 1 and 2 of 2 works like underlying equality"
      prop_WorksLikeUnderlyingEqualityBoth2
  prop "instance for type variables in position 2 of 3 works like underlying equality"
      prop_WorksLikeUnderlyingEquality23
  prop "instance for type variables in position 3 of 3 works like underlying equality"
      prop_WorksLikeUnderlyingEquality33
  prop "instance for type variables in position 2 and 3 of 3 works like underlying equality"
      prop_WorksLikeUnderlyingEqualityBoth3
  prop "instance for nested type variables nested works like underlying equality (1)"
      prop_WorksLikeUnderlyingEqualityNested1
  prop "instance for nested type variables nested works like underlying equality (2)"
      prop_WorksLikeUnderlyingEqualityNested2
  prop "instance for nested type variables nested works like underlying equality (3)"
      prop_WorksLikeUnderlyingEqualityNested3
