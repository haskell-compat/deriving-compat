{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RoleAnnotations #-}
#endif

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-unused-foralls #-}
#endif

{-|
Module:      FunctorSpec
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Functor', 'Foldable', and 'Traversable' instances.
-}
module FunctorSpec where

import Data.Char (chr)
import Data.Foldable (fold)
import Data.Deriving
import Data.Functor.Classes (Eq1)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid
import Data.Orphans ()

import GHC.Exts (Int#)

import Prelude ()
import Prelude.Compat

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary)

-------------------------------------------------------------------------------

-- Adapted from the test cases from
-- https://ghc.haskell.org/trac/ghc/attachment/ticket/2953/deriving-functor-tests.patch

-- Plain data types

data Strange a b c
    = T1 a b c
    | T2 [a] [b] [c]         -- lists
    | T3 [[a]] [[b]] [[c]]   -- nested lists
    | T4 (c,(b,b),(c,c))     -- tuples
    | T5 ([c],Strange a b c) -- tycons

type IntFun a b = (b -> Int) -> a
data StrangeFunctions a b c
    = T6 (a -> c)            -- function types
    | T7 (a -> (c,a))        -- functions and tuples
    | T8 ((b -> a) -> c)     -- continuation
    | T9 (IntFun b c)        -- type synonyms

data StrangeGADT a b where
    T10 :: Ord d            => d        -> StrangeGADT c d
    T11 ::                     Int      -> StrangeGADT e Int
    T12 :: c ~ Int          => c        -> StrangeGADT f Int
    T13 :: i ~ Int          => Int      -> StrangeGADT h i
    T14 :: k ~ Int          => k        -> StrangeGADT j k
    T15 :: (n ~ c, c ~ Int) => Int -> c -> StrangeGADT m n

data NotPrimitivelyRecursive a b
    = S1 (NotPrimitivelyRecursive (a,a) (b, a))
    | S2 a
    | S3 b

newtype OneTwoCompose f g a b = OneTwoCompose (Either (f (g a)) (f (g b)))
  deriving (Arbitrary, Eq, Show)

newtype ComplexConstraint f g a b = ComplexConstraint (f Int Int (g a,a,b))

data Universal a b
    = Universal  (forall b. (b,[a]))
    | Universal2 (forall f. Functor (f a) => f a b)
    | Universal3 (forall a. Maybe a) -- reuse a
    | NotReallyUniversal (forall b. a)

data Existential a b
    = forall a. ExistentialList [a]
    | forall f. Traversable (f a) => ExistentialFunctor (f a b)
    | forall b. SneakyUseSameName (Maybe b)

data IntHash a b
    = IntHash Int# Int#
    | IntHashTuple Int# a b (a, b, Int, IntHash Int (a, b, Int))

data IntHashFun a b
    = IntHashFun ((((a -> Int#) -> b) -> Int#) -> a)

data Empty1 a
data Empty2 a
#if __GLASGOW_HASKELL__ >= 708
type role Empty2 nominal
#endif

data TyCon29 a
    = TyCon29a (forall b. b -> (forall c. a -> c) -> a)
    | TyCon29b (Int -> forall c. c -> a)

type family F :: * -> *
type instance F = Maybe

data TyCon30 a = TyCon30 (F a)

-- Data families

data family   StrangeFam x  y z
data instance StrangeFam a  b c
    = T1Fam a b c
    | T2Fam [a] [b] [c]         -- lists
    | T3Fam [[a]] [[b]] [[c]]   -- nested lists
    | T4Fam (c,(b,b),(c,c))     -- tuples
    | T5Fam ([c],Strange a b c) -- tycons

data family   StrangeFunctionsFam x y z
data instance StrangeFunctionsFam a b c
    = T6Fam (a -> c)            -- function types
    | T7Fam (a -> (c,a))        -- functions and tuples
    | T8Fam ((b -> a) -> c)     -- continuation
    | T9Fam (IntFun b c)        -- type synonyms

data family   StrangeGADTFam x y
data instance StrangeGADTFam a b where
    T10Fam :: Ord d            => d        -> StrangeGADTFam c d
    T11Fam ::                     Int      -> StrangeGADTFam e Int
    T12Fam :: c ~ Int          => c        -> StrangeGADTFam f Int
    T13Fam :: i ~ Int          => Int      -> StrangeGADTFam h i
    T14Fam :: k ~ Int          => k        -> StrangeGADTFam j k
    T15Fam :: (n ~ c, c ~ Int) => Int -> c -> StrangeGADTFam m n

data family   NotPrimitivelyRecursiveFam x y
data instance NotPrimitivelyRecursiveFam a b
    = S1Fam (NotPrimitivelyRecursive (a,a) (b, a))
    | S2Fam a
    | S3Fam b

data family      OneTwoComposeFam (j :: * -> *) (k :: * -> *) x y
newtype instance OneTwoComposeFam f g a b =
    OneTwoComposeFam (Either (f (g a)) (f (g b)))
  deriving (Arbitrary, Eq, Show)

data family      ComplexConstraintFam (j :: * -> * -> * -> *) (k :: * -> *) x y
newtype instance ComplexConstraintFam f g a b = ComplexConstraintFam (f Int Int (g a,a,b))

data family   UniversalFam x y
data instance UniversalFam a b
    = UniversalFam  (forall b. (b,[a]))
    | Universal2Fam (forall f. Functor (f a) => f a b)
    | Universal3Fam (forall a. Maybe a) -- reuse a
    | NotReallyUniversalFam (forall b. a)

data family   ExistentialFam x y
data instance ExistentialFam a b
    = forall a. ExistentialListFam [a]
    | forall f. Traversable (f a) => ExistentialFunctorFam (f a b)
    | forall b. SneakyUseSameNameFam (Maybe b)

data family   IntHashFam x y
data instance IntHashFam a b
    = IntHashFam Int# Int#
    | IntHashTupleFam Int# a b (a, b, Int, IntHashFam Int (a, b, Int))

data family   IntHashFunFam x y
data instance IntHashFunFam a b
    = IntHashFunFam ((((a -> Int#) -> b) -> Int#) -> a)

data family   TyFamily29 x
data instance TyFamily29 a
    = TyFamily29a (forall b. b -> (forall c. a -> c) -> a)
    | TyFamily29b (Int -> forall c. c -> a)

data family   TyFamily30 x
data instance TyFamily30 a = TyFamily30 (F a)

-------------------------------------------------------------------------------

-- Plain data types

$(deriveFunctor     ''Strange)
$(deriveFoldable    ''Strange)
$(deriveTraversable ''Strange)

$(deriveFunctor     ''StrangeFunctions)
$(deriveFoldable    ''StrangeGADT)

$(deriveFunctor     ''NotPrimitivelyRecursive)
$(deriveFoldable    ''NotPrimitivelyRecursive)
$(deriveTraversable ''NotPrimitivelyRecursive)

$(deriveFunctor     ''OneTwoCompose)
$(deriveFoldable    ''OneTwoCompose)
$(deriveTraversable ''OneTwoCompose)

instance Functor (f Int Int) => Functor (ComplexConstraint f g a) where
    fmap      = $(makeFmap      ''ComplexConstraint)
    (<$)      = $(makeReplace   ''ComplexConstraint)
instance Foldable (f Int Int) => Foldable (ComplexConstraint f g a) where
    foldr     = $(makeFoldr     ''ComplexConstraint)
    foldMap   = $(makeFoldMap   ''ComplexConstraint)
    fold      = $(makeFold      ''ComplexConstraint)
    foldl     = $(makeFoldl     ''ComplexConstraint)
#if MIN_VERSION_base(4,8,0)
    null      = $(makeNull      ''ComplexConstraint)
#endif
instance Traversable (f Int Int) => Traversable (ComplexConstraint f g a) where
    traverse  = $(makeTraverse  ''ComplexConstraint)
    sequenceA = $(makeSequenceA ''ComplexConstraint)
    mapM      = $(makeMapM      ''ComplexConstraint)
    sequence  = $(makeSequence  ''ComplexConstraint)

$(deriveFunctor     ''Universal)

$(deriveFunctor     ''Existential)
$(deriveFoldable    ''Existential)
$(deriveTraversable ''Existential)

$(deriveFunctor     ''IntHash)
$(deriveFoldable    ''IntHash)
$(deriveTraversable ''IntHash)

$(deriveFunctor     ''IntHashFun)

$(deriveFunctor     ''Empty1)
$(deriveFoldable    ''Empty1)
$(deriveTraversable ''Empty1)

-- Use EmptyCase here
$(deriveFunctorOptions     defaultFFTOptions{ fftEmptyCaseBehavior = True } ''Empty2)
$(deriveFoldableOptions    defaultFFTOptions{ fftEmptyCaseBehavior = True } ''Empty2)
$(deriveTraversableOptions defaultFFTOptions{ fftEmptyCaseBehavior = True } ''Empty2)

$(deriveFunctor     ''TyCon29)

$(deriveFunctor     ''TyCon30)
$(deriveFoldable    ''TyCon30)
$(deriveTraversable ''TyCon30)

#if MIN_VERSION_template_haskell(2,7,0)
-- Data families

$(deriveFunctor     'T1Fam)
$(deriveFoldable    'T2Fam)
$(deriveTraversable 'T3Fam)

$(deriveFunctor     'T6Fam)
$(deriveFoldable    'T10Fam)

$(deriveFunctor     'S1Fam)
$(deriveFoldable    'S2Fam)
$(deriveTraversable 'S3Fam)

$(deriveFunctor     'OneTwoComposeFam)
$(deriveFoldable    'OneTwoComposeFam)
$(deriveTraversable 'OneTwoComposeFam)

instance Functor (f Int Int) => Functor (ComplexConstraintFam f g a) where
    fmap      = $(makeFmap      'ComplexConstraintFam)
    (<$)      = $(makeReplace   'ComplexConstraintFam)
instance Foldable (f Int Int) => Foldable (ComplexConstraintFam f g a) where
    foldr     = $(makeFoldr     'ComplexConstraintFam)
    foldMap   = $(makeFoldMap   'ComplexConstraintFam)
    fold      = $(makeFold      'ComplexConstraintFam)
    foldl     = $(makeFoldl     'ComplexConstraintFam)
# if MIN_VERSION_base(4,8,0)
    null      = $(makeNull      'ComplexConstraintFam)
# endif
instance Traversable (f Int Int) => Traversable (ComplexConstraintFam f g a) where
    traverse  = $(makeTraverse  'ComplexConstraintFam)
    sequenceA = $(makeSequenceA 'ComplexConstraintFam)
    mapM      = $(makeMapM      'ComplexConstraintFam)
    sequence  = $(makeSequence  'ComplexConstraintFam)

$(deriveFunctor     'UniversalFam)

$(deriveFunctor     'ExistentialListFam)
$(deriveFoldable    'ExistentialFunctorFam)
$(deriveTraversable 'SneakyUseSameNameFam)

$(deriveFunctor     'IntHashFam)
$(deriveFoldable    'IntHashTupleFam)
$(deriveTraversable 'IntHashFam)

$(deriveFunctor     'IntHashFunFam)

$(deriveFunctor     'TyFamily29a)

$(deriveFunctor     'TyFamily30)
$(deriveFoldable    'TyFamily30)
$(deriveTraversable 'TyFamily30)
#endif

-------------------------------------------------------------------------------

prop_FunctorLaws :: (Functor f, Eq (f a), Eq (f c))
                 => (b -> c) -> (a -> b) -> f a -> Bool
prop_FunctorLaws f g x =
       fmap id      x == x
    && fmap (f . g) x == (fmap f . fmap g) x

prop_FunctorEx :: (Functor f, Eq (f [Int])) => f [Int] -> Bool
prop_FunctorEx = prop_FunctorLaws reverse (++ [42])

prop_FoldableLaws :: (Eq a, Eq b, Eq z, Monoid a, Monoid b, Foldable f)
                  => (a -> b) -> (a -> z -> z) -> z -> f a -> Bool
prop_FoldableLaws f h z x =
       fold      x == foldMap id x
    && foldMap f x == foldr (mappend . f) mempty x
    && foldr h z x == appEndo (foldMap (Endo . h) x) z

prop_FoldableEx :: Foldable f => f [Int] -> Bool
prop_FoldableEx = prop_FoldableLaws reverse ((+) . length) 0

prop_TraversableLaws :: forall t f g a b c.
                        (Applicative f, Applicative g, Traversable t,
                         Eq (t (f a)),  Eq (g (t a)),  Eq (g (t b)),
                         Eq (t a),      Eq (t c),      Eq1 f, Eq1 g)
                       => (a -> f b) -> (b -> f c)
                       -> (forall x. f x -> g x) -> t a -> Bool
prop_TraversableLaws f g t x =
       (t . traverse f)  x == traverse (t . f)   x
    && traverse Identity x == Identity           x
    && traverse (Compose . fmap g . f) x
         == (Compose . fmap (traverse g) . traverse f) x

    && (t . sequenceA)             y == (sequenceA . fmap t) y
    && (sequenceA . fmap Identity) y == Identity             y
    && (sequenceA . fmap Compose)  z
         == (Compose . fmap sequenceA . sequenceA) z
  where
    y :: t (f a)
    y = fmap pure x

    z :: t (f (g a))
    z = fmap (fmap pure) y

prop_TraversableEx :: (Traversable t, Eq (t [[Int]]),
                       Eq (t [Int]), Eq (t String), Eq (t Char))
                   => t [Int] -> Bool
prop_TraversableEx = prop_TraversableLaws
    (replicate 2 . map (chr . abs))
    (++ "Hello")
    reverse

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "OneTwoCompose Maybe ((,) Bool) [Int] [Int]" $ do
        prop "satisfies the Functor laws"
            (prop_FunctorEx     :: OneTwoCompose Maybe ((,) Bool) [Int] [Int] -> Bool)
        prop "satisfies the Foldable laws"
            (prop_FoldableEx    :: OneTwoCompose Maybe ((,) Bool) [Int] [Int] -> Bool)
        prop "satisfies the Traversable laws"
            (prop_TraversableEx :: OneTwoCompose Maybe ((,) Bool) [Int] [Int] -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "OneTwoComposeFam Maybe ((,) Bool) [Int] [Int]" $ do
        prop "satisfies the Functor laws"
            (prop_FunctorEx     :: OneTwoComposeFam Maybe ((,) Bool) [Int] [Int] -> Bool)
        prop "satisfies the Foldable laws"
            (prop_FoldableEx    :: OneTwoComposeFam Maybe ((,) Bool) [Int] [Int] -> Bool)
        prop "satisfies the Traversable laws"
            (prop_TraversableEx :: OneTwoComposeFam Maybe ((,) Bool) [Int] [Int] -> Bool)
#endif
