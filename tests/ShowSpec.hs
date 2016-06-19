{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module:      ShowSpec
Copyright:   (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Show', 'Show1', and 'Show2' instances.
-}
module ShowSpec where

#if !defined(NEW_FUNCTOR_CLASSES)
import Data.Functor.Classes (Show1(..))
#endif
import Data.Deriving

import GHC.Exts (Char#, Double#, Float#, Int#, Word#)

import Prelude ()
import Prelude.Compat

import Test.Hspec

-------------------------------------------------------------------------------

-- Plain data types

infixl 4 :@:
data TyCon1 a b = TyConPrefix { tc1 :: a, tc2 :: b }
                | (:@:) { tc3 :: b, tc4 :: a }

infixl 3 :!!:
infix  4 :@@:
infixr 5 `TyConPlain`
infixr 6 `TyConFakeInfix`
data TyConPlain a b = (:!!:) a b
                    | a :@@: b
                    | a `TyConPlain` b
                    | TyConFakeInfix a b

data TyConGADT a b where
    (:.)    ::           c ->       d        -> TyConGADT c d
    (:..)   ::           e ->       f        -> TyConGADT e f
    (:...)  ::           g ->       h -> Int -> TyConGADT g h
    (:....) :: { tcg1 :: i, tcg2 :: j }      -> TyConGADT i j

data TyCon# a b = TyCon# {
    tcA       :: a
  , tcB       :: b
  , tcInt#    :: Int#
  , tcFloat#  :: Float#
  , tcDouble# :: Double#
  , tcChar#   :: Char#
  , tcWord#   :: Word#
}

data TyCon2 a b c d where
    TyConClassConstraints    :: (Ord m, Ord n, Ord o, Ord p)
                             => m -> n -> o -> p
                             -> TyCon2 m n o p

    TyConEqualityConstraints :: (e ~ g, f ~ h, e ~ f)
                                => e -> f -> g -> h
                             -> TyCon2 e f g h

    TyConTypeRefinement1,
      TyConTypeRefinement2   :: Int -> z
                             -> TyCon2 Int Int z z

    TyConForalls             :: forall p q r s t u.
                                (Show p, Show q)
                             => p -> q -> u -> t
                             -> TyCon2 r s t u

data TyConWrap f g h a = TyConWrap1 (f a)
                       | TyConWrap2 (f (g a))
                       | TyConWrap3 (f (g (h a)))

-- Data families

data family TyFamily1 y z :: *

infixl 4 :!:
data instance TyFamily1 a b = TyFamilyPrefix { tf1 :: a, tf2 :: b }
                            | (:!:)          { tf3 :: b, tf4 :: a }

data family TyFamilyPlain y z :: *

infixl 3 :#:
infix  4 :$:
infixr 5 `TyFamilyPlain`
infixr 6 `TyFamilyFakeInfix`
data instance TyFamilyPlain a b = (:#:) a b
                                 | a :$: b
                                 | a `TyFamilyPlain` b
                                 | TyFamilyFakeInfix a b


data family TyFamilyGADT y z :: *

infixr 1 :*, :***, :****
data instance TyFamilyGADT a b where
    (:*)    ::           c ->       d        -> TyFamilyGADT c d
    (:**)   ::           e ->       f        -> TyFamilyGADT e f
    (:***)  ::           g ->       h -> Int -> TyFamilyGADT g h
    (:****) :: { tfg1 :: i, tfg2 :: j }      -> TyFamilyGADT i j

data family TyFamily# y z :: *

data instance TyFamily# a b = TyFamily# {
    tfA       :: a
  , tfB       :: b
  , tfInt#    :: Int#
  , tfFloat#  :: Float#
  , tfDouble# :: Double#
  , tfChar#   :: Char#
  , tfWord#   :: Word#
}

data family TyFamily2 w x y z :: *

data instance TyFamily2 a b c d where
    TyFamilyClassConstraints    :: (Ord m, Ord n, Ord o, Ord p)
                                => m -> n -> o -> p
                                -> TyFamily2 m n o p

    TyFamilyEqualityConstraints :: (e ~ g, f ~ h, e ~ f)
                                => e -> f -> g -> h
                                -> TyFamily2 e f g h

    TyFamilyTypeRefinement1,
      TyFamilyTypeRefinement2   :: Int -> z
                                -> TyFamily2 Int Int z z

    TyFamilyForalls             :: forall p q r s t u.
                                   (Show p, Show q)
                                => p -> q -> u -> t
                                -> TyFamily2 r s t u

data family TyFamilyWrap (w :: * -> *) (x :: * -> *) (y :: * -> *) z :: *

data instance TyFamilyWrap f g h a = TyFamilyWrap1 (f a)
                                   | TyFamilyWrap2 (f (g a))
                                   | TyFamilyWrap3 (f (g (h a)))

-------------------------------------------------------------------------------

-- Plain data types

$(deriveShow  ''TyCon1)
$(deriveShow  ''TyConPlain)
$(deriveShow  ''TyConGADT)
$(deriveShow  ''TyCon#)
$(deriveShow  ''TyCon2)
instance (Show (f a), Show (f (g a)), Show (f (g (h a))))
  => Show (TyConWrap f g h a) where
    showsPrec = $(makeShowsPrec ''TyConWrap)

$(deriveShow1 ''TyCon1)
$(deriveShow1 ''TyConPlain)
$(deriveShow1 ''TyConGADT)
$(deriveShow1 ''TyCon#)
$(deriveShow1 ''TyCon2)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1 ''TyConWrap)
#else
instance (Show1 f, Functor f, Show1 g, Functor g, Show1 h)
  => Show1 (TyConWrap f g h) where
    showsPrec1 = $(makeShowsPrec1 ''TyConWrap)
#endif

#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 ''TyCon1)
$(deriveShow2 ''TyConPlain)
$(deriveShow2 ''TyConGADT)
$(deriveShow2 ''TyCon#)
$(deriveShow2 ''TyCon2)
#endif

#if MIN_VERSION_template_haskell(2,7,0)
-- Data families

$(deriveShow  'TyFamilyPrefix)
$(deriveShow  '(:#:))
$(deriveShow  '(:*))
$(deriveShow  'TyFamily#)
$(deriveShow  'TyFamilyClassConstraints)
instance (Show (f a), Show (f (g a)), Show (f (g (h a))))
  => Show (TyFamilyWrap f g h a) where
    showsPrec = $(makeShowsPrec 'TyFamilyWrap1)

$(deriveShow1 '(:!:))
$(deriveShow1 '(:$:))
$(deriveShow1 '(:**))
$(deriveShow1 'TyFamily#)
$(deriveShow1 'TyFamilyEqualityConstraints)
# if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1 'TyFamilyWrap2)
# else
instance (Show1 f, Functor f, Show1 g, Functor g, Show1 h)
  => Show1 (TyFamilyWrap f g h) where
    showsPrec1 = $(makeShowsPrec1 'TyFamilyWrap3)
# endif

# if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 'TyFamilyPrefix)
$(deriveShow2 'TyFamilyPlain)
$(deriveShow2 '(:***))
$(deriveShow2 'TyFamily#)
$(deriveShow2 'TyFamilyTypeRefinement1)
# endif
#endif

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()
