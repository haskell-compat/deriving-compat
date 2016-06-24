{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module:      EqSpec
Copyright:   (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Eq', 'Eq1', and 'Eq2' instances.
-}
module EqSpec where

#if !defined(NEW_FUNCTOR_CLASSES)
import Data.Functor.Classes (Eq1(..))
#endif
import Data.Deriving

import GHC.Exts (Char#, Double#, Float#, Int#, Word#)

import Prelude ()
import Prelude.Compat

import Test.Hspec

-------------------------------------------------------------------------------

-- Plain data types

data TyCon1 a m =
    TyCon1A a
  | TyCon1B
  | TyCon1C
  | TyCon1D
  | TyCon1E
  | TyCon1F
  | TyCon1G
  | TyCon1H
  | TyCon1I
  | TyCon1J
  | TyCon1K
  | TyCon1L
  | TyCon1M m

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
    TyConClassConstraints    :: (Show m, Show n, Show o, Show p)
                             => m -> n -> o -> p
                             -> TyCon2 m n o p

    TyConEqualityConstraints :: (e ~ g, f ~ h, e ~ f)
                                => e -> f -> g -> h
                             -> TyCon2 e f g h

    TyConTypeRefinement1,
      TyConTypeRefinement2   :: Int -> z
                             -> TyCon2 Int Int z z

data TyConWrap f g h a = TyConWrap1 (f a)
                       | TyConWrap2 (f (g a))
                       | TyConWrap3 (f (g (h a)))

-- Data families

data family TyFamily1 y z :: *

data instance TyFamily1 a m =
    TyFamily1A a
  | TyFamily1B
  | TyFamily1C
  | TyFamily1D
  | TyFamily1E
  | TyFamily1F
  | TyFamily1G
  | TyFamily1H
  | TyFamily1I
  | TyFamily1J
  | TyFamily1K
  | TyFamily1L
  | TyFamily1M m

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
    TyFamilyClassConstraints    :: (Show m, Show n, Show o, Show p)
                                => m -> n -> o -> p
                                -> TyFamily2 m n o p

    TyFamilyEqualityConstraints :: (e ~ g, f ~ h, e ~ f)
                                => e -> f -> g -> h
                                -> TyFamily2 e f g h

    TyFamilyTypeRefinement1,
      TyFamilyTypeRefinement2   :: Int -> z
                                -> TyFamily2 Int Int z z

data family TyFamilyWrap (w :: * -> *) (x :: * -> *) (y :: * -> *) z :: *

data instance TyFamilyWrap f g h a = TyFamilyWrap1 (f a)
                                   | TyFamilyWrap2 (f (g a))
                                   | TyFamilyWrap3 (f (g (h a)))

-------------------------------------------------------------------------------

-- Plain data types

$(deriveEq  ''TyCon1)
$(deriveEq  ''TyCon#)
$(deriveEq  ''TyCon2)
instance (Eq (f a), Eq (f (g a)), Eq (f (g (h a))))
  => Eq (TyConWrap f g h a) where
    (==) = $(makeEq ''TyConWrap)

$(deriveEq1 ''TyCon1)
$(deriveEq1 ''TyCon#)
$(deriveEq1 ''TyCon2)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveEq1 ''TyConWrap)
#else
instance (Eq1 f, Functor f, Eq1 g, Functor g, Eq1 h)
  => Eq1 (TyConWrap f g h) where
    eq1 = $(makeEq1 ''TyConWrap)
#endif

#if defined(NEW_FUNCTOR_CLASSES)
$(deriveEq2 ''TyCon1)
$(deriveEq2 ''TyCon#)
$(deriveEq2 ''TyCon2)
#endif

#if MIN_VERSION_template_haskell(2,7,0)
-- Data families

$(deriveEq  'TyFamily1A)
$(deriveEq  'TyFamily#)
$(deriveEq  'TyFamilyClassConstraints)
instance (Eq (f a), Eq (f (g a)), Eq (f (g (h a))))
  => Eq (TyFamilyWrap f g h a) where
    (==) = $(makeEq 'TyFamilyWrap1)

$(deriveEq1 'TyFamily1B)
$(deriveEq1 'TyFamily#)
$(deriveEq1 'TyFamilyEqualityConstraints)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveEq1 'TyFamilyWrap2)
#else
instance (Eq1 f, Functor f, Eq1 g, Functor g, Eq1 h)
  => Eq1 (TyFamilyWrap f g h) where
    eq1 = $(makeEq1 'TyFamilyWrap3)
#endif

# if defined(NEW_FUNCTOR_CLASSES)
$(deriveEq2 'TyFamily1C)
$(deriveEq2 'TyFamily#)
$(deriveEq2 'TyFamilyTypeRefinement1)
# endif
#endif

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()
