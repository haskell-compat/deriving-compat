{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase #-}
#endif

{-|
Module:      ShowSpec
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Show', 'Show1', and 'Show2' instances.
-}
module ShowSpec where

import Data.Deriving

import GHC.Exts ( Char#, Double#, Float#, Int#, Word#
#if MIN_VERSION_base(4,13,0)
                , Int8#, Int16#, Word8#, Word16#
#endif
                )

import Prelude ()
import Prelude.Compat

import Test.Hspec

import Types.ReadShow ()

-------------------------------------------------------------------------------

-- Plain data types

data TyCon# a b = TyCon# {
    tcA       :: a
  , tcB       :: b
  , tcInt#    :: Int#
  , tcFloat#  :: Float#
  , tcDouble# :: Double#
  , tcChar#   :: Char#
  , tcWord#   :: Word#
#if MIN_VERSION_base(4,13,0)
  , tcInt8#   :: Int8#
  , tcInt16#  :: Int16#
  , tcWord8#  :: Word8#
  , tcWord16# :: Word16#
#endif
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

data Empty1 a b
data Empty2 a b

-- Data families

data family TyFamily# y z :: *

data instance TyFamily# a b = TyFamily# {
    tfA       :: a
  , tfB       :: b
  , tfInt#    :: Int#
  , tfFloat#  :: Float#
  , tfDouble# :: Double#
  , tfChar#   :: Char#
  , tfWord#   :: Word#
#if MIN_VERSION_base(4,13,0)
  , tfInt8#   :: Int8#
  , tfInt16#  :: Int16#
  , tfWord8#  :: Word8#
  , tfWord16# :: Word16#
#endif
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

-------------------------------------------------------------------------------

-- Plain data types

$(deriveShow  ''TyCon#)
$(deriveShow  ''TyCon2)
$(deriveShow  ''Empty1)

$(deriveShow1 ''TyCon#)
$(deriveShow1 ''TyCon2)
$(deriveShow1 ''Empty1)

#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 ''TyCon#)
$(deriveShow2 ''TyCon2)
$(deriveShow2 ''Empty1)
#endif

-- Use EmptyCase here
$(deriveShowOptions  defaultShowOptions{ showEmptyCaseBehavior = True } ''Empty2)
$(deriveShow1Options defaultShowOptions{ showEmptyCaseBehavior = True } ''Empty2)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2Options defaultShowOptions{ showEmptyCaseBehavior = True } ''Empty2)
#endif

#if MIN_VERSION_template_haskell(2,7,0)
-- Data families

$(deriveShow  'TyFamily#)
$(deriveShow  'TyFamilyClassConstraints)

$(deriveShow1 'TyFamily#)
$(deriveShow1 'TyFamilyEqualityConstraints)

# if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 'TyFamily#)
$(deriveShow2 'TyFamilyTypeRefinement1)
# endif
#endif

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()
