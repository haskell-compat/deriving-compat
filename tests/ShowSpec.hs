{-# LANGUAGE CPP #-}
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

import Data.Deriving

import GHC.Exts (Char#, Double#, Float#, Int#, Word#)

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

$(deriveShow1 ''TyCon#)
$(deriveShow1 ''TyCon2)

#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 ''TyCon#)
$(deriveShow2 ''TyCon2)
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
