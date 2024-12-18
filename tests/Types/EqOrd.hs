{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Types.EqOrd
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Shared datatypes between "EqSpec" and "OrdSpec".
-}
module Types.EqOrd where

import Data.Deriving

import GHC.Exts ( Addr#, Char#, Double#, Float#, Int#, Word#
#if MIN_VERSION_base(4,13,0)
                , Int8#, Int16#, Word8#, Word16#
#endif
#if MIN_VERSION_base(4,16,0)
                , Int32#, Word32#
#endif
                )

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
  , tcAddr#   :: Addr#
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
#if MIN_VERSION_base(4,16,0)
  , tcInt32#  :: Int32#
  , tcWord32# :: Word32#
#endif
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

data Empty a b

data TyConNullary a b
  = TyConNullary1
  | TyConNullary2
  | TyConNullary3

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
#if MIN_VERSION_base(4,13,0)
  , tfInt8#   :: Int8#
  , tfInt16#  :: Int16#
  , tfWord8#  :: Word8#
  , tfWord16# :: Word16#
#endif
#if MIN_VERSION_base(4,16,0)
  , tfInt32#  :: Int32#
  , tfWord32# :: Word32#
#endif
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

data family TyFamilyNullary x y :: *

data instance TyFamilyNullary a b
  = TyFamilyNullary1
  | TyFamilyNullary2
  | TyFamilyNullary3

-------------------------------------------------------------------------------

-- Plain data types

$(deriveEq  ''TyCon1)
$(deriveEq  ''TyCon#)
$(deriveEq  ''TyCon2)
instance (Eq (f a), Eq (f (g a)), Eq (f (g (h a))))
  => Eq (TyConWrap f g h a) where
    (==) = $(makeEq    ''TyConWrap)
    (/=) = $(makeNotEq ''TyConWrap)
$(deriveEq  ''Empty)
$(deriveEq ''TyConNullary)

$(deriveEq1 ''TyCon1)
$(deriveEq1 ''TyCon#)
$(deriveEq1 ''TyCon2)
$(deriveEq1 ''TyConWrap)
$(deriveEq1 ''Empty)
$(deriveEq1 ''TyConNullary)

$(deriveOrd  ''TyCon1)
$(deriveOrd  ''TyCon#)
$(deriveOrd  ''TyCon2)
instance (Ord (f a), Ord (f (g a)), Ord (f (g (h a))))
  => Ord (TyConWrap f g h a) where
    compare = $(makeCompare ''TyConWrap)
    (>)     = $(makeLT      ''TyConWrap)
    (>=)    = $(makeLE      ''TyConWrap)
    (<)     = $(makeGT      ''TyConWrap)
    (<=)    = $(makeGE      ''TyConWrap)
    max     = $(makeMax     ''TyConWrap)
    min     = $(makeMin     ''TyConWrap)
$(deriveOrd  ''Empty)
$(deriveOrd ''TyConNullary)

$(deriveOrd1 ''TyCon1)
$(deriveOrd1 ''TyCon#)
$(deriveOrd1 ''TyCon2)
$(deriveOrd1 ''TyConWrap)
$(deriveOrd1 ''Empty)
$(deriveOrd1 ''TyConNullary)

$(deriveEq2 ''TyCon1)
$(deriveEq2 ''TyCon#)
$(deriveEq2 ''TyCon2)
$(deriveEq2 ''Empty)
$(deriveEq2 ''TyConNullary)

$(deriveOrd2 ''TyCon1)
$(deriveOrd2 ''TyCon#)
$(deriveOrd2 ''TyCon2)
$(deriveOrd2 ''Empty)
$(deriveOrd2 ''TyConNullary)

-- Data families

$(deriveEq  'TyFamily1A)
$(deriveEq  'TyFamily#)
$(deriveEq  'TyFamilyClassConstraints)
instance (Eq (f a), Eq (f (g a)), Eq (f (g (h a))))
  => Eq (TyFamilyWrap f g h a) where
    (==) = $(makeEq    'TyFamilyWrap1)
    (/=) = $(makeNotEq 'TyFamilyWrap1)
$(deriveEq 'TyFamilyNullary1)

$(deriveEq1 'TyFamily1B)
$(deriveEq1 'TyFamily#)
$(deriveEq1 'TyFamilyEqualityConstraints)
$(deriveEq1 'TyFamilyNullary1)

$(deriveOrd  'TyFamily1A)
$(deriveOrd  'TyFamily#)
$(deriveOrd  'TyFamilyClassConstraints)
$(deriveEq1 'TyFamilyWrap2)
instance (Ord (f a), Ord (f (g a)), Ord (f (g (h a))))
  => Ord (TyFamilyWrap f g h a) where
    compare = $(makeCompare 'TyFamilyWrap1)
    (>)     = $(makeLT      'TyFamilyWrap1)
    (>=)    = $(makeLE      'TyFamilyWrap1)
    (<)     = $(makeGT      'TyFamilyWrap1)
    (<=)    = $(makeGE      'TyFamilyWrap1)
    max     = $(makeMax     'TyFamilyWrap1)
    min     = $(makeMin     'TyFamilyWrap1)
$(deriveOrd 'TyFamilyNullary1)

$(deriveOrd1 'TyFamily1B)
$(deriveOrd1 'TyFamily#)
$(deriveOrd1 'TyFamilyEqualityConstraints)
$(deriveOrd1 'TyFamilyWrap2)
$(deriveOrd1 'TyFamilyNullary1)

$(deriveEq2 'TyFamily1C)
$(deriveEq2 'TyFamily#)
$(deriveEq2 'TyFamilyTypeRefinement1)
$(deriveEq2 'TyFamilyNullary1)

$(deriveOrd2 'TyFamily1C)
$(deriveOrd2 'TyFamily#)
$(deriveOrd2 'TyFamilyTypeRefinement1)
$(deriveOrd2 'TyFamilyNullary1)
