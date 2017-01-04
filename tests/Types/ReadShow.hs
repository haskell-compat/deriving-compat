{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module:      Types.ReadShow
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Shared datatypes between "ReadSpec" and "ShowSpec".
-}
module Types.ReadShow where

#if !defined(NEW_FUNCTOR_CLASSES)
import Data.Functor.Classes (Read1(..), Show1(..))
#endif
import Data.Deriving

import Text.Read (Read(..), readListPrecDefault)

-------------------------------------------------------------------------------

-- Plain data types

infixl 4 :@:
data TyCon1 a b = TyConPrefix { tc1 :: a, tc2  :: b }
                | (:@:)       { tc3 :: b, (##) :: a }

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

data TyConWrap f g h a = TyConWrap1 (f a)
                       | TyConWrap2 (f (g a))
                       | TyConWrap3 (f (g (h a)))

data TC# a b = MkTC1# a b
             | MkTC2# { getTC2# :: b, (#~#) :: a }
             | a `MkTC3#` b

-- Data families

data family TyFamily1 y z :: *

infixl 4 :!:
data instance TyFamily1 a b = TyFamilyPrefix { tf1 :: a, tf2   :: b }
                            | (:!:)          { tf3 :: b, (###) :: a }

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

data family TyFamilyWrap (w :: * -> *) (x :: * -> *) (y :: * -> *) z :: *

data instance TyFamilyWrap f g h a = TyFamilyWrap1 (f a)
                                   | TyFamilyWrap2 (f (g a))
                                   | TyFamilyWrap3 (f (g (h a)))

data family TF# y z :: *

data instance TF# a b = MkTF1# a b
                      | MkTF2# { getTF2# :: b, (#~~#) :: a }
                      | a `MkTF3#` b

-------------------------------------------------------------------------------

-- Plain data types

$(deriveRead  ''TyCon1)
$(deriveRead  ''TyConPlain)
$(deriveRead  ''TyConGADT)
instance (Read (f a), Read (f (g a)), Read (f (g (h a))))
  => Read (TyConWrap f g h a) where
    readPrec     = $(makeReadPrec ''TyConWrap)
    readListPrec = readListPrecDefault
$(deriveRead  ''TC#)

$(deriveRead1 ''TyCon1)
$(deriveRead1 ''TyConPlain)
$(deriveRead1 ''TyConGADT)
$(deriveRead1 ''TC#)

$(deriveShow  ''TyCon1)
$(deriveShow  ''TyConPlain)
$(deriveShow  ''TyConGADT)
instance (Show (f a), Show (f (g a)), Show (f (g (h a))))
  => Show (TyConWrap f g h a) where
    showsPrec = $(makeShowsPrec ''TyConWrap)
    show      = $(makeShow      ''TyConWrap)
    showList  = $(makeShowList  ''TyConWrap)
$(deriveShow  ''TC#)

$(deriveShow1 ''TyCon1)
$(deriveShow1 ''TyConPlain)
$(deriveShow1 ''TyConGADT)
$(deriveShow1 ''TC#)

#if defined(NEW_FUNCTOR_CLASSES)
$(deriveRead1 ''TyConWrap)

$(deriveShow1 ''TyConWrap)
#else
instance (Read1 f, Functor f, Read1 g, Functor g, Read1 h)
  => Read1 (TyConWrap f g h) where
    readsPrec1 = $(makeReadsPrec1 ''TyConWrap)

instance (Show1 f, Functor f, Show1 g, Functor g, Show1 h)
  => Show1 (TyConWrap f g h) where
    showsPrec1 = $(makeShowsPrec1 ''TyConWrap)
#endif

#if defined(NEW_FUNCTOR_CLASSES)
$(deriveRead2 ''TyCon1)
$(deriveRead2 ''TyConPlain)
$(deriveRead2 ''TyConGADT)
$(deriveRead2 ''TC#)

$(deriveShow2 ''TyCon1)
$(deriveShow2 ''TyConPlain)
$(deriveShow2 ''TyConGADT)
$(deriveShow2 ''TC#)
#endif

#if MIN_VERSION_template_haskell(2,7,0)
-- Data families

$(deriveRead  'TyFamilyPrefix)
$(deriveRead  '(:#:))
$(deriveRead  '(:*))
instance (Read (f a), Read (f (g a)), Read (f (g (h a))))
  => Read (TyFamilyWrap f g h a) where
    readsPrec = $(makeReadsPrec 'TyFamilyWrap1)
$(deriveRead  'MkTF1#)

$(deriveRead1 '(:!:))
$(deriveRead1 '(:$:))
$(deriveRead1 '(:**))
$(deriveRead1 'MkTF2#)

$(deriveShow  'TyFamilyPrefix)
$(deriveShow  '(:#:))
$(deriveShow  '(:*))
instance (Show (f a), Show (f (g a)), Show (f (g (h a))))
  => Show (TyFamilyWrap f g h a) where
    showsPrec = $(makeShowsPrec 'TyFamilyWrap1)
    show      = $(makeShow      'TyFamilyWrap1)
    showList  = $(makeShowList  'TyFamilyWrap1)
$(deriveShow  'MkTF3#)

$(deriveShow1 '(:!:))
$(deriveShow1 '(:$:))
$(deriveShow1 '(:**))
$(deriveShow1 'MkTF1#)

# if defined(NEW_FUNCTOR_CLASSES)
$(deriveRead1 'TyFamilyWrap2)

$(deriveShow1 'TyFamilyWrap2)
# else
instance (Read1 f, Functor f, Read1 g, Functor g, Read1 h)
  => Read1 (TyFamilyWrap f g h) where
    readsPrec1 = $(makeReadsPrec1 'TyFamilyWrap3)

instance (Show1 f, Functor f, Show1 g, Functor g, Show1 h)
  => Show1 (TyFamilyWrap f g h) where
    showsPrec1 = $(makeShowsPrec1 'TyFamilyWrap3)
# endif

# if defined(NEW_FUNCTOR_CLASSES)
$(deriveRead2 'TyFamilyPrefix)
$(deriveRead2 'TyFamilyPlain)
$(deriveRead2 '(:***))
$(deriveRead2 'MkTF2#)

$(deriveShow2 'TyFamilyPrefix)
$(deriveShow2 'TyFamilyPlain)
$(deriveShow2 '(:***))
$(deriveShow2 'MkTF3#)
# endif
#endif
