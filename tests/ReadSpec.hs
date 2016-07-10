{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module:      ReadSpec
Copyright:   (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

@hspec@ tests for derived 'Read', 'Read1', and 'Read2' instances.
-}
module ReadSpec where

import Data.Deriving

import Prelude ()
import Prelude.Compat

import Test.Hspec

import Types.ReadShow ()

-------------------------------------------------------------------------------

-- Plain data types

data TyCon# a b = TyCon# {
    tcA# :: a
  , tcB# :: b
}

-- Data families

data family TyFamily# y z :: *

data instance TyFamily# a b = TyFamily# {
    tfA# :: a
  , tfB# :: b
}

-------------------------------------------------------------------------------

-- Plain data types

$(deriveRead  ''TyCon#)
$(deriveRead1 ''TyCon#)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveRead2 ''TyCon#)
#endif

#if MIN_VERSION_template_haskell(2,7,0)
-- Data families

$(deriveRead  'TyFamily#)
$(deriveRead1 'TyFamily#)
# if defined(NEW_FUNCTOR_CLASSES)
$(deriveRead2 'TyFamily#)
# endif
#endif

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()

