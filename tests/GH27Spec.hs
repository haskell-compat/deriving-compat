{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

#if MIN_VERSION_template_haskell(2,12,0)
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-} -- Due to GHC #13512. Sigh.
#endif

{-|
Module:      GH27Spec
Copyright:   (C) 2019 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

A regression test for
https://github.com/haskell-compat/deriving-compat/issues/27.
We isolate this test case from the rest of "DerivingViaSpec" because
we have to disable @-Wunused-foralls@ due to
https://gitlab.haskell.org/ghc/ghc/issues/13512, and we would prefer not to
contaminate the rest of "DerivingViaSpec" with this hack.
-}
module GH27Spec where

import Prelude ()
import Prelude.Compat

import Test.Hspec

#if MIN_VERSION_template_haskell(2,12,0)
import Data.Deriving.Via
import Data.Functor.Const

newtype Age = MkAge Int
$(deriveVia [t| forall a. Show Age `Via` Const Int a |])
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()
