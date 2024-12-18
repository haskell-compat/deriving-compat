{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

{-|
Module:      GH24Spec
Copyright:   (C) 2019 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

A regression test for
https://github.com/haskell-compat/deriving-compat/issues/24.
-}
module GH24Spec (main, spec) where

import Data.Deriving

import Test.Hspec

data family P (a :: j) (b :: k)
data instance P (a :: k) k = MkP deriving (Eq, Ord)

$(deriveEnum 'MkP)
$(deriveIx   'MkP)

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()
