{-# LANGUAGE CPP #-}

{-|
Module:      Data.Deriving.Via
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

On @template-haskell-2.12@ or later (i.e., GHC 8.2 or later), this module
exports functionality which emulates the @GeneralizedNewtypeDeriving@ and
@DerivingVia@ GHC extensions (the latter of which was introduced in GHC 8.6).

On older versions of @template-haskell@/GHC, this module does not export
anything.
-}
module Data.Deriving.Via (
#if !(MIN_VERSION_template_haskell(2,12,0))
  ) where
#else
    -- * @GeneralizedNewtypeDeriving@
    deriveGND
    -- * @DerivingVia@
  , deriveVia
  , Via
    -- * Limitations
    -- $constraints
  ) where

import Data.Deriving.Internal (Via)
import Data.Deriving.Via.Internal

{- $constraints

Be aware of the following potential gotchas:

* Unlike every other module in this library, the functions exported by
  "Data.Deriving.Via" only support GHC 8.2 and later, as they require
  Template Haskell functionality not present in earlier GHCs.

* Additionally, using the functions in "Data.Deriving.Via" will likely
  require you to enable some language extensions (besides @TemplateHaskell@).
  These may include:

    * @ImpredicativeTypes@ (if any class methods contain higher-rank types)

    * @InstanceSigs@

    * @KindSignatures@

    * @RankNTypes@

    * @ScopedTypeVariables@

    * @TypeApplications@

    * @TypeOperators@

    * @UndecidableInstances@ (if deriving an instance of a type class with
      associated type families)

* The functions in "Data.Deriving.Via" are not terribly robust in the presence
  of @PolyKinds@. Alas, Template Haskell does not make this easy to fix.

* The functions in "Data.Deriving.Via" make a best-effort attempt to derive
  instances for classes with associated type families. This is known not to
  work in all scenarios, however, especially when the last parameter to a type
  class appears as a kind variable in an associated type family. (See
  <https://ghc.haskell.org/trac/ghc/ticket/14728 Trac #14728>.)
-}
#endif
