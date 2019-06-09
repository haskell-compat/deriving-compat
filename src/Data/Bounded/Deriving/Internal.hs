{-# LANGUAGE CPP #-}

{-|
Module:      Data.Bounded.Deriving.Internal
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Bounded' instances.

Note: this is an internal module, and as such, the API presented here is not
guaranteed to be stable, even between minor releases of this library.
-}
module Data.Bounded.Deriving.Internal (
      -- * 'Bounded'
      deriveBounded
    , makeMinBound
    , makeMaxBound
    ) where

import Data.Deriving.Internal

import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Generates a 'Bounded' instance declaration for the given data type or data
-- family instance.
deriveBounded :: Name -> Q [Dec]
deriveBounded name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (instanceCxt, instanceType)
          <- buildTypeInstance BoundedClass parentName ctxt instTypes variant
      (:[]) `fmap` instanceD (return instanceCxt)
                   (return instanceType)
                   (boundedFunDecs parentName cons)

-- | Generates a lambda expression which behaves like 'minBound' (without
-- requiring a 'Bounded' instance).
makeMinBound :: Name -> Q Exp
makeMinBound = makeBoundedFun MinBound

-- | Generates a lambda expression which behaves like 'maxBound' (without
-- requiring a 'Bounded' instance).
makeMaxBound :: Name -> Q Exp
makeMaxBound = makeBoundedFun MaxBound

-- | Generates 'minBound' and 'maxBound' method declarations.
boundedFunDecs :: Name -> [ConstructorInfo] -> [Q Dec]
boundedFunDecs tyName cons = [makeFunD MinBound, makeFunD MaxBound]
  where
    makeFunD :: BoundedFun -> Q Dec
    makeFunD bf =
      funD (boundedFunName bf)
           [ clause []
                    (normalB $ makeBoundedFunForCons bf tyName cons)
                    []
           ]

-- | Generates a lambda expression which behaves like the BoundedFun argument.
makeBoundedFun :: BoundedFun -> Name -> Q Exp
makeBoundedFun bf name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      -- We force buildTypeInstance here since it performs some checks for whether
      -- or not the provided datatype can actually have minBound/maxBound
      -- implemented for it, and produces errors if it can't.
      buildTypeInstance BoundedClass parentName ctxt instTypes variant
        >> makeBoundedFunForCons bf parentName cons

-- | Generates a lambda expression for minBound/maxBound. for the
-- given constructors. All constructors must be from the same type.
makeBoundedFunForCons :: BoundedFun -> Name -> [ConstructorInfo] -> Q Exp
makeBoundedFunForCons _  _      [] = noConstructorsError
makeBoundedFunForCons bf tyName cons
    | not (isProduct || isEnumeration)
    = enumerationOrProductError $ nameBase tyName
    | isEnumeration
    = pickCon
    | otherwise -- It's a product type
    = pickConApp
  where
    isProduct, isEnumeration :: Bool
    isProduct     = isProductType cons
    isEnumeration = isEnumerationType cons

    con1, conN :: Q Exp
    con1 = conE $ constructorName $ head cons
    conN = conE $ constructorName $ last cons

    pickCon :: Q Exp
    pickCon = case bf of
                   MinBound -> con1
                   MaxBound -> conN

    pickConApp :: Q Exp
    pickConApp = appsE
               $ pickCon
               : map varE (replicate (conArity $ head cons) (boundedFunName bf))

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- There's only one Bounded variant!
data BoundedClass = BoundedClass

instance ClassRep BoundedClass where
    arity _ = 0

    allowExQuant _ = True

    fullClassName _ = boundedTypeName

    classConstraint _ 0 = Just $ boundedTypeName
    classConstraint _ _ = Nothing

-- | A representation of which function is being generated.
data BoundedFun = MinBound | MaxBound

boundedFunName :: BoundedFun -> Name
boundedFunName MinBound = minBoundValName
boundedFunName MaxBound = maxBoundValName
