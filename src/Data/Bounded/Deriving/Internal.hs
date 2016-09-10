{-# LANGUAGE CPP #-}

{-|
Module:      Data.Bounded.Deriving.Internal
Copyright:   (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Bounded' instances.
-}
module Data.Bounded.Deriving.Internal (
      -- * 'Bounded'
      deriveBounded
    , makeMinBound
    , makeMaxBound
    ) where

import Data.Deriving.Internal

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Generates a 'Bounded' instance declaration for the given data type or data
-- family instance.
deriveBounded :: Name -> Q [Dec]
deriveBounded name = withType name fromCons
  where
    fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q [Dec]
    fromCons name' ctxt tvbs cons mbTys = (:[]) `fmap` do
        (instanceCxt, instanceType)
            <- buildTypeInstance BoundedClass name' ctxt tvbs mbTys
        instanceD (return instanceCxt)
                  (return instanceType)
                  (boundedFunDecs name' cons)

-- | Generates a lambda expression which behaves like 'minBound' (without
-- requiring a 'Bounded' instance).
makeMinBound :: Name -> Q Exp
makeMinBound = makeBoundedFun MinBound

-- | Generates a lambda expression which behaves like 'maxBound' (without
-- requiring a 'Bounded' instance).
makeMaxBound :: Name -> Q Exp
makeMaxBound = makeBoundedFun MaxBound

-- | Generates 'minBound' and 'maxBound' method declarations.
boundedFunDecs :: Name -> [Con] -> [Q Dec]
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
makeBoundedFun bf name = withType name fromCons where
  fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q Exp
  fromCons name' ctxt tvbs cons mbTys =
    -- We force buildTypeInstance here since it performs some checks for whether
    -- or not the provided datatype can actually have minBound/maxBound
    -- implemented for it, and produces errors if it can't.
    buildTypeInstance BoundedClass name' ctxt tvbs mbTys
      `seq` makeBoundedFunForCons bf name' cons

-- | Generates a lambda expression for minBound/maxBound. for the
-- given constructors. All constructors must be from the same type.
makeBoundedFunForCons :: BoundedFun -> Name -> [Con] -> Q Exp
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