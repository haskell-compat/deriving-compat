{-|
Module:      Data.Ix.Deriving.Internal
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Ix' instances.

Note: this is an internal module, and as such, the API presented here is not
guaranteed to be stable, even between minor releases of this library.
-}
module Data.Ix.Deriving.Internal (
      -- * 'Ix'
      deriveIx
    , makeRange
    , makeUnsafeIndex
    , makeInRange
    ) where

import Data.Deriving.Internal

import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Generates a 'Ix' instance declaration for the given data type or data
-- family instance.
deriveIx :: Name -> Q [Dec]
deriveIx name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (instanceCxt, instanceType)
          <- buildTypeInstance IxClass parentName ctxt instTypes variant
      (:[]) `fmap` instanceD (return instanceCxt)
                             (return instanceType)
                             (ixFunDecs parentName instanceType cons)

-- | Generates a lambda expression which behaves like 'range' (without
-- requiring an 'Ix' instance).
makeRange :: Name -> Q Exp
makeRange = makeIxFun Range

-- | Generates a lambda expression which behaves like 'unsafeIndex' (without
-- requiring an 'Ix' instance).
makeUnsafeIndex :: Name -> Q Exp
makeUnsafeIndex = makeIxFun UnsafeIndex

-- | Generates a lambda expression which behaves like 'inRange' (without
-- requiring an 'Ix' instance).
makeInRange :: Name -> Q Exp
makeInRange = makeIxFun InRange

-- | Generates method declarations for an 'Ix' instance.
ixFunDecs :: Name -> Type -> [ConstructorInfo] -> [Q Dec]
ixFunDecs tyName ty cons =
    [ makeFunD Range
    , makeFunD UnsafeIndex
    , makeFunD InRange
    ]
  where
    makeFunD :: IxFun -> Q Dec
    makeFunD ixf =
      funD (ixFunName ixf)
           [ clause []
                    (normalB $ makeIxFunForCons ixf tyName ty cons)
                    []
           ]

-- | Generates a lambda expression which behaves like the IxFun argument.
makeIxFun :: IxFun -> Name -> Q Exp
makeIxFun ixf name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (_, instanceType) <- buildTypeInstance IxClass parentName ctxt instTypes variant
      makeIxFunForCons ixf parentName instanceType cons

-- | Generates a lambda expression for an 'Ix' method for the
-- given constructors. All constructors must be from the same type.
makeIxFunForCons :: IxFun -> Name -> Type -> [ConstructorInfo] -> Q Exp
makeIxFunForCons _   _      _  [] = noConstructorsError
makeIxFunForCons ixf tyName ty cons
    | not (isProduct || isEnumeration)
    = enumerationOrProductError $ nameBase tyName
    | isEnumeration
    = case ixf of
        Range -> do
          a     <- newName "a"
          aHash <- newName "a#"
          b     <- newName "b"
          bHash <- newName "b#"
          lamE [tupP [varP a, varP b]] $
              untagExpr [(a, aHash)] $
              untagExpr [(b, bHash)] $
              appE (varE mapValName `appE` tag2Con) $
                  enumFromToExpr (conE iHashDataName `appE` varE aHash)
                                 (conE iHashDataName `appE` varE bHash)

        UnsafeIndex -> do
          a     <- newName "a"
          aHash <- newName "a#"
          c     <- newName "c"
          cHash <- newName "c#"
          dHash <- newName "d#"
          lamE [tupP [varP a, wildP], varP c] $
              untagExpr [(a, aHash)] $
              untagExpr [(c, cHash)] $
              caseE (infixApp (varE cHash) (varE minusIntHashValName) (varE aHash))
                  [ match (varP dHash)
                          (normalB $ conE iHashDataName `appE` varE dHash)
                          []
                  ]

        InRange -> do
          a     <- newName "a"
          aHash <- newName "a#"
          b     <- newName "b"
          bHash <- newName "b#"
          c     <- newName "c"
          cHash <- newName "c#"
          lamE [tupP [varP a, varP b], varP c] $
              untagExpr [(a, aHash)] $
              untagExpr [(b, bHash)] $
              untagExpr [(c, cHash)] $
              appsE [ varE andValName
                    , primOpAppExpr (varE cHash) geIntHashValName (varE aHash)
                    , primOpAppExpr (varE cHash) leIntHashValName (varE bHash)
                    ]

    | otherwise -- It's a product type
    = do let con :: ConstructorInfo
             con = head cons

             conName :: Name
             conName = constructorName con

             conFields :: Int
             conFields = conArity con

         as <- newNameList "a" conFields
         bs <- newNameList "b" conFields
         cs <- newNameList "c" conFields

         let conPat :: [Name] -> Q Pat
             conPat = conP conName . map varP

             conExpr :: Q Exp
             conExpr = appsE $ conE conName : map varE cs

         case ixf of
           Range -> lamE [tupP [conPat as, conPat bs]] $
               compE $ stmts ++ [noBindS conExpr]
             where
               stmts :: [Q Stmt]
               stmts = zipWith3 mkQual as bs cs

               mkQual :: Name -> Name -> Name -> Q Stmt
               mkQual a b c = bindS (varP c) $
                   varE rangeValName `appE` tupE [varE a, varE b]

           UnsafeIndex -> lamE [tupP [conPat as, conPat bs], conPat cs] $
               mkUnsafeIndex $ reverse $ zip3 as bs cs
             where
               mkUnsafeIndex :: [(Name, Name, Name)] -> Q Exp
               mkUnsafeIndex []          = integerE 0
               mkUnsafeIndex [(l, u, i)] = mkOne l u i
               mkUnsafeIndex ((l, u, i):rest) =
                   infixApp (mkOne l u i)
                            (varE plusValName)
                            (infixApp (varE unsafeRangeSizeValName
                                         `appE` tupE [varE l, varE u])
                                      (varE timesValName)
                                      (mkUnsafeIndex rest))

               mkOne :: Name -> Name -> Name -> Q Exp
               mkOne l u i = varE unsafeIndexValName `appE` tupE [varE l, varE u]
                                                     `appE` varE i

           InRange -> lamE [tupP [conPat as, conPat bs], conPat cs] $
               if conFields == 0
                  then conE trueDataName
                  else foldl1 andExpr $ zipWith3 mkInRange as bs cs
             where
               andExpr :: Q Exp -> Q Exp -> Q Exp
               andExpr a b = infixApp a (varE andValName) b

               mkInRange :: Name -> Name -> Name -> Q Exp
               mkInRange a b c = varE inRangeValName `appE` tupE [varE a, varE b]
                                                     `appE` varE c
  where
    isProduct, isEnumeration :: Bool
    isProduct     = isProductType cons
    isEnumeration = isEnumerationType cons

    tag2Con :: Q Exp
    tag2Con = tag2ConExpr $ removeClassApp ty

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- There's only one Ix variant!
data IxClass = IxClass

instance ClassRep IxClass where
    arity _ = 0

    allowExQuant _ = True

    fullClassName _ = ixTypeName

    classConstraint _ 0 = Just ixTypeName
    classConstraint _ _ = Nothing

-- | A representation of which function is being generated.
data IxFun = Range
           | UnsafeIndex
           | InRange
  deriving Show

ixFunName :: IxFun -> Name
ixFunName Range       = rangeValName
ixFunName UnsafeIndex = unsafeIndexValName
ixFunName InRange     = inRangeValName
