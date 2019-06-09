{-|
Module:      Data.Enum.Deriving.Internal
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Enum' instances.

Note: this is an internal module, and as such, the API presented here is not
guaranteed to be stable, even between minor releases of this library.
-}
module Data.Enum.Deriving.Internal (
      -- * 'Enum'
      deriveEnum
    , makeSucc
    , makePred
    , makeToEnum
    , makeFromEnum
    , makeEnumFrom
    , makeEnumFromThen
    ) where

import Data.Deriving.Internal

import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Generates an 'Enum' instance declaration for the given data type or data
-- family instance.
deriveEnum :: Name -> Q [Dec]
deriveEnum name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (instanceCxt, instanceType)
          <- buildTypeInstance EnumClass parentName ctxt instTypes variant
      (:[]) `fmap` instanceD (return instanceCxt)
                             (return instanceType)
                             (enumFunDecs parentName instanceType cons)

-- | Generates a lambda expression which behaves like 'succ' (without
-- requiring an 'Enum' instance).
makeSucc :: Name -> Q Exp
makeSucc = makeEnumFun Succ

-- | Generates a lambda expression which behaves like 'pred' (without
-- requiring an 'Enum' instance).
makePred :: Name -> Q Exp
makePred = makeEnumFun Pred

-- | Generates a lambda expression which behaves like 'toEnum' (without
-- requiring an 'Enum' instance).
makeToEnum :: Name -> Q Exp
makeToEnum = makeEnumFun ToEnum

-- | Generates a lambda expression which behaves like 'fromEnum' (without
-- requiring an 'Enum' instance).
makeFromEnum :: Name -> Q Exp
makeFromEnum = makeEnumFun FromEnum

-- | Generates a lambda expression which behaves like 'enumFrom' (without
-- requiring an 'Enum' instance).
makeEnumFrom :: Name -> Q Exp
makeEnumFrom = makeEnumFun EnumFrom

-- | Generates a lambda expression which behaves like 'enumFromThen' (without
-- requiring an 'Enum' instance).
makeEnumFromThen :: Name -> Q Exp
makeEnumFromThen = makeEnumFun EnumFromThen

-- | Generates method declarations for an 'Enum' instance.
enumFunDecs :: Name -> Type -> [ConstructorInfo] -> [Q Dec]
enumFunDecs tyName ty cons =
    map makeFunD [ Succ
                 , Pred
                 , ToEnum
                 , EnumFrom
                 , EnumFromThen
                 , FromEnum
                 ]
  where
    makeFunD :: EnumFun -> Q Dec
    makeFunD ef =
      funD (enumFunName ef)
           [ clause []
                    (normalB $ makeEnumFunForCons ef tyName ty cons)
                    []
           ]

-- | Generates a lambda expression which behaves like the EnumFun argument.
makeEnumFun :: EnumFun -> Name -> Q Exp
makeEnumFun ef name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (_, instanceType) <- buildTypeInstance EnumClass parentName ctxt instTypes variant
      makeEnumFunForCons ef parentName instanceType cons

-- | Generates a lambda expression for fromEnum/toEnum/etc. for the
-- given constructors. All constructors must be from the same type.
makeEnumFunForCons :: EnumFun -> Name -> Type -> [ConstructorInfo] -> Q Exp
makeEnumFunForCons _  _      _  [] = noConstructorsError
makeEnumFunForCons ef tyName ty cons
    | not $ isEnumerationType cons
    = enumerationError tyNameBase
    | otherwise = case ef of
        Succ -> lamOneHash $ \aHash ->
          condE (varE eqValName `appE` maxTagExpr `appE`
                   (conE iHashDataName `appE` varE aHash))
                (illegalExpr "succ" tyNameBase
                             "tried to take `succ' of last tag in enumeration")
                (tag2Con `appE` (varE plusValName `appE`
                  (conE iHashDataName `appE` varE aHash) `appE` integerE 1))

        Pred -> lamOneHash $ \aHash ->
          condE (varE eqValName `appE` integerE 0 `appE`
                   (conE iHashDataName `appE` varE aHash))
                (illegalExpr "pred" tyNameBase
                             "tried to take `pred' of first tag in enumeration")
                (tag2Con `appE` (varE plusValName `appE`
                  (conE iHashDataName `appE` varE aHash) `appE` integerE (-1)))

        ToEnum -> lamOne $ \a ->
          condE (appsE [ varE andValName
                       , varE geValName `appE` varE a `appE` integerE 0
                       , varE leValName `appE` varE a `appE` maxTagExpr
                       ])
                (tag2Con `appE` varE a)
                (illegalToEnumTag tyNameBase maxTagExpr a)

        EnumFrom -> lamOneHash $ \aHash ->
          appsE [ varE mapValName
                , tag2Con
                , enumFromToExpr (conE iHashDataName `appE` varE aHash) maxTagExpr
                ]

        EnumFromThen -> do
          a     <- newName "a"
          aHash <- newName "a#"
          b     <- newName "b"
          bHash <- newName "b#"
          lamE [varP a, varP b] $ untagExpr [(a, aHash), (b, bHash)] $
              appE (varE mapValName `appE` tag2Con) $
                  enumFromThenToExpr
                    (conE iHashDataName `appE` varE aHash)
                    (conE iHashDataName `appE` varE bHash)
                    (condE (appsE [ varE gtValName
                                  , conE iHashDataName `appE` varE aHash
                                  , conE iHashDataName `appE` varE bHash
                                  ])
                           (integerE 0) maxTagExpr)

        FromEnum -> lamOneHash $ \aHash ->
          conE iHashDataName `appE` varE aHash

  where
    tyNameBase :: String
    tyNameBase = nameBase tyName

    maxTagExpr :: Q Exp
    maxTagExpr = integerE (length cons - 1) `sigE` conT intTypeName

    lamOne :: (Name -> Q Exp) -> Q Exp
    lamOne f = do
        a <- newName "a"
        lam1E (varP a) $ f a

    lamOneHash :: (Name -> Q Exp) -> Q Exp
    lamOneHash f = lamOne $ \a -> do
        aHash <- newName "a#"
        untagExpr [(a, aHash)] $ f aHash

    tag2Con :: Q Exp
    tag2Con = tag2ConExpr $ removeClassApp ty

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- There's only one Enum variant!
data EnumClass = EnumClass

instance ClassRep EnumClass where
    arity _ = 0

    allowExQuant _ = True

    fullClassName _ = enumTypeName

    classConstraint _ 0 = Just $ enumTypeName
    classConstraint _ _ = Nothing

-- | A representation of which function is being generated.
data EnumFun = Succ
             | Pred
             | ToEnum
             | FromEnum
             | EnumFrom
             | EnumFromThen
  deriving Show

enumFunName :: EnumFun -> Name
enumFunName Succ           = succValName
enumFunName Pred           = predValName
enumFunName ToEnum         = toEnumValName
enumFunName FromEnum       = fromEnumValName
enumFunName EnumFrom       = enumFromValName
enumFunName EnumFromThen   = enumFromThenValName

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

enumFromThenToExpr :: Q Exp -> Q Exp -> Q Exp -> Q Exp
enumFromThenToExpr f t1 t2 = varE enumFromThenToValName `appE` f `appE` t1 `appE` t2

illegalExpr :: String -> String -> String -> Q Exp
illegalExpr meth tp msg =
    varE errorValName `appE` stringE (meth ++ '{':tp ++ "}: " ++ msg)

illegalToEnumTag :: String -> Q Exp -> Name -> Q Exp
illegalToEnumTag tp maxtag a =
    appE (varE errorValName)
         (appE (appE (varE appendValName)
                     (stringE ("toEnum{" ++ tp ++ "}: tag(")))
               (appE (appE (appE
                 (varE showsPrecValName)
                 (integerE 0))
                 (varE a))
                 (appE (appE
                   (varE appendValName)
                   (stringE ") is outside of enumeration's range (0,"))
                   (appE (appE (appE
                         (varE showsPrecValName)
                         (integerE 0))
                         maxtag)
                         (stringE ")")))))
