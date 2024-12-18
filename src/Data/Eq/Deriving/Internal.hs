{-# LANGUAGE GADTs #-}

{-|
Module:      Data.Eq.Deriving.Internal
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Eq', 'Eq1', and 'Eq2' instances.

Note: this is an internal module, and as such, the API presented here is not
guaranteed to be stable, even between minor releases of this library.
-}
module Data.Eq.Deriving.Internal (
      -- * 'Eq'
      deriveEq
    , makeEq
    , makeNotEq
      -- * 'Eq1'
    , deriveEq1
    , makeLiftEq
    , makeEq1
      -- * 'Eq2'
    , deriveEq2
    , makeLiftEq2
    , makeEq2
    ) where

import           Data.Deriving.Internal
import           Data.List (foldl1')
import qualified Data.Map as Map

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

-- | Generates an 'Eq' instance declaration for the given data type or data
-- family instance.
deriveEq :: Name -> Q [Dec]
deriveEq = deriveEqClass Eq

-- | Generates a lambda expression which behaves like '(==)' (without
-- requiring an 'Eq' instance).
makeEq :: Name -> Q Exp
makeEq = makeEqClass Eq

-- | Generates a lambda expression which behaves like '(/=)' (without
-- requiring an 'Eq' instance).
makeNotEq :: Name -> Q Exp
makeNotEq name = do
    x1 <- newName "x1"
    x2 <- newName "x2"
    lamE [varP x1, varP x2] $ varE notValName `appE`
        (makeEq name `appE` varE x1 `appE` varE x2)

-- | Generates an 'Eq1' instance declaration for the given data type or data
-- family instance.
deriveEq1 :: Name -> Q [Dec]
deriveEq1 = deriveEqClass Eq1

-- | Generates a lambda expression which behaves like 'liftEq' (without
-- requiring an 'Eq1' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftEq :: Name -> Q Exp
makeLiftEq = makeEqClass Eq1

-- | Generates a lambda expression which behaves like 'eq1' (without
-- requiring an 'Eq1' instance).
makeEq1 :: Name -> Q Exp
makeEq1 name = makeLiftEq name `appE` varE eqValName

-- | Generates an 'Eq2' instance declaration for the given data type or data
-- family instance.
--
-- This function is not available with @transformers-0.4@.
deriveEq2 :: Name -> Q [Dec]
deriveEq2 = deriveEqClass Eq2

-- | Generates a lambda expression which behaves like 'liftEq2' (without
-- requiring an 'Eq2' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftEq2 :: Name -> Q Exp
makeLiftEq2 = makeEqClass Eq2

-- | Generates a lambda expression which behaves like 'eq2' (without
-- requiring an 'Eq2' instance).
--
-- This function is not available with @transformers-0.4@.
makeEq2 :: Name -> Q Exp
makeEq2 name = makeLiftEq name `appE` varE eqValName `appE` varE eqValName

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive an Eq(1)(2) instance declaration (depending on the EqClass
-- argument's value).
deriveEqClass :: EqClass -> Name -> Q [Dec]
deriveEqClass eClass name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (instanceCxt, instanceType)
          <- buildTypeInstance eClass parentName ctxt instTypes variant
      (:[]) `fmap` instanceD (return instanceCxt)
                             (return instanceType)
                             (eqDecs eClass instTypes cons)

-- | Generates a declaration defining the primary function corresponding to a
-- particular class ((==) for Eq, liftEq for Eq1, and
-- liftEq2 for Eq2).
eqDecs :: EqClass -> [Type] -> [ConstructorInfo] -> [Q Dec]
eqDecs eClass instTypes cons =
    [ funD (eqName eClass)
           [ clause []
                    (normalB $ makeEqForCons eClass instTypes cons)
                    []
           ]
    ]

-- | Generates a lambda expression which behaves like (==) (for Eq),
-- liftEq (for Eq1), or liftEq2 (for Eq2).
makeEqClass :: EqClass -> Name -> Q Exp
makeEqClass eClass name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      -- We force buildTypeInstance here since it performs some checks for whether
      -- or not the provided datatype can actually have (==)/liftEq/etc.
      -- implemented for it, and produces errors if it can't.
      buildTypeInstance eClass parentName ctxt instTypes variant
        >> makeEqForCons eClass instTypes cons

-- | Generates a lambda expression for (==)/liftEq/etc. for the
-- given constructors. All constructors must be from the same type.
makeEqForCons :: EqClass -> [Type] -> [ConstructorInfo] -> Q Exp
makeEqForCons eClass instTypes cons = do
    value1 <- newName "value1"
    value2 <- newName "value2"
    eqDefn <- newName "eqDefn"
    eqs    <- newNameList "eq" $ arity eClass

    let lastTyVars = map varTToName $ drop (length instTypes - fromEnum eClass) instTypes
        tvMap      = Map.fromList $ zipWith (\x y -> (x, OneName y)) lastTyVars eqs

    lamE (map varP $ eqs ++ [value1, value2]
         ) . appsE
         $ [ varE $ eqConstName eClass
           , letE [ funD eqDefn [eqClause tvMap]
                  ] $ varE eqDefn `appE` varE value1 `appE` varE value2
           ] ++ map varE eqs
             ++ [varE value1, varE value2]
  where
    nonNullaryCons :: [ConstructorInfo]
    nonNullaryCons = filter (not . isNullaryCon) cons

    numNonNullaryCons :: Int
    numNonNullaryCons = length nonNullaryCons

    eqClause :: TyVarMap1 -> Q Clause
    eqClause tvMap
      | null cons
      = makeFallThroughCaseTrue
      -- Tag checking is redundant when there is only one data constructor
      | [con] <- cons
      = makeCaseForCon eClass tvMap con
      -- This is an enum (all constructors are nullary) - just do a simple tag check
      | all isNullaryCon cons
      = makeTagCase
      | otherwise
      = do abNames@(a, _, b, _) <- newABNames
           clause (map varP [a,b])
                  (normalB $ eqExprWithTagCheck tvMap abNames)
                  []

    eqExprWithTagCheck :: TyVarMap1 -> (Name, Name, Name, Name) ->  Q Exp
    eqExprWithTagCheck tvMap (a, aHash, b, bHash) =
      condE (untagExpr [(a, aHash), (b, bHash)]
                       (primOpAppExpr (varE aHash) neqIntHashValName (varE bHash)))
            (conE falseDataName)
            (caseE (varE a)
                   (map (mkNestedMatchesForCon eClass tvMap b) nonNullaryCons
                    ++ [ makeFallThroughMatchTrue
                       | 0 < numNonNullaryCons && numNonNullaryCons < length cons
                       ]))

newABNames :: Q (Name, Name, Name, Name)
newABNames = do
    a     <- newName "a"
    aHash <- newName "a#"
    b     <- newName "b"
    bHash <- newName "b#"
    return (a, aHash, b, bHash)

makeTagCase :: Q Clause
makeTagCase = do
    (a, aHash, b, bHash) <- newABNames
    clause (map varP [a,b])
           (normalB $ untagExpr [(a, aHash), (b, bHash)] $
               primOpAppExpr (varE aHash) eqIntHashValName (varE bHash)) []

makeFallThroughCaseTrue :: Q Clause
makeFallThroughCaseTrue = clause [wildP, wildP] (normalB $ conE trueDataName) []

makeFallThroughMatchFalse, makeFallThroughMatchTrue :: Q Match
makeFallThroughMatchFalse = makeFallThroughMatch falseDataName
makeFallThroughMatchTrue  = makeFallThroughMatch trueDataName

makeFallThroughMatch :: Name -> Q Match
makeFallThroughMatch dataName = match wildP (normalB $ conE dataName) []

makeCaseForCon :: EqClass -> TyVarMap1 -> ConstructorInfo -> Q Clause
makeCaseForCon eClass tvMap
  (ConstructorInfo { constructorName = conName, constructorFields = ts }) = do
    ts' <- mapM resolveTypeSynonyms ts
    let tsLen = length ts'
    as <- newNameList "a" tsLen
    bs <- newNameList "b" tsLen
    clause [conP conName (map varP as), conP conName (map varP bs)]
           (normalB $ makeCaseForArgs eClass tvMap conName ts' as bs)
           []

mkNestedMatchesForCon :: EqClass -> TyVarMap1 -> Name -> ConstructorInfo -> Q Match
mkNestedMatchesForCon eClass tvMap b
  (ConstructorInfo { constructorName = conName, constructorFields = ts }) = do
    ts' <- mapM resolveTypeSynonyms ts
    let tsLen = length ts'
    as <- newNameList "a" tsLen
    bs <- newNameList "b" tsLen
    match (conP conName (map varP as))
          (normalB $ caseE (varE b)
                           [ match (conP conName (map varP bs))
                                   (normalB $ makeCaseForArgs eClass tvMap conName ts' as bs)
                                   []
                           , makeFallThroughMatchFalse
                           ])
          []

makeCaseForArgs :: EqClass
                -> TyVarMap1
                -> Name
                -> [Type]
                -> [Name]
                -> [Name]
                -> Q Exp
makeCaseForArgs _ _ _ [] [] [] = conE trueDataName
makeCaseForArgs eClass tvMap conName tys as bs =
    foldl1' (\q e -> infixApp q (varE andValName) e)
            (zipWith3 (makeCaseForArg eClass tvMap conName) tys as bs)

makeCaseForArg :: EqClass
               -> TyVarMap1
               -> Name
               -> Type
               -> Name
               -> Name
               -> Q Exp
makeCaseForArg _ _ _ (ConT tyName) a b = primEqExpr
  where
    aExpr, bExpr :: Q Exp
    aExpr = varE a
    bExpr = varE b

    makePrimEqExpr :: Name -> Q Exp
    makePrimEqExpr n = primOpAppExpr aExpr n bExpr

    primEqExpr :: Q Exp
    primEqExpr =
      case Map.lookup tyName primOrdFunTbl of
        Just (_, _, eq, _, _) -> makePrimEqExpr eq
        Nothing               -> infixApp aExpr (varE eqValName) bExpr
makeCaseForArg eClass tvMap conName ty a b =
    makeCaseForType eClass tvMap conName ty `appE` varE a `appE` varE b

makeCaseForType :: EqClass
                -> TyVarMap1
                -> Name
                -> Type
                -> Q Exp
makeCaseForType _ tvMap _ (VarT tyName) =
    varE $ case Map.lookup tyName tvMap of
      Just (OneName eq) -> eq
      Nothing           -> eqValName
makeCaseForType eClass tvMap conName (SigT ty _)      = makeCaseForType eClass tvMap conName ty
makeCaseForType eClass tvMap conName (ForallT _ _ ty) = makeCaseForType eClass tvMap conName ty
makeCaseForType eClass tvMap conName ty = do
    let tyCon :: Type
        tyArgs :: [Type]
        (tyCon, tyArgs) = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (arity eClass) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNames :: [Name]
        tyVarNames = Map.keys tvMap

    itf <- isInTypeFamilyApp tyVarNames tyCon tyArgs
    if any (`mentionsName` tyVarNames) lhsArgs
          || itf && any (`mentionsName` tyVarNames) tyArgs
       then outOfPlaceTyVarError eClass conName
       else if any (`mentionsName` tyVarNames) rhsArgs
               then appsE $ [ varE . eqName $ toEnum numLastArgs]
                            ++ map (makeCaseForType eClass tvMap conName) rhsArgs
               else varE eqValName

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which @Eq@ variant is being derived.
data EqClass = Eq
             | Eq1
             | Eq2
  deriving (Bounded, Enum)

instance ClassRep EqClass where
    arity = fromEnum

    allowExQuant _ = True

    fullClassName Eq  = eqTypeName
    fullClassName Eq1 = eq1TypeName
    fullClassName Eq2 = eq2TypeName

    classConstraint eClass i
      | eMin <= i && i <= eMax = Just $ fullClassName (toEnum i :: EqClass)
      | otherwise              = Nothing
      where
        eMin, eMax :: Int
        eMin = fromEnum (minBound :: EqClass)
        eMax = fromEnum eClass

eqConstName :: EqClass -> Name
eqConstName Eq  = eqConstValName
eqConstName Eq1 = liftEqConstValName
eqConstName Eq2 = liftEq2ConstValName

eqName :: EqClass -> Name
eqName Eq  = eqValName
eqName Eq1 = liftEqValName
eqName Eq2 = liftEq2ValName
