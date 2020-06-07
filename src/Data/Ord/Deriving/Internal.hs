{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

{-|
Module:      Data.Ord.Deriving.Internal
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Ord', 'Ord1', and 'Ord2' instances.

Note: this is an internal module, and as such, the API presented here is not
guaranteed to be stable, even between minor releases of this library.
-}
module Data.Ord.Deriving.Internal (
      -- * 'Ord'
      deriveOrd
    , makeCompare
    , makeLE
    , makeLT
    , makeGT
    , makeGE
    , makeMax
    , makeMin
      -- * 'Ord1'
    , deriveOrd1
#if defined(NEW_FUNCTOR_CLASSES)
    , makeLiftCompare
#endif
    , makeCompare1
#if defined(NEW_FUNCTOR_CLASSES)
      -- * 'Ord2'
    , deriveOrd2
    , makeLiftCompare2
    , makeCompare2
#endif
    ) where

import           Data.Deriving.Internal
import           Data.List (partition)
import qualified Data.Map as Map
import           Data.Map (Map)

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

-- | Generates an 'Ord' instance declaration for the given data type or data
-- family instance.
deriveOrd :: Name -> Q [Dec]
deriveOrd = deriveOrdClass Ord

-- | Generates a lambda expression which behaves like 'compare' (without
-- requiring an 'Ord' instance).
makeCompare :: Name -> Q Exp
makeCompare = makeOrdFun OrdCompare (error "This shouldn't happen")

-- | Generates a lambda expression which behaves like '(<)' (without
-- requiring an 'Ord' instance).
makeLT :: Name -> Q Exp
makeLT = makeOrdFun OrdLT [ match (conP ltDataName []) (normalB $ conE trueDataName)  []
                          , match wildP                (normalB $ conE falseDataName) []
                          ]

-- | Generates a lambda expression which behaves like '(<=)' (without
-- requiring an 'Ord' instance).
makeLE :: Name -> Q Exp
makeLE = makeOrdFun OrdLE [ match (conP gtDataName []) (normalB $ conE falseDataName) []
                          , match wildP                (normalB $ conE trueDataName)  []
                          ]

-- | Generates a lambda expression which behaves like '(>)' (without
-- requiring an 'Ord' instance).
makeGT :: Name -> Q Exp
makeGT = makeOrdFun OrdGT [ match (conP gtDataName []) (normalB $ conE trueDataName)  []
                          , match wildP                (normalB $ conE falseDataName) []
                          ]

-- | Generates a lambda expression which behaves like '(>=)' (without
-- requiring an 'Ord' instance).
makeGE :: Name -> Q Exp
makeGE = makeOrdFun OrdGE [ match (conP ltDataName []) (normalB $ conE falseDataName) []
                          , match wildP                (normalB $ conE trueDataName)  []
                          ]

-- | Generates a lambda expression which behaves like 'max' (without
-- requiring an 'Ord' instance).
makeMax :: Name -> Q Exp
makeMax = makeMinMax flip

-- | Generates a lambda expression which behaves like 'min' (without
-- requiring an 'Ord' instance).
makeMin :: Name -> Q Exp
makeMin = makeMinMax id

makeMinMax :: ((Q Exp -> Q Exp -> Q Exp) -> Q Exp -> Q Exp -> Q Exp)
           -> Name -> Q Exp
makeMinMax f name = do
    x <- newName "x"
    y <- newName "y"
    let xExpr = varE x
        yExpr = varE y
    lamE [varP x, varP y] $
        f (condE $ makeLE name `appE` xExpr `appE` yExpr) xExpr yExpr

-- | Generates an 'Ord1' instance declaration for the given data type or data
-- family instance.
deriveOrd1 :: Name -> Q [Dec]
deriveOrd1 = deriveOrdClass Ord1

#if defined(NEW_FUNCTOR_CLASSES)
-- | Generates a lambda expression which behaves like 'liftCompare' (without
-- requiring an 'Ord1' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftCompare :: Name -> Q Exp
makeLiftCompare = makeOrdFun Ord1LiftCompare (error "This shouldn't happen")

-- | Generates a lambda expression which behaves like 'compare1' (without
-- requiring an 'Ord1' instance).
makeCompare1 :: Name -> Q Exp
makeCompare1 name = makeLiftCompare name `appE` varE compareValName
#else
-- | Generates a lambda expression which behaves like 'compare1' (without
-- requiring an 'Ord1' instance).
makeCompare1 :: Name -> Q Exp
makeCompare1 = makeOrdFun Ord1Compare1 (error "This shouldn't happen")
#endif

#if defined(NEW_FUNCTOR_CLASSES)
-- | Generates an 'Ord2' instance declaration for the given data type or data
-- family instance.
--
-- This function is not available with @transformers-0.4@.
deriveOrd2 :: Name -> Q [Dec]
deriveOrd2 = deriveOrdClass Ord2

-- | Generates a lambda expression which behaves like 'liftCompare2' (without
-- requiring an 'Ord2' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftCompare2 :: Name -> Q Exp
makeLiftCompare2 = makeOrdFun Ord2LiftCompare2 (error "This shouldn't happen")

-- | Generates a lambda expression which behaves like 'compare2' (without
-- requiring an 'Ord2' instance).
--
-- This function is not available with @transformers-0.4@.
makeCompare2 :: Name -> Q Exp
makeCompare2 name = makeLiftCompare name
             `appE` varE compareValName
             `appE` varE compareValName
#endif

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive an Ord(1)(2) instance declaration (depending on the OrdClass
-- argument's value).
deriveOrdClass :: OrdClass -> Name -> Q [Dec]
deriveOrdClass oClass name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (instanceCxt, instanceType)
          <- buildTypeInstance oClass parentName ctxt instTypes variant
      (:[]) `fmap` instanceD (return instanceCxt)
                             (return instanceType)
                             (ordFunDecs oClass instTypes cons)

-- | Generates a declaration defining the primary function(s) corresponding to a
-- particular class (compare for Ord, liftCompare for Ord1, and
-- liftCompare2 for Ord2).
ordFunDecs :: OrdClass -> [Type] -> [ConstructorInfo] -> [Q Dec]
ordFunDecs oClass instTypes cons =
    map makeFunD $ ordClassToCompare oClass : otherFuns oClass cons
  where
    makeFunD :: OrdFun -> Q Dec
    makeFunD oFun =
      funD (ordFunName oFun $ arity oClass)
           [ clause []
                    (normalB $ dispatchFun oFun)
                    []
           ]

    negateExpr :: Q Exp -> Q Exp
    negateExpr = appE (varE notValName)

    dispatchLT :: (Q Exp -> Q Exp -> Q Exp -> Q Exp) -> Q Exp
    dispatchLT f = do
        x <- newName "x"
        y <- newName "y"
        lamE [varP x, varP y] $ f (varE ltValName) (varE x) (varE y)

    dispatchFun :: OrdFun -> Q Exp
    dispatchFun oFun | oFun `elem` [ OrdCompare, OrdLT
                                     -- OrdLT is included to mirror the fix to
                                     -- GHC Trac #10858.
#if defined(NEW_FUNCTOR_CLASSES)
                                   , Ord1LiftCompare, Ord2LiftCompare2
#else
                                   , Ord1Compare1
#endif
                                   ]
                      = makeOrdFunForCons oFun instTypes cons
    dispatchFun OrdLE = dispatchLT $ \lt x y -> negateExpr $ lt `appE` y `appE` x
    dispatchFun OrdGT = dispatchLT $ \lt x y ->              lt `appE` y `appE` x
    dispatchFun OrdGE = dispatchLT $ \lt x y -> negateExpr $ lt `appE` x `appE` y
    dispatchFun _     = fail "ordFunDecs"

-- | Generates a lambda expression which behaves like the OrdFun value. This
-- function uses heuristics to determine whether to implement the OrdFun from
-- scratch or define it in terms of compare.
makeOrdFun :: OrdFun -> [Q Match] -> Name -> Q Exp
makeOrdFun oFun matches name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      let oClass = ordFunToClass oFun
          others = otherFuns oClass cons
      -- We force buildTypeInstance here since it performs some checks for whether
      -- or not the provided datatype can actually have compare/liftCompare/etc.
      -- implemented for it, and produces errors if it can't.
      buildTypeInstance oClass parentName ctxt instTypes variant >>
        if oFun `elem` compareFuns || oFun `elem` others
           then makeOrdFunForCons oFun instTypes cons
           else do
             x <- newName "x"
             y <- newName "y"
             lamE [varP x, varP y] $
                  caseE (makeOrdFunForCons (ordClassToCompare oClass) instTypes cons
                             `appE` varE x `appE` varE y)
                        matches
  where
    compareFuns :: [OrdFun]
    compareFuns = [ OrdCompare
#if defined(NEW_FUNCTOR_CLASSES)
                  , Ord1LiftCompare
                  , Ord2LiftCompare2
#else
                  , Ord1Compare1
#endif
                  ]

-- | Generates a lambda expression for the given constructors.
-- All constructors must be from the same type.
makeOrdFunForCons :: OrdFun -> [Type] -> [ConstructorInfo] -> Q Exp
makeOrdFunForCons oFun instTypes cons = do
    let oClass = ordFunToClass oFun
    v1     <- newName "v1"
    v2     <- newName "v2"
    v1Hash <- newName "v1#"
    v2Hash <- newName "v2#"
    ords   <- newNameList "ord" $ arity oClass

    let lastTyVars :: [Name]
        lastTyVars = map varTToName $ drop (length instTypes - fromEnum oClass) instTypes

        tvMap :: TyVarMap1
        tvMap = Map.fromList $ zipWith (\x y -> (x, OneName y)) lastTyVars ords

        nullaryCons, nonNullaryCons :: [ConstructorInfo]
        (nullaryCons, nonNullaryCons) = partition isNullaryCon cons

        singleConType :: Bool
        singleConType = isSingleton cons

        firstConName, lastConName :: Name
        firstConName = constructorName $ head cons
        lastConName  = constructorName $ last cons

        -- Alternatively, we could look these up from dataConTagMap, but this
        -- is slightly faster due to the lack of Map lookups.
        firstTag, lastTag :: Int
        firstTag = 0
        lastTag  = length cons - 1

        dataConTagMap :: Map Name Int
        dataConTagMap = Map.fromList $ zip (map constructorName cons) [0..]

        ordMatches :: ConstructorInfo -> Q Match
        ordMatches = makeOrdFunForCon oFun v2 v2Hash tvMap singleConType
                                      firstTag firstConName lastTag lastConName
                                      dataConTagMap

        ordFunRhs :: Q Exp
        ordFunRhs
          | null cons
          = conE eqDataName
          | length nullaryCons <= 2
          = caseE (varE v1) $ map ordMatches cons
          | null nonNullaryCons
          = mkTagCmp
          | otherwise
          = caseE (varE v1) $ map ordMatches nonNullaryCons
                ++ [match wildP (normalB mkTagCmp) []]

        mkTagCmp :: Q Exp
        mkTagCmp = untagExpr [(v1, v1Hash), (v2, v2Hash)] $
                       unliftedOrdFun intHashTypeName oFun v1Hash v2Hash

    lamE (map varP $
#if defined(NEW_FUNCTOR_CLASSES)
                     ords ++
#endif
                     [v1, v2])
        . appsE
        $ [ varE $ compareConstName oFun
          , ordFunRhs
          ]
#if defined(NEW_FUNCTOR_CLASSES)
            ++ map varE ords
#endif
            ++ [varE v1, varE v2]

makeOrdFunForCon :: OrdFun
                 -> Name
                 -> Name
                 -> TyVarMap1
                 -> Bool
                 -> Int -> Name
                 -> Int -> Name
                 -> Map Name Int
                 -> ConstructorInfo -> Q Match
makeOrdFunForCon oFun v2 v2Hash tvMap singleConType
                 firstTag firstConName lastTag lastConName dataConTagMap
  (ConstructorInfo { constructorName = conName, constructorFields = ts }) = do
    ts' <- mapM resolveTypeSynonyms ts
    let tsLen = length ts'
    as <- newNameList "a" tsLen
    bs <- newNameList "b" tsLen

    let innerRhs :: Q Exp
        innerRhs
          | singleConType
          = caseE (varE v2) [innerEqAlt]

          | tag == firstTag
          = caseE (varE v2) [innerEqAlt, match wildP (normalB $ ltResult oFun) []]

          | tag == lastTag
          = caseE (varE v2) [innerEqAlt, match wildP (normalB $ gtResult oFun) []]

          | tag == firstTag + 1
          = caseE (varE v2) [ match (recP firstConName []) (normalB $ gtResult oFun) []
                            , innerEqAlt
                            , match wildP (normalB $ ltResult oFun) []
                            ]

          | tag == lastTag - 1
          = caseE (varE v2) [ match (recP lastConName []) (normalB $ ltResult oFun) []
                            , innerEqAlt
                            , match wildP (normalB $ gtResult oFun) []
                            ]

          | tag > lastTag `div` 2
          = untagExpr [(v2, v2Hash)] $
            condE (primOpAppExpr (varE v2Hash) ltIntHashValName tagLit)
                  (gtResult oFun) $
            caseE (varE v2) [innerEqAlt, match wildP (normalB $ ltResult oFun) []]

          | otherwise
          = untagExpr [(v2, v2Hash)] $
            condE (primOpAppExpr (varE v2Hash) gtIntHashValName tagLit)
                  (ltResult oFun) $
            caseE (varE v2) [innerEqAlt, match wildP (normalB $ gtResult oFun) []]

        innerEqAlt :: Q Match
        innerEqAlt = match (conP conName $ map varP bs)
                           (normalB $ makeOrdFunForFields oFun tvMap conName ts' as bs)
                           []

        tagLit :: Q Exp
        tagLit = litE . intPrimL $ fromIntegral tag

    match (conP conName $ map varP as)
          (normalB innerRhs)
          []
  where
    tag = dataConTagMap Map.! conName

makeOrdFunForFields :: OrdFun
                    -> TyVarMap1
                    -> Name
                    -> [Type]
                    -> [Name]
                    -> [Name]
                    -> Q Exp
makeOrdFunForFields oFun tvMap conName = go
  where
    go :: [Type] -> [Name] -> [Name] -> Q Exp
    go [] _ _ = eqResult oFun
    go [ty] [a] [b]
      | isSupportedUnliftedType ty = unliftedOrdFun (conTToName ty) oFun a b
      | otherwise = makeOrdFunForType oFun tvMap conName ty
                        `appE` varE a `appE` varE b
    go (ty:tys) (a:as) (b:bs) =
        mkCompare ty a b (ltResult oFun) (go tys as bs) (gtResult oFun)
    go _ _ _ = fail "Data.Ord.Deriving.Internal.makeOrdFunForFields"

    mkCompare :: Type -> Name -> Name -> Q Exp -> Q Exp -> Q Exp -> Q Exp
    mkCompare ty a b lt eq gt
      | isSupportedUnliftedType ty =
          let (ltFun, _, eqFun, _, _) = primOrdFuns $ conTToName ty
          in unliftedCompare ltFun eqFun aExpr bExpr lt eq gt
      | otherwise
      = caseE (makeOrdFunForType (ordClassToCompare $ ordFunToClass oFun)
                   tvMap conName ty `appE` aExpr `appE` bExpr)
              [ match (conP ltDataName []) (normalB lt) []
              , match (conP eqDataName []) (normalB eq) []
              , match (conP gtDataName []) (normalB gt) []
              ]
      where
        aExpr, bExpr :: Q Exp
        aExpr = varE a
        bExpr = varE b

makeOrdFunForType :: OrdFun
                  -> TyVarMap1
                  -> Name
                  -> Type
                  -> Q Exp
#if defined(NEW_FUNCTOR_CLASSES)
makeOrdFunForType oFun tvMap _ (VarT tyName) =
    varE $ case Map.lookup tyName tvMap of
      Just (OneName ord) -> ord
      Nothing            -> ordFunName oFun 0
#else
makeOrdFunForType oFun _ _ VarT{} = varE $ ordFunName oFun 0
#endif
makeOrdFunForType oFun tvMap conName (SigT ty _)      = makeOrdFunForType oFun tvMap conName ty
makeOrdFunForType oFun tvMap conName (ForallT _ _ ty) = makeOrdFunForType oFun tvMap conName ty
#if defined(NEW_FUNCTOR_CLASSES)
makeOrdFunForType oFun tvMap conName ty = do
    let oClass :: OrdClass
        oClass = ordFunToClass oFun

        tyCon :: Type
        tyArgs :: [Type]
        (tyCon, tyArgs) = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (arity oClass) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNames :: [Name]
        tyVarNames = Map.keys tvMap

    itf <- isInTypeFamilyApp tyVarNames tyCon tyArgs
    if any (`mentionsName` tyVarNames) lhsArgs
          || itf && any (`mentionsName` tyVarNames) tyArgs
       then outOfPlaceTyVarError oClass conName
       else if any (`mentionsName` tyVarNames) rhsArgs
               then appsE $ [ varE . ordFunName oFun $ toEnum numLastArgs]
                            ++ map (makeOrdFunForType oFun tvMap conName) rhsArgs
               else varE $ ordFunName oFun 0
#else
makeOrdFunForType oFun tvMap conName ty = do
  let varNames = Map.keys tvMap
      oClass   = ordFunToClass oFun

  a' <- newName "a'"
  b' <- newName "b'"
  case varNames of
    [] -> varE $ ordFunName oFun 0
    varName:_ ->
      if mentionsName ty varNames
         then lamE (map varP [a',b']) $ varE (ordFunName oFun 1)
                `appE` (makeFmapApplyNeg oClass conName ty varName `appE` varE a')
                `appE` (makeFmapApplyNeg oClass conName ty varName `appE` varE b')
         else varE $ ordFunName oFun 0
#endif

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which @Ord@ variant is being derived.
data OrdClass = Ord
              | Ord1
#if defined(NEW_FUNCTOR_CLASSES)
              | Ord2
#endif
  deriving (Bounded, Enum)

instance ClassRep OrdClass where
    arity = fromEnum

    allowExQuant _ = True

    fullClassName Ord  = ordTypeName
    fullClassName Ord1 = ord1TypeName
#if defined(NEW_FUNCTOR_CLASSES)
    fullClassName Ord2 = ord2TypeName
#endif

    classConstraint oClass i
      | oMin <= i && i <= oMax = Just $ fullClassName (toEnum i :: OrdClass)
      | otherwise              = Nothing
      where
        oMin, oMax :: Int
        oMin = fromEnum (minBound :: OrdClass)
        oMax = fromEnum oClass

compareConstName :: OrdFun -> Name
compareConstName OrdCompare       = compareConstValName
compareConstName OrdLT            = ltConstValName
compareConstName OrdLE            = ltConstValName
compareConstName OrdGT            = ltConstValName
compareConstName OrdGE            = ltConstValName
#if defined(NEW_FUNCTOR_CLASSES)
compareConstName Ord1LiftCompare  = liftCompareConstValName
compareConstName Ord2LiftCompare2 = liftCompare2ConstValName
#else
compareConstName Ord1Compare1     = compare1ConstValName
#endif

ordClassToCompare :: OrdClass -> OrdFun
ordClassToCompare Ord  = OrdCompare
#if defined(NEW_FUNCTOR_CLASSES)
ordClassToCompare Ord1 = Ord1LiftCompare
ordClassToCompare Ord2 = Ord2LiftCompare2
#else
ordClassToCompare Ord1 = Ord1Compare1
#endif

data OrdFun = OrdCompare | OrdLT | OrdLE | OrdGE | OrdGT
#if defined(NEW_FUNCTOR_CLASSES)
            | Ord1LiftCompare | Ord2LiftCompare2
#else
            | Ord1Compare1
#endif
  deriving Eq

ordFunName :: OrdFun -> Int -> Name
ordFunName OrdCompare       0 = compareValName
ordFunName OrdLT            0 = ltValName
ordFunName OrdLE            0 = leValName
ordFunName OrdGE            0 = geValName
ordFunName OrdGT            0 = gtValName
#if defined(NEW_FUNCTOR_CLASSES)
ordFunName Ord1LiftCompare  0 = ordFunName OrdCompare 0
ordFunName Ord1LiftCompare  1 = liftCompareValName
ordFunName Ord2LiftCompare2 0 = ordFunName OrdCompare 0
ordFunName Ord2LiftCompare2 1 = ordFunName Ord1LiftCompare 1
ordFunName Ord2LiftCompare2 2 = liftCompare2ValName
#else
ordFunName Ord1Compare1     0 = ordFunName OrdCompare 0
ordFunName Ord1Compare1     1 = compare1ValName
#endif
ordFunName _                _ = error "Data.Ord.Deriving.Internal.ordFunName"

ordFunToClass :: OrdFun -> OrdClass
ordFunToClass OrdCompare       = Ord
ordFunToClass OrdLT            = Ord
ordFunToClass OrdLE            = Ord
ordFunToClass OrdGE            = Ord
ordFunToClass OrdGT            = Ord
#if defined(NEW_FUNCTOR_CLASSES)
ordFunToClass Ord1LiftCompare  = Ord1
ordFunToClass Ord2LiftCompare2 = Ord2
#else
ordFunToClass Ord1Compare1     = Ord1
#endif

eqResult :: OrdFun -> Q Exp
eqResult OrdCompare       = eqTagExpr
eqResult OrdLT            = falseExpr
eqResult OrdLE            = trueExpr
eqResult OrdGE            = trueExpr
eqResult OrdGT            = falseExpr
#if defined(NEW_FUNCTOR_CLASSES)
eqResult Ord1LiftCompare  = eqTagExpr
eqResult Ord2LiftCompare2 = eqTagExpr
#else
eqResult Ord1Compare1     = eqTagExpr
#endif

gtResult :: OrdFun -> Q Exp
gtResult OrdCompare       = gtTagExpr
gtResult OrdLT            = falseExpr
gtResult OrdLE            = falseExpr
gtResult OrdGE            = trueExpr
gtResult OrdGT            = trueExpr
#if defined(NEW_FUNCTOR_CLASSES)
gtResult Ord1LiftCompare  = gtTagExpr
gtResult Ord2LiftCompare2 = gtTagExpr
#else
gtResult Ord1Compare1     = gtTagExpr
#endif

ltResult :: OrdFun -> Q Exp
ltResult OrdCompare       = ltTagExpr
ltResult OrdLT            = trueExpr
ltResult OrdLE            = trueExpr
ltResult OrdGE            = falseExpr
ltResult OrdGT            = falseExpr
#if defined(NEW_FUNCTOR_CLASSES)
ltResult Ord1LiftCompare  = ltTagExpr
ltResult Ord2LiftCompare2 = ltTagExpr
#else
ltResult Ord1Compare1     = ltTagExpr
#endif

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

ltTagExpr, eqTagExpr, gtTagExpr, falseExpr, trueExpr :: Q Exp
ltTagExpr = conE ltDataName
eqTagExpr = conE eqDataName
gtTagExpr = conE gtDataName
falseExpr = conE falseDataName
trueExpr  = conE trueDataName

-- Besides compare, that is
otherFuns :: OrdClass -> [ConstructorInfo] -> [OrdFun]
otherFuns _ [] = [] -- We only need compare for empty data types.
otherFuns oClass cons = case oClass of
    Ord1 -> []
#if defined(NEW_FUNCTOR_CLASSES)
    Ord2 -> []
#endif
    Ord | (lastTag - firstTag) <= 2 || null nonNullaryCons
       -> [OrdLT, OrdLE, OrdGE, OrdGT]
        | otherwise
       -> []
  where
    firstTag, lastTag :: Int
    firstTag = 0
    lastTag  = length cons - 1

    nonNullaryCons :: [ConstructorInfo]
    nonNullaryCons = filterOut isNullaryCon cons

unliftedOrdFun :: Name -> OrdFun -> Name -> Name -> Q Exp
unliftedOrdFun tyName oFun a b = case oFun of
    OrdCompare       -> unliftedCompareExpr
    OrdLT            -> wrap ltFun
    OrdLE            -> wrap leFun
    OrdGE            -> wrap geFun
    OrdGT            -> wrap gtFun
#if defined(NEW_FUNCTOR_CLASSES)
    Ord1LiftCompare  -> unliftedCompareExpr
    Ord2LiftCompare2 -> unliftedCompareExpr
#else
    Ord1Compare1     -> unliftedCompareExpr
#endif
  where
    unliftedCompareExpr :: Q Exp
    unliftedCompareExpr = unliftedCompare ltFun eqFun aExpr bExpr
                                          ltTagExpr eqTagExpr gtTagExpr

    ltFun, leFun, eqFun, geFun, gtFun :: Name
    (ltFun, leFun, eqFun, geFun, gtFun) = primOrdFuns tyName

    wrap :: Name -> Q Exp
    wrap primFun = primOpAppExpr aExpr primFun bExpr

    aExpr, bExpr :: Q Exp
    aExpr = varE a
    bExpr = varE b

unliftedCompare :: Name -> Name
                -> Q Exp -> Q Exp          -- What to compare
                -> Q Exp -> Q Exp -> Q Exp -- Three results
                -> Q Exp
unliftedCompare ltFun eqFun aExpr bExpr lt eq gt =
    condE (ascribeBool $ primOpAppExpr aExpr ltFun bExpr) lt $
        condE (ascribeBool $ primOpAppExpr aExpr eqFun bExpr) eq gt
  where
    ascribeBool :: Q Exp -> Q Exp
    ascribeBool e = sigE e $ conT boolTypeName

primOrdFuns :: Name -> (Name, Name, Name, Name, Name)
primOrdFuns tyName =
  case Map.lookup tyName primOrdFunTbl of
    Just names -> names
    Nothing    -> error $ nameBase tyName ++ " is not supported."

isSupportedUnliftedType :: Type -> Bool
isSupportedUnliftedType (ConT tyName) = Map.member tyName primOrdFunTbl
isSupportedUnliftedType _             = False

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

-- | Like 'filter', only it reverses the sense of the test
filterOut :: (a -> Bool) -> [a] -> [a]
filterOut _ [] = []
filterOut p (x:xs) | p x       = filterOut p xs
                   | otherwise = x : filterOut p xs
