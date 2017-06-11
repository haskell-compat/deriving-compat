{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-|
Module:      Data.Functor.Deriving.Internal
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

The machinery needed to derive 'Foldable', 'Functor', and 'Traversable' instances.

For more info on how deriving @Functor@ works, see
<https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor this GHC wiki page>.
-}
module Data.Functor.Deriving.Internal (
      -- * 'Foldable'
      deriveFoldable
    , makeFoldMap
    , makeFoldr
    , makeFold
    , makeFoldl
      -- * 'Functor'
    , deriveFunctor
    , makeFmap
      -- * 'Traversable'
    , deriveTraversable
    , makeTraverse
    , makeSequenceA
    , makeMapM
    , makeSequence
    ) where

import           Control.Monad (guard, zipWithM)

import           Data.Deriving.Internal
import           Data.Either (rights)
import           Data.List
import qualified Data.Map as Map (keys, lookup, singleton)
import           Data.Maybe

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

-- | Generates a 'Foldable' instance declaration for the given data type or data
-- family instance.
deriveFoldable :: Name -> Q [Dec]
deriveFoldable = deriveFunctorClass Foldable

-- | Generates a lambda expression which behaves like 'foldMap' (without requiring a
-- 'Foldable' instance).
makeFoldMap :: Name -> Q Exp
makeFoldMap = makeFunctorFun FoldMap

-- | Generates a lambda expression which behaves like 'foldr' (without requiring a
-- 'Foldable' instance).
makeFoldr :: Name -> Q Exp
makeFoldr = makeFunctorFun Foldr

-- | Generates a lambda expression which behaves like 'fold' (without requiring a
-- 'Foldable' instance).
makeFold :: Name -> Q Exp
makeFold name = makeFoldMap name `appE` varE idValName

-- | Generates a lambda expression which behaves like 'foldl' (without requiring a
-- 'Foldable' instance).
makeFoldl :: Name -> Q Exp
makeFoldl name = do
  f <- newName "f"
  z <- newName "z"
  t <- newName "t"
  lamE [varP f, varP z, varP t] $
    appsE [ varE appEndoValName
          , appsE [ varE getDualValName
                  , appsE [ makeFoldMap name, foldFun f, varE t]
                  ]
          , varE z
          ]
  where
    foldFun :: Name -> Q Exp
    foldFun n = infixApp (conE dualDataName)
                         (varE composeValName)
                         (infixApp (conE endoDataName)
                                   (varE composeValName)
                                   (varE flipValName `appE` varE n)
                         )

-- | Generates a 'Functor' instance declaration for the given data type or data
-- family instance.
deriveFunctor :: Name -> Q [Dec]
deriveFunctor = deriveFunctorClass Functor

-- | Generates a lambda expression which behaves like 'fmap' (without requiring a
-- 'Functor' instance).
makeFmap :: Name -> Q Exp
makeFmap = makeFunctorFun Fmap

-- | Generates a 'Traversable' instance declaration for the given data type or data
-- family instance.
deriveTraversable :: Name -> Q [Dec]
deriveTraversable = deriveFunctorClass Traversable

-- | Generates a lambda expression which behaves like 'traverse' (without requiring a
-- 'Traversable' instance).
makeTraverse :: Name -> Q Exp
makeTraverse = makeFunctorFun Traverse

-- | Generates a lambda expression which behaves like 'sequenceA' (without requiring a
-- 'Traversable' instance).
makeSequenceA :: Name -> Q Exp
makeSequenceA name = makeTraverse name `appE` varE idValName

-- | Generates a lambda expression which behaves like 'mapM' (without requiring a
-- 'Traversable' instance).
makeMapM :: Name -> Q Exp
makeMapM name = do
  f <- newName "f"
  lam1E (varP f) . infixApp (varE unwrapMonadValName) (varE composeValName) $
                   makeTraverse name `appE` wrapMonadExp f
  where
    wrapMonadExp :: Name -> Q Exp
    wrapMonadExp n = infixApp (conE wrapMonadDataName) (varE composeValName) (varE n)

-- | Generates a lambda expression which behaves like 'sequence' (without requiring a
-- 'Traversable' instance).
makeSequence :: Name -> Q Exp
makeSequence name = makeMapM name `appE` varE idValName

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive a class instance declaration (depending on the FunctorClass argument's value).
deriveFunctorClass :: FunctorClass -> Name -> Q [Dec]
deriveFunctorClass fc name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext = ctxt
                 , datatypeName    = parentName
                 , datatypeVars    = vars
                 , datatypeVariant = variant
                 , datatypeCons    = cons
                 } -> do
      (instanceCxt, instanceType)
          <- buildTypeInstance fc parentName ctxt vars variant
      (:[]) `fmap` instanceD (return instanceCxt)
                             (return instanceType)
                             (functorFunDecs fc vars cons)

-- | Generates a declaration defining the primary function(s) corresponding to a
-- particular class (fmap for Functor, foldr and foldMap for Foldable, and
-- traverse for Traversable).
--
-- For why both foldr and foldMap are derived for Foldable, see Trac #7436.
functorFunDecs :: FunctorClass -> [Type] -> [ConstructorInfo] -> [Q Dec]
functorFunDecs fc vars cons = map makeFunD $ functorClassToFuns fc where
  makeFunD :: FunctorFun -> Q Dec
  makeFunD ff =
    funD (functorFunName ff)
         [ clause []
                  (normalB $ makeFunctorFunForCons ff vars cons)
                  []
         ]

-- | Generates a lambda expression which behaves like the FunctorFun argument.
makeFunctorFun :: FunctorFun -> Name -> Q Exp
makeFunctorFun ff name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext = ctxt
                 , datatypeName    = parentName
                 , datatypeVars    = vars
                 , datatypeVariant = variant
                 , datatypeCons    = cons
                 } -> do
      -- We force buildTypeInstance here since it performs some checks for whether
      -- or not the provided datatype can actually have fmap/foldr/traverse/etc.
      -- implemented for it, and produces errors if it can't.
      buildTypeInstance (functorFunToClass ff) parentName ctxt vars variant
        `seq` makeFunctorFunForCons ff vars cons

-- | Generates a lambda expression for the given constructors.
-- All constructors must be from the same type.
makeFunctorFunForCons :: FunctorFun -> [Type] -> [ConstructorInfo] -> Q Exp
makeFunctorFunForCons ff vars cons = do
  argNames <- mapM newName $ catMaybes [ Just "f"
                                       , guard (ff == Foldr) >> Just "z"
                                       , Just "value"
                                       ]
  let mapFun:others = argNames
      z         = head others -- If we're deriving foldr, this will be well defined
                              -- and useful. Otherwise, it'll be ignored.
      value     = last others
      lastTyVar = varTToName $ last vars
      tvMap     = Map.singleton lastTyVar $ OneName mapFun
  lamE (map varP argNames)
      . appsE
      $ [ varE $ functorFunConstName ff
        , if null cons
             then appE (varE errorValName)
                       (stringE $ "Void " ++ nameBase (functorFunName ff))
             else caseE (varE value)
                        (map (makeFunctorFunForCon ff z tvMap) cons)
        ] ++ map varE argNames

-- | Generates a lambda expression for a single constructor.
makeFunctorFunForCon :: FunctorFun -> Name -> TyVarMap1 -> ConstructorInfo -> Q Match
makeFunctorFunForCon ff z tvMap
  (ConstructorInfo { constructorName    = conName
                   , constructorContext = ctxt
                   , constructorFields  = ts }) = do
    ts'      <- mapM resolveTypeSynonyms ts
    argNames <- newNameList "_arg" $ length ts'
    checkExistentialContext (functorFunToClass ff) tvMap ctxt conName $
      makeFunctorFunForArgs ff z tvMap conName ts' argNames

-- | Generates a lambda expression for a single constructor's arguments.
makeFunctorFunForArgs :: FunctorFun
                      -> Name
                      -> TyVarMap1
                      -> Name
                      -> [Type]
                      -> [Name]
                      -> Q Match
makeFunctorFunForArgs ff z tvMap conName tys args =
  match (conP conName $ map varP args)
        (normalB $ functorFunCombine ff conName z args mappedArgs)
        []
  where
    mappedArgs :: Q [Either Exp Exp]
    mappedArgs = zipWithM (makeFunctorFunForArg ff tvMap conName) tys args

-- | Generates a lambda expression for a single argument of a constructor.
--  The returned value is 'Right' if its type mentions the last type
-- parameter. Otherwise, it is 'Left'.
makeFunctorFunForArg :: FunctorFun
                     -> TyVarMap1
                     -> Name
                     -> Type
                     -> Name
                     -> Q (Either Exp Exp)
makeFunctorFunForArg ff tvMap conName ty tyExpName =
  makeFunctorFunForType ff tvMap conName True ty `appEitherE` varE tyExpName

-- | Generates a lambda expression for a specific type. The returned value is
-- 'Right' if its type mentions the last type parameter. Otherwise,
-- it is 'Left'.
makeFunctorFunForType :: FunctorFun
                      -> TyVarMap1
                      -> Name
                      -> Bool
                      -> Type
                      -> Q (Either Exp Exp)
makeFunctorFunForType ff tvMap conName covariant (VarT tyName) =
  case Map.lookup tyName tvMap of
    Just (OneName mapName) ->
      fmap Right $ if covariant
                      then varE mapName
                      else contravarianceError conName
    -- Invariant: this should only happen when deriving fmap
    Nothing -> fmap Left $ functorFunTriv ff
makeFunctorFunForType ff tvMap conName covariant (SigT ty _) =
  makeFunctorFunForType ff tvMap conName covariant ty
makeFunctorFunForType ff tvMap conName covariant (ForallT _ _ ty) =
  makeFunctorFunForType ff tvMap conName covariant ty
makeFunctorFunForType ff tvMap conName covariant ty =
  let tyCon  :: Type
      tyArgs :: [Type]
      tyCon:tyArgs = unapplyTy ty

      numLastArgs :: Int
      numLastArgs = min 1 $ length tyArgs

      lhsArgs, rhsArgs :: [Type]
      (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

      tyVarNames :: [Name]
      tyVarNames = Map.keys tvMap

      mentionsTyArgs :: Bool
      mentionsTyArgs = any (`mentionsName` tyVarNames) tyArgs

      makeFunctorFunTuple :: ([Q Pat] -> Q Pat) -> (Int -> Name) -> Int
                          -> Q (Either Exp Exp)
      makeFunctorFunTuple mkTupP mkTupleDataName n = do
         args <- mapM newName $ catMaybes [ Just "x"
                                          , guard (ff == Foldr) >> Just "z"
                                          ]
         xs <- newNameList "_tup" n

         let x = head args
             z = last args
         fmap Right $ lamE (map varP args) $ caseE (varE x)
              [ match (mkTupP $ map varP xs)
                      (normalB $ functorFunCombine ff
                                                   (mkTupleDataName n)
                                                   z
                                                   xs
                                                   (zipWithM makeFunctorFunTupleField tyArgs xs)
                      )
                      []
              ]

      makeFunctorFunTupleField :: Type -> Name -> Q (Either Exp Exp)
      makeFunctorFunTupleField fieldTy fieldName =
        makeFunctorFunForType ff tvMap conName covariant fieldTy
          `appEitherE` varE fieldName

      fc :: FunctorClass
      fc = functorFunToClass ff

   in case tyCon of
     ArrowT
       | not (allowFunTys fc) -> noFunctionsError conName
       | mentionsTyArgs, [argTy, resTy] <- tyArgs ->
         do x <- newName "x"
            b <- newName "b"
            fmap Right . lamE [varP x, varP b] $
              covFunctorFun covariant resTy `appE` (varE x `appE`
                (covFunctorFun (not covariant) argTy `appE` varE b))
         where
           covFunctorFun :: Bool -> Type -> Q Exp
           covFunctorFun cov = fmap fromEither . makeFunctorFunForType ff tvMap conName cov
#if MIN_VERSION_template_haskell(2,6,0)
     UnboxedTupleT n
       | n > 0 && mentionsTyArgs -> makeFunctorFunTuple unboxedTupP unboxedTupleDataName n
#endif
     TupleT n
       | n > 0 && mentionsTyArgs -> makeFunctorFunTuple tupP tupleDataName n
     _ -> do
         itf <- isTyFamily tyCon
         if any (`mentionsName` tyVarNames) lhsArgs || (itf && mentionsTyArgs)
           then outOfPlaceTyVarError fc conName
           else if any (`mentionsName` tyVarNames) rhsArgs
                  then fmap Right . functorFunApp ff . appsE $
                         ( varE (functorFunName ff)
                         : map (fmap fromEither . makeFunctorFunForType ff tvMap conName covariant)
                                rhsArgs
                         )
                  else fmap Left $ functorFunTriv ff

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which class is being derived.
data FunctorClass = Functor | Foldable | Traversable

instance ClassRep FunctorClass where
    arity _ = 1

    allowExQuant Foldable = True
    allowExQuant _        = False

    fullClassName Functor     = functorTypeName
    fullClassName Foldable    = foldableTypeName
    fullClassName Traversable = traversableTypeName

    classConstraint fClass 1 = Just $ fullClassName fClass
    classConstraint  _      _ = Nothing

-- | A representation of which function is being generated.
data FunctorFun = Fmap | Foldr | FoldMap | Traverse
  deriving Eq

instance Show FunctorFun where
    showsPrec _ Fmap     = showString "fmap"
    showsPrec _ Foldr    = showString "foldr"
    showsPrec _ FoldMap  = showString "foldMap"
    showsPrec _ Traverse = showString "traverse"

functorFunConstName :: FunctorFun -> Name
functorFunConstName Fmap     = fmapConstValName
functorFunConstName Foldr    = foldrConstValName
functorFunConstName FoldMap  = foldMapConstValName
functorFunConstName Traverse = traverseConstValName

functorFunName :: FunctorFun -> Name
functorFunName Fmap     = fmapValName
functorFunName Foldr    = foldrValName
functorFunName FoldMap  = foldMapValName
functorFunName Traverse = traverseValName

functorClassToFuns :: FunctorClass -> [FunctorFun]
functorClassToFuns Functor     = [Fmap]
functorClassToFuns Foldable    = [Foldr, FoldMap]
functorClassToFuns Traversable = [Traverse]

functorFunToClass :: FunctorFun -> FunctorClass
functorFunToClass Fmap     = Functor
functorFunToClass Foldr    = Foldable
functorFunToClass FoldMap  = Foldable
functorFunToClass Traverse = Traversable

allowFunTys :: FunctorClass -> Bool
allowFunTys Functor = True
allowFunTys _       = False

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

-- See Trac #7436 for why explicit lambdas are used
functorFunTriv :: FunctorFun -> Q Exp
functorFunTriv Fmap = do
  x <- newName "x"
  lam1E (varP x) $ varE x
-- We filter out trivial expressions from derived foldr, foldMap, and traverse
-- implementations, so if we attempt to call functorFunTriv on one of those
-- methods, we've done something wrong.
functorFunTriv ff = return . error $ "functorFunTriv: " ++ show ff

functorFunApp :: FunctorFun -> Q Exp -> Q Exp
functorFunApp Foldr e = do
  x <- newName "x"
  z <- newName "z"
  lamE [varP x, varP z] $ appsE [e, varE z, varE x]
functorFunApp _ e = e

functorFunCombine :: FunctorFun
                  -> Name
                  -> Name
                  -> [Name]
                  -> Q [Either Exp Exp]
                  -> Q Exp
functorFunCombine Fmap     = fmapCombine
functorFunCombine Foldr    = foldrCombine
functorFunCombine FoldMap  = foldMapCombine
functorFunCombine Traverse = traverseCombine

fmapCombine :: Name
            -> Name
            -> [Name]
            -> Q [Either Exp Exp]
            -> Q Exp
fmapCombine conName _ _ = fmap (foldl' AppE (ConE conName) . fmap fromEither)

-- foldr, foldMap, and traverse are handled differently from fmap, since
-- they filter out subexpressions whose types do not mention the last
-- type parameter. See
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor#AlternativestrategyforderivingFoldableandTraversable
-- for further discussion.

foldrCombine :: Name
             -> Name
             -> [Name]
             -> Q [Either Exp Exp]
             -> Q Exp
foldrCombine _ zName _ = fmap (foldr AppE (VarE zName) . rights)

foldMapCombine :: Name
               -> Name
               -> [Name]
               -> Q [Either Exp Exp]
               -> Q Exp
foldMapCombine _ _ _ = fmap (go . rights)
  where
    go :: [Exp] -> Exp
    go [] = VarE memptyValName
    go es = foldr1 (AppE . AppE (VarE mappendValName)) es

traverseCombine :: Name
                -> Name
                -> [Name]
                -> Q [Either Exp Exp]
                -> Q Exp
traverseCombine conName _ args essQ = do
    ess <- essQ

    let argTysTyVarInfo :: [Bool]
        argTysTyVarInfo = map isRight ess

        argsWithTyVar, argsWithoutTyVar :: [Name]
        (argsWithTyVar, argsWithoutTyVar) = partitionByList argTysTyVarInfo args

        conExpQ :: Q Exp
        conExpQ
          | null argsWithTyVar
          = appsE (conE conName:map varE argsWithoutTyVar)
          | otherwise = do
              bs <- newNameList "b" $ length args
              let bs'  = filterByList  argTysTyVarInfo bs
                  vars = filterByLists argTysTyVarInfo
                                       (map varE bs) (map varE args)
              lamE (map varP bs') (appsE (conE conName:vars))

    conExp <- conExpQ

    let go :: [Exp] -> Exp
        go []     = VarE pureValName `AppE` conExp
        go [e] = VarE fmapValName `AppE` conExp `AppE` e
        go (e1:e2:es) = foldl' (\se1 se2 -> InfixE (Just se1) (VarE apValName) (Just se2))
          (VarE liftA2ValName `AppE` conExp `AppE` e1 `AppE` e2) es

    return . go . rights $ ess
