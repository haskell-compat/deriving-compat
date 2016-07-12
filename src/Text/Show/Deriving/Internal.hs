{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-|
Module:      Text.Show.Deriving.Internal
Copyright:   (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Show', 'Show1', and 'Show2' instances.
-}
module Text.Show.Deriving.Internal (
      -- * 'Show'
      deriveShow
    , deriveShowOptions
    , makeShowsPrec
    , makeShowsPrecOptions
    , makeShow
    , makeShowOptions
    , makeShowList
    , makeShowListOptions
      -- * 'Show1'
    , deriveShow1
    , deriveShow1Options
#if defined(NEW_FUNCTOR_CLASSES)
    , makeLiftShowsPrec
    , makeLiftShowsPrecOptions
    , makeLiftShowList
    , makeLiftShowListOptions
#endif
    , makeShowsPrec1
    , makeShowsPrec1Options
#if defined(NEW_FUNCTOR_CLASSES)
      -- * 'Show2'
    , deriveShow2
    , deriveShow2Options
    , makeLiftShowsPrec2
    , makeLiftShowsPrec2Options
    , makeLiftShowList2
    , makeLiftShowList2Options
    , makeShowsPrec2
    , makeShowsPrec2Options
#endif
      -- * 'ShowOptions'
    , ShowOptions(..)
    , defaultShowOptions
    , legacyShowOptions
    ) where

#if MIN_VERSION_template_haskell(2,11,0)
import           Control.Monad ((<=<))
import           Data.Maybe (fromMaybe, isJust)
#endif

import           Data.Deriving.Internal
import           Data.List
import qualified Data.Map as Map

#if __GLASGOW_HASKELL__ >= 800
import           GHC.Lexeme (startsConSym)
#endif
import           GHC.Show (appPrec, appPrec1)

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

-- | Options that further configure how the functions in "Text.Show.Deriving"
-- should behave.
newtype ShowOptions = ShowOptions
  { ghc8ShowBehavior :: Bool
    -- ^ If 'True', the derived 'Show', 'Show1', or 'Show2' instance will not
    --   surround the output of showing fields of unlifted types with parentheses,
    --   and the output will be suffixed with hash signs (@#@).
  } deriving (Eq, Ord, Read, Show)

-- | 'ShowOptions' that match the behavior of the most recent GHC release.
defaultShowOptions :: ShowOptions
defaultShowOptions = ShowOptions { ghc8ShowBehavior = True }

-- | 'ShowOptions' that match the behavior of the installed version of GHC.
legacyShowOptions :: ShowOptions
legacyShowOptions = ShowOptions
  { ghc8ShowBehavior =
#if __GLASGOW_HASKELL__ >= 711
                       True
#else
                       False
#endif
  }

-- | Generates a 'Show' instance declaration for the given data type or data
-- family instance.
deriveShow :: Name -> Q [Dec]
deriveShow = deriveShowOptions defaultShowOptions

-- | Like 'deriveShow', but takes a 'ShowOptions' argument.
deriveShowOptions :: ShowOptions -> Name -> Q [Dec]
deriveShowOptions = deriveShowClass Show

-- | Generates a lambda expression which behaves like 'show' (without
-- requiring a 'Show' instance).
makeShow :: Name -> Q Exp
makeShow = makeShowOptions defaultShowOptions

-- | Like 'makeShow', but takes a 'ShowOptions' argument.
makeShowOptions :: ShowOptions -> Name -> Q Exp
makeShowOptions opts name = do
    x <- newName "x"
    lam1E (varP x) $ makeShowsPrecOptions opts name
                     `appE` integerE 0
                     `appE` varE x
                     `appE` stringE ""

-- | Generates a lambda expression which behaves like 'showsPrec' (without
-- requiring a 'Show' instance).
makeShowsPrec :: Name -> Q Exp
makeShowsPrec = makeShowsPrecOptions defaultShowOptions

-- | Like 'makeShowsPrec', but takes a 'ShowOptions' argument.
makeShowsPrecOptions :: ShowOptions -> Name -> Q Exp
makeShowsPrecOptions = makeShowsPrecClass Show

-- | Generates a lambda expression which behaves like 'showList' (without
-- requiring a 'Show' instance).
makeShowList :: Name -> Q Exp
makeShowList = makeShowListOptions defaultShowOptions

-- | Like 'makeShowList', but takes a 'ShowOptions' argument.
makeShowListOptions :: ShowOptions -> Name -> Q Exp
makeShowListOptions opts name =
    varE showListWithValName `appE` (makeShowsPrecOptions opts name `appE` integerE 0)

-- | Generates a 'Show1' instance declaration for the given data type or data
-- family instance.
deriveShow1 :: Name -> Q [Dec]
deriveShow1 = deriveShow1Options defaultShowOptions

-- | Like 'deriveShow1', but takes a 'ShowOptions' argument.
deriveShow1Options :: ShowOptions -> Name -> Q [Dec]
deriveShow1Options = deriveShowClass Show1

-- | Generates a lambda expression which behaves like 'showsPrec1' (without
-- requiring a 'Show1' instance).
makeShowsPrec1 :: Name -> Q Exp
makeShowsPrec1 = makeShowsPrec1Options defaultShowOptions

#if defined(NEW_FUNCTOR_CLASSES)
-- | Generates a lambda expression which behaves like 'liftShowsPrec' (without
-- requiring a 'Show1' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftShowsPrec :: Name -> Q Exp
makeLiftShowsPrec = makeLiftShowsPrecOptions defaultShowOptions

-- | Like 'makeLiftShowsPrec', but takes a 'ShowOptions' argument.
--
-- This function is not available with @transformers-0.4@.
makeLiftShowsPrecOptions :: ShowOptions -> Name -> Q Exp
makeLiftShowsPrecOptions = makeShowsPrecClass Show1

-- | Generates a lambda expression which behaves like 'liftShowList' (without
-- requiring a 'Show' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftShowList :: Name -> Q Exp
makeLiftShowList = makeLiftShowListOptions defaultShowOptions

-- | Like 'makeLiftShowList', but takes a 'ShowOptions' argument.
--
-- This function is not available with @transformers-0.4@.
makeLiftShowListOptions :: ShowOptions -> Name -> Q Exp
makeLiftShowListOptions opts name = do
    sp' <- newName "sp'"
    sl' <- newName "sl'"
    lamE [varP sp', varP sl'] $ varE showListWithValName `appE`
        (makeLiftShowsPrecOptions opts name `appE` varE sp' `appE` varE sl'
                                            `appE` integerE 0)

-- | Like 'makeShowsPrec1', but takes a 'ShowOptions' argument.
makeShowsPrec1Options :: ShowOptions -> Name -> Q Exp
makeShowsPrec1Options opts name = makeLiftShowsPrecOptions opts name
                           `appE` varE showsPrecValName
                           `appE` varE showListValName
#else
-- | Like 'makeShowsPrec1', but takes a 'ShowOptions' argument.
makeShowsPrec1Options :: ShowOptions -> Name -> Q Exp
makeShowsPrec1Options = makeShowsPrecClass Show1
#endif

#if defined(NEW_FUNCTOR_CLASSES)
-- | Generates a 'Show2' instance declaration for the given data type or data
-- family instance.
--
-- This function is not available with @transformers-0.4@.
deriveShow2 :: Name -> Q [Dec]
deriveShow2 = deriveShow2Options defaultShowOptions

-- | Like 'deriveShow2', but takes a 'ShowOptions' argument.
--
-- This function is not available with @transformers-0.4@.
deriveShow2Options :: ShowOptions -> Name -> Q [Dec]
deriveShow2Options = deriveShowClass Show2

-- | Generates a lambda expression which behaves like 'liftShowsPrec2' (without
-- requiring a 'Show2' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftShowsPrec2 :: Name -> Q Exp
makeLiftShowsPrec2 = makeLiftShowsPrec2Options defaultShowOptions

-- | Like 'makeLiftShowsPrec2', but takes a 'ShowOptions' argument.
--
-- This function is not available with @transformers-0.4@.
makeLiftShowsPrec2Options :: ShowOptions -> Name -> Q Exp
makeLiftShowsPrec2Options = makeShowsPrecClass Show2

-- | Generates a lambda expression which behaves like 'liftShowList2' (without
-- requiring a 'Show' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftShowList2 :: Name -> Q Exp
makeLiftShowList2 = makeLiftShowList2Options defaultShowOptions

-- | Like 'makeLiftShowList2', but takes a 'ShowOptions' argument.
--
-- This function is not available with @transformers-0.4@.
makeLiftShowList2Options :: ShowOptions -> Name -> Q Exp
makeLiftShowList2Options opts name = do
    sp1' <- newName "sp1'"
    sl1' <- newName "sl1'"
    sp2' <- newName "sp2'"
    sl2' <- newName "sl2'"
    lamE [varP sp1', varP sl1', varP sp2', varP sl2'] $
        varE showListWithValName `appE`
            (makeLiftShowsPrec2Options opts name `appE` varE sp1' `appE` varE sl1'
                                                 `appE` varE sp2' `appE` varE sl2'
                                                 `appE` integerE 0)

-- | Generates a lambda expression which behaves like 'showsPrec2' (without
-- requiring a 'Show2' instance).
--
-- This function is not available with @transformers-0.4@.
makeShowsPrec2 :: Name -> Q Exp
makeShowsPrec2 = makeShowsPrec2Options defaultShowOptions

-- | Like 'makeShowsPrec2', but takes a 'ShowOptions' argument.
--
-- This function is not available with @transformers-0.4@.
makeShowsPrec2Options :: ShowOptions -> Name -> Q Exp
makeShowsPrec2Options opts name = makeLiftShowsPrec2Options opts name
                           `appE` varE showsPrecValName
                           `appE` varE showListValName
                           `appE` varE showsPrecValName
                           `appE` varE showListValName
#endif

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive a Show(1)(2) instance declaration (depending on the ShowClass
-- argument's value).
deriveShowClass :: ShowClass -> ShowOptions -> Name -> Q [Dec]
deriveShowClass sClass opts name = withType name fromCons
  where
    fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q [Dec]
    fromCons name' ctxt tvbs cons mbTys = (:[]) `fmap` do
        (instanceCxt, instanceType)
            <- buildTypeInstance sClass name' ctxt tvbs mbTys
        instanceD (return instanceCxt)
                  (return instanceType)
                  (showsPrecDecs sClass opts cons)

-- | Generates a declaration defining the primary function corresponding to a
-- particular class (showsPrec for Show, liftShowsPrec for Show1, and
-- liftShowsPrec2 for Show2).
showsPrecDecs :: ShowClass -> ShowOptions -> [Con] -> [Q Dec]
showsPrecDecs sClass opts cons =
    [ funD (showsPrecName sClass)
           [ clause []
                    (normalB $ makeShowForCons sClass opts cons)
                    []
           ]
    ]

-- | Generates a lambda expression which behaves like showsPrec (for Show),
-- liftShowsPrec (for Show1), or liftShowsPrec2 (for Show2).
makeShowsPrecClass :: ShowClass -> ShowOptions -> Name -> Q Exp
makeShowsPrecClass sClass opts name = withType name fromCons
  where
    fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q Exp
    fromCons name' ctxt tvbs cons mbTys =
        -- We force buildTypeInstance here since it performs some checks for whether
        -- or not the provided datatype can actually have showsPrec/liftShowsPrec/etc.
        -- implemented for it, and produces errors if it can't.
        buildTypeInstance sClass name' ctxt tvbs mbTys
          `seq` makeShowForCons sClass opts cons

-- | Generates a lambda expression for showsPrec/liftShowsPrec/etc. for the
-- given constructors. All constructors must be from the same type.
makeShowForCons :: ShowClass -> ShowOptions -> [Con] -> Q Exp
makeShowForCons _ _ [] = noConstructorsError
makeShowForCons sClass opts cons = do
    p     <- newName "p"
    value <- newName "value"
    sps   <- newNameList "sp" $ arity sClass
    sls   <- newNameList "sl" $ arity sClass
    let spls       = zip sps sls
        _spsAndSls = interleave sps sls
    matches <- concatMapM (makeShowForCon p sClass opts spls) cons
    lamE (map varP $
#if defined(NEW_FUNCTOR_CLASSES)
                     _spsAndSls ++
#endif
                     [p, value])
        . appsE
        $ [ varE $ showsPrecConstName sClass
          , caseE (varE value) (map return matches)
          ]
#if defined(NEW_FUNCTOR_CLASSES)
            ++ map varE _spsAndSls
#endif
            ++ [varE p, varE value]

-- | Generates a lambda expression for showsPrec/liftShowsPrec/etc. for a
-- single constructor.
makeShowForCon :: Name
               -> ShowClass
               -> ShowOptions
               -> [(Name, Name)]
               -> Con
               -> Q [Match]
makeShowForCon _ sClass _ spls (NormalC conName []) = do
    ([], _) <- reifyConTys2 sClass spls conName
    m <- match
           (conP conName [])
           (normalB $ varE showStringValName `appE` stringE (parenInfixConName conName ""))
           []
    return [m]
makeShowForCon p sClass opts spls (NormalC conName [_]) = do
    ([argTy], tvMap) <- reifyConTys2 sClass spls conName
    arg <- newName "arg"

    let showArg  = makeShowForArg appPrec1 sClass opts conName tvMap argTy arg
        namedArg = infixApp (varE showStringValName `appE` stringE (parenInfixConName conName " "))
                            (varE composeValName)
                            showArg

    m <- match
           (conP conName [varP arg])
           (normalB $ varE showParenValName
                       `appE` infixApp (varE p) (varE ltValName) (integerE appPrec)
                       `appE` namedArg)
           []
    return [m]
makeShowForCon p sClass opts spls (NormalC conName _) = do
    (argTys, tvMap) <- reifyConTys2 sClass spls conName
    args <- newNameList "arg" $ length argTys

    m <- if isNonUnitTuple conName
         then do
           let showArgs       = zipWith (makeShowForArg 0 sClass opts conName tvMap) argTys args
               parenCommaArgs = (varE showCharValName `appE` charE '(')
                                : intersperse (varE showCharValName `appE` charE ',') showArgs
               mappendArgs    = foldr (`infixApp` varE composeValName)
                                      (varE showCharValName `appE` charE ')')
                                      parenCommaArgs

           match (conP conName $ map varP args)
                 (normalB mappendArgs)
                 []
         else do
           let showArgs    = zipWith (makeShowForArg appPrec1 sClass opts conName tvMap) argTys args
               mappendArgs = foldr1 (\v q -> infixApp v (varE composeValName)
                                                      (infixApp (varE showSpaceValName)
                                                              (varE composeValName)
                                                              q)) showArgs
               namedArgs   = infixApp (varE showStringValName `appE` stringE (parenInfixConName conName " "))
                                      (varE composeValName)
                                      mappendArgs

           match (conP conName $ map varP args)
                 (normalB $ varE showParenValName
                              `appE` infixApp (varE p) (varE ltValName) (integerE appPrec)
                              `appE` namedArgs)
                 []
    return [m]
makeShowForCon p sClass opts spls (RecC conName []) =
    makeShowForCon p sClass opts spls $ NormalC conName []
makeShowForCon p sClass opts spls (RecC conName ts) = do
    (argTys, tvMap) <- reifyConTys2 sClass spls conName
    args <- newNameList "arg" $ length argTys

    let showArgs       = concatMap (\((argName, _, _), argTy, arg)
                                      -> [ varE showStringValName `appE` stringE (nameBase argName ++ " = ")
                                         , makeShowForArg 0 sClass opts conName tvMap argTy arg
                                         , varE showStringValName `appE` stringE ", "
                                         ]
                                   )
                                   (zip3 ts argTys args)
        braceCommaArgs = (varE showCharValName `appE` charE '{') : take (length showArgs - 1) showArgs
        mappendArgs    = foldr (`infixApp` varE composeValName)
                               (varE showCharValName `appE` charE '}')
                               braceCommaArgs
        namedArgs      = infixApp (varE showStringValName `appE` stringE (parenInfixConName conName " "))
                                  (varE composeValName)
                                  mappendArgs

    m <- match
           (conP conName $ map varP args)
           (normalB $ varE showParenValName
                        `appE` infixApp (varE p) (varE ltValName) (integerE appPrec)
                        `appE` namedArgs)
           []
    return [m]
makeShowForCon p sClass opts spls (InfixC _ conName _) = do
    ([alTy, arTy], tvMap) <- reifyConTys2 sClass spls conName
    al   <- newName "argL"
    ar   <- newName "argR"
    info <- reify conName

#if MIN_VERSION_template_haskell(2,11,0)
    conPrec <- case info of
                        DataConI{} -> do
                            fi <- fromMaybe defaultFixity <$> reifyFixity conName
                            case fi of
                                 Fixity prec _ -> return prec
#else
    let conPrec  = case info of
                        DataConI _ _ _ (Fixity prec _) -> prec
#endif
                        _ -> error $ "Text.Show.Deriving.Internal.makeShowForCon: Unsupported type: " ++ show info

    let opName   = nameBase conName
        infixOpE = appE (varE showStringValName) . stringE $
                     if isInfixTypeCon opName
                        then " "  ++ opName ++ " "
                        else " `" ++ opName ++ "` "

    m <- match
           (infixP (varP al) conName (varP ar))
           (normalB $ (varE showParenValName `appE` infixApp (varE p) (varE ltValName) (integerE conPrec))
                        `appE` (infixApp (makeShowForArg (conPrec + 1) sClass opts conName tvMap alTy al)
                                         (varE composeValName)
                                         (infixApp infixOpE
                                                   (varE composeValName)
                                                   (makeShowForArg (conPrec + 1) sClass opts conName tvMap arTy ar)))
           )
           []
    return [m]
makeShowForCon p sClass opts spls (ForallC _ _ con) =
    makeShowForCon p sClass opts spls con
#if MIN_VERSION_template_haskell(2,11,0)
makeShowForCon p sClass opts spls (GadtC conNames ts _) =
    let con :: Name -> Q Con
        con conName = do
            mbFi <- reifyFixity conName
            return $ if startsConSym (head $ nameBase conName)
                        && length ts == 2
                        && isJust mbFi
                      then let [t1, t2] = ts in InfixC t1 conName t2
                      else NormalC conName ts

    in concatMapM (makeShowForCon p sClass opts spls <=< con) conNames
makeShowForCon p sClass opts spls (RecGadtC conNames ts _) =
    concatMapM (makeShowForCon p sClass opts spls . flip RecC ts) conNames
#endif

-- | Generates a lambda expression for showsPrec/liftShowsPrec/etc. for an
-- argument of a constructor.
makeShowForArg :: Int
               -> ShowClass
               -> ShowOptions
               -> Name
               -> TyVarMap2
               -> Type
               -> Name
               -> Q Exp
makeShowForArg p _ opts _ _ (ConT tyName) tyExpName =
    showE
  where
    tyVarE :: Q Exp
    tyVarE = varE tyExpName

    showE :: Q Exp
    showE | tyName == charHashTypeName   = showPrimE cHashDataName oneHashE
          | tyName == doubleHashTypeName = showPrimE dHashDataName twoHashE
          | tyName == floatHashTypeName  = showPrimE fHashDataName oneHashE
          | tyName == intHashTypeName    = showPrimE iHashDataName oneHashE
          | tyName == wordHashTypeName   = showPrimE wHashDataName twoHashE
          | otherwise = varE showsPrecValName `appE` integerE p `appE` tyVarE

    -- Starting with GHC 7.10, data types containing unlifted types with derived Show
    -- instances show hashed literals with actual hash signs, and negative hashed
    -- literals are not surrounded with parentheses.
    showPrimE :: Name -> Q Exp -> Q Exp
    showPrimE con hashE
      | ghc8ShowBehavior opts
      = infixApp (varE showsPrecValName `appE` integerE 0 `appE` (conE con `appE` tyVarE))
                 (varE composeValName)
                 hashE
      | otherwise = varE showsPrecValName `appE` integerE p `appE` (conE con `appE` tyVarE)

    oneHashE, twoHashE :: Q Exp
    oneHashE = varE showCharValName `appE` charE '#'
    twoHashE = varE showStringValName `appE` stringE "##"
makeShowForArg p sClass _ conName tvMap ty tyExpName =
    makeShowForType sClass conName tvMap False ty `appE` integerE p `appE` varE tyExpName

-- | Generates a lambda expression for showsPrec/liftShowsPrec/etc. for a
-- specific type. The generated expression depends on the number of type variables.
--
-- 1. If the type is of kind * (T), apply showsPrec.
-- 2. If the type is of kind * -> * (T a), apply liftShowsPrec $(makeShowForType a)
-- 3. If the type is of kind * -> * -> * (T a b), apply
--    liftShowsPrec2 $(makeShowForType a) $(makeShowForType b)
makeShowForType :: ShowClass
                -> Name
                -> TyVarMap2
                -> Bool -- ^ True if we are using the function of type ([a] -> ShowS),
                        --   False if we are using the function of type (Int -> a -> ShowS).
                -> Type
                -> Q Exp
#if defined(NEW_FUNCTOR_CLASSES)
makeShowForType _ _ tvMap sl (VarT tyName) =
    varE $ case Map.lookup tyName tvMap of
      Just (TwoNames spExp slExp) -> if sl then slExp else spExp
      Nothing -> if sl then showListValName else showsPrecValName
#else
makeShowForType _ _ _ _ VarT{} = varE showsPrecValName
#endif
makeShowForType sClass conName tvMap sl (SigT ty _)      = makeShowForType sClass conName tvMap sl ty
makeShowForType sClass conName tvMap sl (ForallT _ _ ty) = makeShowForType sClass conName tvMap sl ty
#if defined(NEW_FUNCTOR_CLASSES)
makeShowForType sClass conName tvMap sl ty = do
    let tyCon :: Type
        tyArgs :: [Type]
        tyCon:tyArgs = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (arity sClass) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNames :: [Name]
        tyVarNames = Map.keys tvMap

    itf <- isTyFamily tyCon
    if any (`mentionsName` tyVarNames) lhsArgs
          || itf && any (`mentionsName` tyVarNames) tyArgs
       then outOfPlaceTyVarError sClass conName
       else appsE $ [ varE . showsPrecOrListName sl $ toEnum numLastArgs]
                    ++ zipWith (makeShowForType sClass conName tvMap)
                               (cycle [False,True])
                               (interleave rhsArgs rhsArgs)
#else
makeShowForType sClass conName tvMap _ ty = do
  let varNames = Map.keys tvMap

  p'     <- newName "p'"
  value' <- newName "value'"
  case varNames of
    [] -> varE showsPrecValName
    varName:_ ->
      if mentionsName ty varNames
         then lamE [varP p', varP value'] $ varE showsPrec1ValName
                `appE` varE p'
                `appE` (makeFmapApplyNeg sClass conName ty varName `appE` varE value')
         else varE showsPrecValName
#endif

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which @Show@ variant is being derived.
data ShowClass = Show
               | Show1
#if defined(NEW_FUNCTOR_CLASSES)
               | Show2
#endif
  deriving (Bounded, Enum)

instance ClassRep ShowClass where
    arity = fromEnum

    allowExQuant _ = True

    fullClassName Show  = showTypeName
    fullClassName Show1 = show1TypeName
#if defined(NEW_FUNCTOR_CLASSES)
    fullClassName Show2 = show2TypeName
#endif

    classConstraint sClass i
      | sMin <= i && i <= sMax = Just $ fullClassName (toEnum i :: ShowClass)
      | otherwise              = Nothing
      where
        sMin, sMax :: Int
        sMin = fromEnum (minBound :: ShowClass)
        sMax = fromEnum sClass

showsPrecConstName :: ShowClass -> Name
showsPrecConstName Show  = showsPrecConstValName
#if defined(NEW_FUNCTOR_CLASSES)
showsPrecConstName Show1 = liftShowsPrecConstValName
showsPrecConstName Show2 = liftShowsPrec2ConstValName
#else
showsPrecConstName Show1 = showsPrec1ConstValName
#endif

showsPrecName :: ShowClass -> Name
showsPrecName Show  = showsPrecValName
#if defined(NEW_FUNCTOR_CLASSES)
showsPrecName Show1 = liftShowsPrecValName
showsPrecName Show2 = liftShowsPrec2ValName
#else
showsPrecName Show1 = showsPrec1ValName
#endif

#if defined(NEW_FUNCTOR_CLASSES)
showListName :: ShowClass -> Name
showListName Show  = showListValName
showListName Show1 = liftShowListValName
showListName Show2 = liftShowList2ValName

showsPrecOrListName :: Bool -- ^ showListName if True, showsPrecName if False
                    -> ShowClass
                    -> Name
showsPrecOrListName False = showsPrecName
showsPrecOrListName True  = showListName
#endif

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

-- | Parenthesize an infix constructor name if it is being applied as a prefix
-- function (e.g., data Amp a = (:&) a a)
parenInfixConName :: Name -> ShowS
parenInfixConName conName =
    let conNameBase = nameBase conName
     in showParen (isInfixTypeCon conNameBase) $ showString conNameBase

-- | Checks if a 'String' names a valid Haskell infix type constructor (i.e., does
-- it begin with a colon?).
isInfixTypeCon :: String -> Bool
isInfixTypeCon (':':_) = True
isInfixTypeCon _       = False

charE :: Char -> Q Exp
charE = litE . charL
