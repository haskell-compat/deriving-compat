{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-|
Module:      Text.Read.Deriving
Copyright:   (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Read', 'Read1', and 'Read2' instances.
-}
module Text.Read.Deriving.Internal (
      -- * 'Read'
      deriveRead
    , deriveReadOptions
    , makeReadsPrec
    , makeReadsPrecOptions
    , makeReadList
    , makeReadListOptions
    , makeReadPrec
    , makeReadPrecOptions
    , makeReadListPrec
    , makeReadListPrecOptions
      -- * 'Read1'
    , deriveRead1
    , deriveRead1Options
#if defined(NEW_FUNCTOR_CLASSES)
    , makeLiftReadsPrec
    , makeLiftReadsPrecOptions
    , makeLiftReadList
    , makeLiftReadListOptions
# if __GLASGOW_HASKELL__ >= 801
    , makeLiftReadPrec
    , makeLiftReadPrecOptions
    , makeLiftReadListPrec
    , makeLiftReadListPrecOptions
# endif
#endif
    , makeReadsPrec1
    , makeReadsPrec1Options
#if defined(NEW_FUNCTOR_CLASSES)
      -- * 'Read2'
    , deriveRead2
    , deriveRead2Options
    , makeLiftReadsPrec2
    , makeLiftReadsPrec2Options
    , makeLiftReadList2
    , makeLiftReadList2Options
# if __GLASGOW_HASKELL__ >= 801
    , makeLiftReadPrec2
    , makeLiftReadPrec2Options
    , makeLiftReadListPrec2
    , makeLiftReadListPrec2Options
# endif
    , makeReadsPrec2
    , makeReadsPrec2Options
#endif
      -- * 'ReadOptions'
    , ReadOptions(..)
    , defaultReadOptions
    ) where

#if MIN_VERSION_template_haskell(2,11,0)
import           Control.Monad ((<=<))
import           Data.Maybe (fromMaybe, isJust)
#endif

import           Data.Deriving.Internal
import           Data.List (intersperse, partition)
import qualified Data.Map as Map

import           GHC.Show (appPrec, appPrec1)

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

#if defined(MIN_VERSION_ghc_boot)
import           GHC.Lexeme (startsConSym, startsVarSym)
#else
import           Data.Char (isSymbol, ord)
#endif

data ReadOptions = ReadOptions

defaultReadOptions :: ReadOptions
defaultReadOptions = undefined

-- | Generates a 'Read' instance declaration for the given data type or data
-- family instance.
deriveRead :: Name -> Q [Dec]
deriveRead = deriveReadOptions defaultReadOptions

-- | Like 'deriveRead', but takes a 'ReadOptions' argument.
deriveReadOptions :: ReadOptions -> Name -> Q [Dec]
deriveReadOptions = deriveReadClass Read

-- | Generates a lambda expression which behaves like 'readsPrec' (without
-- requiring a 'Read' instance).
makeReadsPrec :: Name -> Q Exp
makeReadsPrec = makeReadsPrecOptions defaultReadOptions

-- | Like 'readsPrec', but takes a 'ReadOptions' argument.
makeReadsPrecOptions :: ReadOptions -> Name -> Q Exp
makeReadsPrecOptions = makeReadPrecClass Read

-- | Generates a lambda expression which behaves like 'readList' (without
-- requiring a 'Read' instance).
makeReadList :: Name -> Q Exp
makeReadList = makeReadListOptions defaultReadOptions

-- | Like 'readList', but takes a 'ReadOptions' argument.
makeReadListOptions :: ReadOptions -> Name -> Q Exp
makeReadListOptions = undefined

-- | Generates a lambda expression which behaves like 'readPrec' (without
-- requiring a 'Read' instance).
makeReadPrec :: Name -> Q Exp
makeReadPrec = makeReadPrecOptions defaultReadOptions

-- | Like 'readPrec', but takes a 'ReadOptions' argument.
makeReadPrecOptions :: ReadOptions -> Name -> Q Exp
makeReadPrecOptions = undefined

-- | Generates a lambda expression which behaves like 'readListPrec' (without
-- requiring a 'Read' instance).
makeReadListPrec :: Name -> Q Exp
makeReadListPrec = makeReadListPrecOptions defaultReadOptions

-- | Like 'readListPrec', but takes a 'ReadOptions' argument.
makeReadListPrecOptions :: ReadOptions -> Name -> Q Exp
makeReadListPrecOptions = undefined

-- | Generates a 'Read1' instance declaration for the given data type or data
-- family instance.
deriveRead1 :: Name -> Q [Dec]
deriveRead1 = deriveRead1Options defaultReadOptions

-- | Like 'deriveRead1', but takes a 'ReadOptions' argument.
deriveRead1Options :: ReadOptions -> Name -> Q [Dec]
deriveRead1Options = deriveReadClass Read1

-- | Generates a lambda expression which behaves like 'readsPrec1' (without
-- requiring a 'Read1' instance).
makeReadsPrec1 :: Name -> Q Exp
makeReadsPrec1 = makeReadsPrec1Options defaultReadOptions

#if defined(NEW_FUNCTOR_CLASSES)
-- | Generates a lambda expression which behaves like 'liftReadsPrec' (without
-- requiring a 'Read1' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftReadsPrec :: Name -> Q Exp
makeLiftReadsPrec = makeLiftReadsPrecOptions defaultReadOptions

-- | Like 'makeLiftReadsPrec', but takes a 'ReadOptions' argument.
--
-- This function is not available with @transformers-0.4@.
makeLiftReadsPrecOptions :: ReadOptions -> Name -> Q Exp
makeLiftReadsPrecOptions = makeReadPrecClass Read1

-- | Generates a lambda expression which behaves like 'liftReadList' (without
-- requiring a 'Read1' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftReadList :: Name -> Q Exp
makeLiftReadList = makeLiftReadListOptions defaultReadOptions

-- | Like 'makeLiftReadList', but takes a 'ReadOptions' argument.
--
-- This function is not available with @transformers-0.4@.
makeLiftReadListOptions :: ReadOptions -> Name -> Q Exp
makeLiftReadListOptions = undefined

# if __GLASGOW_HASKELL__ >= 801
makeLiftReadPrec :: Name -> Q Exp
makeLiftReadPrec = makeLiftReadPrecOptions defaultReadOptions

makeLiftReadPrecOptions :: ReadOptions -> Name -> Q Exp
makeLiftReadPrecOptions = undefined

makeLiftReadListPrec :: Name -> Q Exp
makeLiftReadListPrec = makeLiftReadListPrecOptions defaultReadOptions

makeLiftReadListPrecOptions :: ReadOptions -> Name -> Q Exp
makeLiftReadListPrecOptions = undefined
# endif

-- | Like 'readsPrec1', but takes a 'ReadOptions' argument.
makeReadsPrec1Options :: ReadOptions -> Name -> Q Exp
makeReadsPrec1Options = undefined
#else
-- | Like 'readsPrec1', but takes a 'ReadOptions' argument.
makeReadsPrec1Options :: ReadOptions -> Name -> Q Exp
makeReadsPrec1Options = makeReadPrecClass Read1
#endif

#if defined(NEW_FUNCTOR_CLASSES)
-- | Generates a 'Read2' instance declaration for the given data type or data
-- family instance.
--
-- This function is not available with @transformers-0.4@.
deriveRead2 :: Name -> Q [Dec]
deriveRead2 = deriveRead2Options defaultReadOptions

-- | Like 'deriveRead2', but takes a 'ReadOptions' argument.
--
-- This function is not available with @transformers-0.4@.
deriveRead2Options :: ReadOptions -> Name -> Q [Dec]
deriveRead2Options = deriveReadClass Read2

-- | Generates a lambda expression which behaves like 'liftReadsPrec2' (without
-- requiring a 'Read2' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftReadsPrec2 :: Name -> Q Exp
makeLiftReadsPrec2 = makeLiftReadsPrec2Options defaultReadOptions

-- | Like 'makeLiftReadsPrec2', but takes a 'ReadOptions' argument.
--
-- This function is not available with @transformers-0.4@.
makeLiftReadsPrec2Options :: ReadOptions -> Name -> Q Exp
makeLiftReadsPrec2Options = makeReadPrecClass Read2

-- | Generates a lambda expression which behaves like 'liftReadList2' (without
-- requiring a 'Read2' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftReadList2 :: Name -> Q Exp
makeLiftReadList2 = makeLiftReadList2Options defaultReadOptions

-- | Like 'makeLiftReadList2', but takes a 'ReadOptions' argument.
--
-- This function is not available with @transformers-0.4@.
makeLiftReadList2Options :: ReadOptions -> Name -> Q Exp
makeLiftReadList2Options = undefined

# if __GLASGOW_HASKELL__ >= 801
makeLiftReadPrec2 :: Name -> Q Exp
makeLiftReadPrec2 = makeLiftReadPrec2Options defaultReadOptions

makeLiftReadPrec2Options :: ReadOptions -> Name -> Q Exp
makeLiftReadPrec2Options = undefined

makeLiftReadListPrec2 :: Name -> Q Exp
makeLiftReadListPrec2 = makeLiftReadListPrec2Options defaultReadOptions

makeLiftReadListPrec2Options :: ReadOptions -> Name -> Q Exp
makeLiftReadListPrec2Options = undefined
# endif

-- | Generates a lambda expression which behaves like 'readsPrec2' (without
-- requiring a 'Read2' instance).
--
-- This function is not available with @transformers-0.4@.
makeReadsPrec2 :: Name -> Q Exp
makeReadsPrec2 = makeReadsPrec2Options defaultReadOptions

-- | Like 'makeReadsPrec2', but takes a 'ReadOptions' argument.
--
-- This function is not available with @transformers-0.4@.
makeReadsPrec2Options :: ReadOptions -> Name -> Q Exp
makeReadsPrec2Options = undefined
#endif

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive a Read(1)(2) instance declaration (depending on the ReadClass
-- argument's value).
deriveReadClass :: ReadClass -> ReadOptions -> Name -> Q [Dec]
deriveReadClass rClass opts name = withType name fromCons
  where
    fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q [Dec]
    fromCons name' ctxt tvbs cons mbTys = (:[]) `fmap` do
        (instanceCxt, instanceType)
            <- buildTypeInstance rClass name' ctxt tvbs mbTys
        instanceD (return instanceCxt)
                  (return instanceType)
                  (readPrecDecs rClass opts cons)

-- | Generates a declaration defining the primary function corresponding to a
-- particular class (read(s)Prec for Read, liftRead(s)Prec for Read1, and
-- liftRead(s)Prec2 for Read2).
readPrecDecs :: ReadClass -> ReadOptions -> [Con] -> [Q Dec]
readPrecDecs rClass opts cons =
    [ funD (readsPrecName rClass)
           [ clause []
                    (normalB $ makeReadForCons rClass opts cons)
                    []
           ]
    ]

-- | Generates a lambda expression which behaves like read(s)Prec (for Read),
-- liftRead(s)Prec (for Read1), or liftRead(s)Prec2 (for Read2).
makeReadPrecClass :: ReadClass -> ReadOptions -> Name -> Q Exp
makeReadPrecClass rClass opts name = withType name fromCons
  where
    fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q Exp
    fromCons name' ctxt tvbs cons mbTys =
        -- We force buildTypeInstance here since it performs some checks for whether
        -- or not the provided datatype can actually have
        -- read(s)Prec/liftRead(s)Prec/etc. implemented for it, and produces errors
        -- if it can't.
        buildTypeInstance rClass name' ctxt tvbs mbTys
          `seq` makeReadForCons rClass opts cons

-- | Generates a lambda expression for read(s)Prec/liftRead(s)Prec/etc. for the
-- given constructors. All constructors must be from the same type.
makeReadForCons :: ReadClass -> ReadOptions -> [Con] -> Q Exp
makeReadForCons rClass _opts cons = do
    p   <- newName "p"
    rps <- newNameList "rp" $ arity rClass
    rls <- newNameList "rl" $ arity rClass
    let rpls       = zip rps rls
        _rpsAndRls = interleave rps rls

    let nullaryCons, nonNullaryCons :: [Con]
        (nullaryCons, nonNullaryCons) = partition isNullaryCon cons

        readConsExpr :: Q Exp
        readConsExpr
          | null cons = varE pfailValName
          | otherwise = do
                readNonNullaryCons <- concatMapM (makeReadForCon rClass rpls)
                                                 nonNullaryCons
                foldr1 mkAlt (readNullaryCons ++ map return readNonNullaryCons)

        readNullaryCons :: [Q Exp]
        readNullaryCons = case nullaryCons of
          []    -> []
          [con] -> [doE $ matchCon con
                          ++ [noBindS $ resultExpr (constructorName con) []]]
          _     -> [varE chooseValName `appE` listE (map mkPair nullaryCons)]

        mkAlt :: Q Exp -> Q Exp -> Q Exp
        mkAlt e1 e2 = infixApp e1 (varE altValName) e2

        mkPair :: Con -> Q Exp
        mkPair con = tupE [ stringE $ dataConStr con
                          , resultExpr (constructorName con) []
                          ]

        matchCon :: Con -> [Q Stmt]
        matchCon con
          | isSym conStr = [symbolPat conStr]
          | otherwise    = identHPat conStr
          where
            conStr = dataConStr con

    lamE (map varP $
#if defined(NEW_FUNCTOR_CLASSES)
                     _rpsAndRls ++
#endif
                     [p]
         ) . appsE
         $ [ varE $ readsPrecConstName rClass
           , varE readPrec_to_SValName
               `appE` (varE parensValName `appE` readConsExpr)
               `appE` varE p
           ]
#if defined(NEW_FUNCTOR_CLASSES)
             ++ map varE _rpsAndRls
#endif
             ++ [varE p]

makeReadForCon :: ReadClass
               -> [(Name, Name)]
               -> Con
               -> Q [Exp]
makeReadForCon rClass rpls (NormalC conName _)  = do
    (argTys, tvMap) <- reifyConTys2 rClass rpls conName
    args <- newNameList "arg" $ length argTys
    (readStmts, varExps) <-
        zipWithAndUnzipM (makeReadForArg rClass tvMap conName) argTys args
    let body   = resultExpr conName varExps
        conStr = nameBase conName
        prefixStmts = readPrefixCon conStr ++ readStmts

    e <- mkParser appPrec prefixStmts body
    return [e]
makeReadForCon rClass rpls (RecC conName ts) = do
    (argTys, tvMap) <- reifyConTys2 rClass rpls conName
    args <- newNameList "arg" $ length argTys
    (readStmts, varExps) <- zipWith3AndUnzipM
        (\(argName, _, _) argTy arg -> makeReadForField rClass tvMap conName
                                           (nameBase argName) argTy arg)
        ts argTys args
    let body        = resultExpr conName varExps
        conStr      = nameBase conName
        recordStmts = readPrefixCon conStr ++ [readPunc "{"]
                      ++ concat (intersperse [readPunc ","] readStmts)
                      ++ [readPunc "}"]

    e <- mkParser appPrec1 recordStmts body
    return [e]
makeReadForCon rClass rpls (InfixC _ conName _) = do
    ([alTy, arTy], tvMap) <- reifyConTys2 rClass rpls conName
    al   <- newName "argL"
    ar   <- newName "argR"
    ([readStmt1, readStmt2], varExps) <-
        zipWithAndUnzipM (makeReadForArg rClass tvMap conName)
                         [alTy, arTy] [al, ar]
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
                        _ -> error $ "Text.Read.Deriving.Internal.makeReadForCon: Unsupported type: " ++ show info

    let body   = resultExpr conName varExps
        conStr = nameBase conName
        readInfixCon
          | isSym conStr = [symbolPat conStr]
          | otherwise    = [readPunc "`"] ++ identHPat conStr ++ [readPunc "`"]
        infixStmts = [readStmt1] ++ readInfixCon ++ [readStmt2]

    e <- mkParser conPrec infixStmts body
    return [e]
makeReadForCon rClass rpls (ForallC _ _ con) =
    makeReadForCon rClass rpls con
#if MIN_VERSION_template_haskell(2,11,0)
makeReadForCon rClass rpls (GadtC conNames ts _) =
    let con :: Name -> Q Con
        con conName = do
            mbFi <- reifyFixity conName
            return $ if startsConSym (head $ nameBase conName)
                        && length ts == 2
                        && isJust mbFi
                      then let [t1, t2] = ts in InfixC t1 conName t2
                      else NormalC conName ts

    in concatMapM (makeReadForCon rClass rpls <=< con) conNames
makeReadForCon rClass rpls (RecGadtC conNames ts _) =
    concatMapM (makeReadForCon rClass rpls . flip RecC ts) conNames
#endif

makeReadForArg :: ReadClass
               -> TyVarMap2
               -> Name
               -> Type
               -> Name
               -> Q (Q Stmt, Exp)
makeReadForArg rClass tvMap conName ty tyExpName = do
    (rExp, varExp) <- makeReadForType rClass tvMap conName tyExpName False ty
    let readStmt = bindS (varP tyExpName) $
                         varE stepValName
                            `appE` (varE readS_to_PrecValName `appE` return rExp)
    return (readStmt, varExp)

makeReadForField :: ReadClass
                 -> TyVarMap2
                 -> Name
                 -> String
                 -> Type
                 -> Name
                 -> Q ([Q Stmt], Exp)
makeReadForField rClass tvMap conName lblStr ty tyExpName = do
    (rExp, varExp) <- makeReadForType rClass tvMap conName tyExpName False ty
    let readStmt = bindS (varP tyExpName) $
                         varE resetValName
                            `appE` (varE readS_to_PrecValName `appE` return rExp)
    return (readLbl ++ [readStmt], varExp)
  where
    readLbl | isSym lblStr
            = [readPunc "(", symbolPat lblStr, readPunc ")"]
            | otherwise
            = identHPat lblStr

makeReadForType :: ReadClass
                -> TyVarMap2
                -> Name
                -> Name
                -> Bool
                -> Type
                -> Q (Exp, Exp)
#if defined(NEW_FUNCTOR_CLASSES)
makeReadForType _ tvMap _ tyExpName rl (VarT tyName) =
    let tyExp = VarE tyExpName
    in return $ case Map.lookup tyName tvMap of
      Just (TwoNames rpExp rlExp) -> (VarE $ if rl then rlExp else rpExp, tyExp)
      Nothing -> (VarE $ if rl then readListValName else readsPrecValName, tyExp)
#else
makeReadForType _ _ _ tyExpName _ VarT{} =
    return (VarE readsPrecValName, VarE tyExpName)
#endif
makeReadForType rClass tvMap conName tyExpName rl (SigT ty _) =
    makeReadForType rClass tvMap conName tyExpName rl ty
makeReadForType rClass tvMap conName tyExpName rl (ForallT _ _ ty) =
    makeReadForType rClass tvMap conName tyExpName rl ty
#if defined(NEW_FUNCTOR_CLASSES)
makeReadForType rClass tvMap conName tyExpName rl ty = do
    let tyCon :: Type
        tyArgs :: [Type]
        tyCon:tyArgs = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (arity rClass) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNames :: [Name]
        tyVarNames = Map.keys tvMap

    itf <- isTyFamily tyCon
    if any (`mentionsName` tyVarNames) lhsArgs
          || itf && any (`mentionsName` tyVarNames) tyArgs
       then outOfPlaceTyVarError rClass conName
       else do
        readExp <- appsE $ [ varE . readsPrecOrListName rl $ toEnum numLastArgs]
                   ++ zipWith (\b -> fmap fst
                                   . makeReadForType rClass tvMap conName tyExpName b)
                              (cycle [False,True])
                              (interleave rhsArgs rhsArgs)
        return (readExp, VarE tyExpName)
#else
makeReadForType rClass tvMap conName tyExpName _ ty = do
  let varNames = Map.keys tvMap

  case varNames of
    [] -> return (VarE readsPrecValName, VarE tyExpName)
    varName:_ -> do
      if mentionsName ty varNames
         then do
             applyExp <- makeFmapApplyPos rClass conName ty varName
             return (VarE readsPrec1ValName, applyExp `AppE` VarE tyExpName)
         else return (VarE readsPrecValName, VarE tyExpName)
#endif

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which @Read@ variant is being derived.
data ReadClass = Read
               | Read1
#if defined(NEW_FUNCTOR_CLASSES)
               | Read2
#endif
  deriving (Bounded, Enum)

instance ClassRep ReadClass where
    arity = fromEnum

    allowExQuant _ = False

    fullClassName Read  = readTypeName
    fullClassName Read1 = read1TypeName
#if defined(NEW_FUNCTOR_CLASSES)
    fullClassName Read2 = read2TypeName
#endif

    classConstraint rClass i
      | rMin <= i && i <= rMax = Just $ fullClassName (toEnum i :: ReadClass)
      | otherwise              = Nothing
      where
        rMin, rMax :: Int
        rMin = fromEnum (minBound :: ReadClass)
        rMax = fromEnum rClass

readsPrecConstName :: ReadClass -> Name
readsPrecConstName Read  = readsPrecConstValName
#if defined(NEW_FUNCTOR_CLASSES)
readsPrecConstName Read1 = liftReadsPrecConstValName
readsPrecConstName Read2 = liftReadsPrec2ConstValName
#else
readsPrecConstName Read1 = readsPrec1ConstValName
#endif

-- readPrecConstName :: ReadClass -> Name
-- readPrecConstName Read  = readPrecConstValName
-- #if defined(NEW_FUNCTOR_CLASSES) && __GLASGOW_HASKELL__ >= 801
-- readPrecConstName Read1 = liftReadPrecConstValName
-- readPrecConstName Read2 = liftReadPrec2ConstValName
-- #endif

readsPrecName :: ReadClass -> Name
readsPrecName Read  = readsPrecValName
#if defined(NEW_FUNCTOR_CLASSES)
readsPrecName Read1 = liftReadsPrecValName
readsPrecName Read2 = liftReadsPrec2ValName
#else
readsPrecName Read1 = readsPrec1ValName
#endif

-- readPrecName :: ReadClass -> Name
-- readPrecName Read  = readPrecValName
-- #if defined(NEW_FUNCTOR_CLASSES) && __GLASGOW_HASKELL__ >= 801
-- readPrecName Read1 = liftReadPrecValName
-- readPrecName Read2 = liftReadPrec2ValName
-- #endif

#if defined(NEW_FUNCTOR_CLASSES)
readListName :: ReadClass -> Name
readListName Read  = readListValName
readListName Read1 = liftReadListValName
readListName Read2 = liftReadList2ValName

-- readListPrecName :: ReadClass -> Name
-- readListPrecName Read  = readListPrecValName
-- readListPrecName Read1 = liftReadListPrecValName
-- readListPrecName Read2 = liftReadListPrec2ValName

readsPrecOrListName :: Bool -- ^ readListName if True, readsPrecName if False
                    -> ReadClass
                    -> Name
readsPrecOrListName False = readsPrecName
readsPrecOrListName True  = readListName
#endif

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

mkParser :: Int -> [Q Stmt] -> Q Exp -> Q Exp
mkParser p ss b = varE precValName
           `appE` litE (integerL $ fromIntegral p)
           `appE` doE (ss ++ [noBindS b])

resultExpr :: Name -> [Exp] -> Q Exp
resultExpr conName as = varE returnValName `appE` conApp
  where
    conApp :: Q Exp
    conApp = appsE $ conE conName : map return as

isSym :: String -> Bool
isSym ""      = False
isSym (c : _) = startsVarSym c || startsConSym c

#if !defined(MIN_VERSION_ghc_boot)
startsVarSym, startsConSym :: Char -> Bool
startsVarSym c = startsVarSymASCII c || (ord c > 0x7f && isSymbol c) -- Infix Ids
startsConSym c = c == ':' -- Infix data constructors

startsVarSymASCII :: Char -> Bool
startsVarSymASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"
#endif

identHPat :: String -> [Q Stmt]
identHPat s
    | Just (ss, '#') <- snocView s = [identPat ss, symbolPat "#"]
    | otherwise                    = [identPat s]

bindLex :: Q Exp -> Q Stmt
bindLex pat = noBindS $ varE expectPValName `appE` pat

identPat :: String -> Q Stmt
identPat s = bindLex $ conE identDataName `appE` stringE s

symbolPat :: String -> Q Stmt
symbolPat s = bindLex $ conE symbolDataName `appE` stringE s

readPunc :: String -> Q Stmt
readPunc c = bindLex $ conE puncDataName `appE` stringE c

snocView :: [a] -> Maybe ([a],a)
        -- Split off the last element
snocView [] = Nothing
snocView xs = go [] xs
  where
      -- Invariant: second arg is non-empty
    go acc [a]    = Just (reverse acc, a)
    go acc (a:as) = go (a:acc) as
    go _   []     = error "Util: snocView"

dataConStr :: Con -> String
dataConStr = nameBase . constructorName

readPrefixCon :: String -> [Q Stmt]
readPrefixCon conStr
  | isSym conStr = [readPunc "(", symbolPat conStr, readPunc ")"]
  | otherwise    = identHPat conStr
