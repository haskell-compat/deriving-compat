{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-|
Module:      Text.Read.Deriving
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Read', 'Read1', and 'Read2' instances.

Note: this is an internal module, and as such, the API presented here is not
guaranteed to be stable, even between minor releases of this library.
-}
module Text.Read.Deriving.Internal (
      -- * 'Read'
      deriveRead
    , deriveReadOptions
    , makeReadsPrec
--     , makeReadsPrecOptions
--     , makeReadList
--     , makeReadListOptions
    , makeReadPrec
--     , makeReadPrecOptions
--     , makeReadListPrec
--     , makeReadListPrecOptions
      -- * 'Read1'
    , deriveRead1
    , deriveRead1Options
#if defined(NEW_FUNCTOR_CLASSES)
    , makeLiftReadsPrec
--     , makeLiftReadsPrecOptions
--     , makeLiftReadList
--     , makeLiftReadListOptions
# if __GLASGOW_HASKELL__ >= 801
    , makeLiftReadPrec
--     , makeLiftReadPrecOptions
--     , makeLiftReadListPrec
--     , makeLiftReadListPrecOptions
    , makeReadPrec1
--     , makeReadPrec1Options
# endif
#endif
    , makeReadsPrec1
--     , makeReadsPrec1Options
#if defined(NEW_FUNCTOR_CLASSES)
      -- * 'Read2'
    , deriveRead2
    , deriveRead2Options
    , makeLiftReadsPrec2
--     , makeLiftReadsPrec2Options
--     , makeLiftReadList2
--     , makeLiftReadList2Options
# if __GLASGOW_HASKELL__ >= 801
    , makeLiftReadPrec2
--     , makeLiftReadPrec2Options
--     , makeLiftReadListPrec2
--     , makeLiftReadListPrec2Options
    , makeReadPrec2
--     , makeReadPrec2Options
# endif
    , makeReadsPrec2
--     , makeReadsPrec2Options
#endif
      -- * 'ReadOptions'
    , ReadOptions(..)
    , defaultReadOptions
    ) where

import           Data.Deriving.Internal
import           Data.List (intersperse, partition)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)

import           GHC.Show (appPrec, appPrec1)

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

-- | Options that further configure how the functions in "Text.Read.Deriving"
-- should behave.
newtype ReadOptions = ReadOptions
  { useReadPrec :: Bool
    -- ^ If 'True':
    --
    -- * Derived 'Read' instances will implement 'readPrec', not 'readsPrec', and
    --   will provide a default implementation of 'readListPrec' in terms of
    --   'readPrec'.
    --
    -- * If built against @base-4.10@ or later, derived 'Read1'/'Read2'
    --   instances will implement 'liftReadPrec'/'liftReadPrec2', not
    --   'liftReadsPrec'/'liftReadsPrec2', and will provide default implementations
    --   of 'liftReadListPrec'/'liftReadListPrec2' in terms of
    --   'liftReadPrec'/'liftReadPrec2'. If built against an earlier version of
    --   @base@, derived 'Read1'/'Read2' instances are not affected, so they will
    --   act as if this flag were 'False'.
    --
    -- If 'False':
    --
    -- * Derived 'Read' instances will implement 'readsPrec'.
    --
    -- * Derived 'Read1' instances will implement 'readsPrec1' (if built against
    --   @transformers-0.4@) or 'liftReadsPrec' (otherwise). If not built against
    --   @transformers-0.4@, derived 'Read2' instances will implement
    --   'liftReadsPrec2'.
    --
    -- It's generally a good idea to enable this option, since 'readPrec' and
    -- friends are more efficient than 'readsPrec' and friends, since the former
    -- use the efficient 'ReadPrec' parser datatype while the latter use the
    -- slower, list-based 'ReadS' type.
  } deriving (Eq, Ord, Read, Show)

-- | 'ReadOptions' that favor 'readPrec' over 'readsPrec'.
defaultReadOptions :: ReadOptions
defaultReadOptions = ReadOptions { useReadPrec = True }

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
makeReadsPrec = makeReadPrecClass Read False

-- -- | Like 'readsPrec', but takes a 'ReadOptions' argument.
-- makeReadsPrecOptions :: ReadOptions -> Name -> Q Exp
-- makeReadsPrecOptions _ = makeReadPrecClass Read False
--
-- -- | Generates a lambda expression which behaves like 'readList' (without
-- -- requiring a 'Read' instance).
-- makeReadList :: Name -> Q Exp
-- makeReadList = makeReadListOptions defaultReadOptions
--
-- -- | Like 'readList', but takes a 'ReadOptions' argument.
-- makeReadListOptions :: ReadOptions -> Name -> Q Exp
-- makeReadListOptions opts name =
--     if shouldDefineReadPrec Read opts
--        then varE readPrec_to_SValName
--             `appE` makeReadListPrecOptions opts name
--             `appE` integerE 0
--        else varE readPrec_to_SValName
--             `appE` (varE listValName `appE` makeReadPrecOptions opts name)
--             `appE` integerE 0

-- | Generates a lambda expression which behaves like 'readPrec' (without
-- requiring a 'Read' instance).
makeReadPrec :: Name -> Q Exp
makeReadPrec = makeReadPrecClass Read True

-- -- | Like 'readPrec', but takes a 'ReadOptions' argument.
-- makeReadPrecOptions :: ReadOptions -> Name -> Q Exp
-- makeReadPrecOptions _ = makeReadPrecClass Read True
--
-- -- | Generates a lambda expression which behaves like 'readListPrec' (without
-- -- requiring a 'Read' instance).
-- makeReadListPrec :: Name -> Q Exp
-- makeReadListPrec = makeReadListPrecOptions defaultReadOptions
--
-- -- | Like 'readListPrec', but takes a 'ReadOptions' argument.
-- makeReadListPrecOptions :: ReadOptions -> Name -> Q Exp
-- makeReadListPrecOptions opts name =
--     if shouldDefineReadPrec Read opts
--        then varE listValName `appE` makeReadPrecOptions opts name
--        else varE readS_to_PrecValName
--             `appE` (varE constValName `appE` makeReadListOptions opts name)

-- | Generates a 'Read1' instance declaration for the given data type or data
-- family instance.
deriveRead1 :: Name -> Q [Dec]
deriveRead1 = deriveRead1Options defaultReadOptions

-- | Like 'deriveRead1', but takes a 'ReadOptions' argument.
deriveRead1Options :: ReadOptions -> Name -> Q [Dec]
deriveRead1Options = deriveReadClass Read1

-- -- | Generates a lambda expression which behaves like 'readsPrec1' (without
-- -- requiring a 'Read1' instance).
-- makeReadsPrec1 :: Name -> Q Exp
-- makeReadsPrec1 = makeReadsPrec1Options defaultReadOptions

#if defined(NEW_FUNCTOR_CLASSES)
-- | Generates a lambda expression which behaves like 'liftReadsPrec' (without
-- requiring a 'Read1' instance).
--
-- This function is not available with @transformers-0.4@.
makeLiftReadsPrec :: Name -> Q Exp
makeLiftReadsPrec = makeReadPrecClass Read1 False

-- -- | Like 'makeLiftReadsPrec', but takes a 'ReadOptions' argument.
-- --
-- -- This function is not available with @transformers-0.4@.
-- makeLiftReadsPrecOptions :: ReadOptions -> Name -> Q Exp
-- makeLiftReadsPrecOptions _ = makeReadPrecClass Read1 False
--
-- -- | Generates a lambda expression which behaves like 'liftReadList' (without
-- -- requiring a 'Read1' instance).
-- --
-- -- This function is not available with @transformers-0.4@.
-- makeLiftReadList :: Name -> Q Exp
-- makeLiftReadList = makeLiftReadListOptions defaultReadOptions
--
-- -- | Like 'makeLiftReadList', but takes a 'ReadOptions' argument.
-- --
-- -- This function is not available with @transformers-0.4@.
-- makeLiftReadListOptions :: ReadOptions -> Name -> Q Exp
-- makeLiftReadListOptions = undefined

# if __GLASGOW_HASKELL__ >= 801
-- | Generates a lambda expression which behaves like 'liftReadPrec' (without
-- requiring a 'Read1' instance).
--
-- This function is only available with @base-4.10@ or later.
makeLiftReadPrec :: Name -> Q Exp
makeLiftReadPrec = makeReadPrecClass Read1 True

-- -- | Like 'makeLiftReadPrec', but takes a 'ReadOptions' argument.
-- --
-- -- This function is only available with @base-4.10@ or later.
-- makeLiftReadPrecOptions :: ReadOptions -> Name -> Q Exp
-- makeLiftReadPrecOptions _ = makeReadPrecClass Read1 True
--
-- -- | Generates a lambda expression which behaves like 'liftReadListPrec' (without
-- -- requiring a 'Read1' instance).
-- --
-- -- This function is only available with @base-4.10@ or later.
-- makeLiftReadListPrec :: Name -> Q Exp
-- makeLiftReadListPrec = makeLiftReadListPrecOptions defaultReadOptions
--
-- -- | Like 'makeLiftReadListPrec', but takes a 'ReadOptions' argument.
-- --
-- -- This function is only available with @base-4.10@ or later.
-- makeLiftReadListPrecOptions :: ReadOptions -> Name -> Q Exp
-- makeLiftReadListPrecOptions = undefined

-- | Generates a lambda expression which behaves like 'readPrec1' (without
-- requiring a 'Read1' instance).
--
-- This function is only available with @base-4.10@ or later.
makeReadPrec1 :: Name -> Q Exp
makeReadPrec1 name = makeLiftReadPrec name
                     `appE` varE readPrecValName
                     `appE` varE readListPrecValName

-- -- | Like 'makeReadPrec1', but takes a 'ReadOptions' argument.
-- --
-- -- This function is only available with @base-4.10@ or later.
-- makeReadPrec1Options :: ReadOptions -> Name -> Q Exp
-- makeReadPrec1Options opts name = makeLiftReadPrecOptions opts name
--                           `appE` varE readPrecValName
--                           `appE` varE readListPrecValName
# endif
-- | Generates a lambda expression which behaves like 'readsPrec1' (without
-- requiring a 'Read1' instance).
makeReadsPrec1 :: Name -> Q Exp
makeReadsPrec1 name = makeLiftReadsPrec name
                      `appE` varE readsPrecValName
                      `appE` varE readListValName

-- -- | Like 'makeReadsPrec1Options', but takes a 'ReadOptions' argument.
-- makeReadsPrec1Options :: ReadOptions -> Name -> Q Exp
-- makeReadsPrec1Options opts name = makeLiftReadsPrecOptions opts name
--                            `appE` varE readsPrecValName
--                            `appE` varE readListValName
#else
-- | Generates a lambda expression which behaves like 'readsPrec1' (without
-- requiring a 'Read1' instance).
makeReadsPrec1 :: Name -> Q Exp
makeReadsPrec1 = makeReadPrecClass Read1 False

-- -- | Like 'makeReadsPrec1Options', but takes a 'ReadOptions' argument.
-- makeReadsPrec1Options :: ReadOptions -> Name -> Q Exp
-- makeReadsPrec1Options _ = makeReadPrecClass Read1 False
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
makeLiftReadsPrec2 = makeReadPrecClass Read2 False

-- -- | Like 'makeLiftReadsPrec2', but takes a 'ReadOptions' argument.
-- --
-- -- This function is not available with @transformers-0.4@.
-- makeLiftReadsPrec2Options :: ReadOptions -> Name -> Q Exp
-- makeLiftReadsPrec2Options _ = makeReadPrecClass Read2 False
--
-- -- | Generates a lambda expression which behaves like 'liftReadList2' (without
-- -- requiring a 'Read2' instance).
-- --
-- -- This function is not available with @transformers-0.4@.
-- makeLiftReadList2 :: Name -> Q Exp
-- makeLiftReadList2 = makeLiftReadList2Options defaultReadOptions
--
-- -- | Like 'makeLiftReadList2', but takes a 'ReadOptions' argument.
-- --
-- -- This function is not available with @transformers-0.4@.
-- makeLiftReadList2Options :: ReadOptions -> Name -> Q Exp
-- makeLiftReadList2Options opts name = do
--     let rp1Expr   = VarE `fmap` newName "rp1'"
--         rl1Expr   = VarE `fmap` newName "rl1'"
--         rp2Expr   = VarE `fmap` newName "rp2'"
--         rl2Expr   = VarE `fmap` newName "rl2'"
--     let rp2sExpr  = varE readPrec_to_SValName
--         rs2pExpr  = varE readS_to_PrecValName
--         constExpr = varE constValName
--     if shouldDefineReadPrec Read2 opts
--        then rp2sExpr
--             `appE` (makeLiftReadListPrec2Options opts name
--                     `appE` (rs2pExpr `appE` rp1Expr)
--                     `appE` (rs2pExpr `appE` (constExpr `appE` rl1Expr))
--                     `appE` (rs2pExpr `appE` rp2Expr)
--                     `appE` (rs2pExpr `appE` (constExpr `appE` rl2Expr)))
--             `appE` integerE 0
--        else rp2sExpr `appE` (varE listValName
--             `appE` (makeLiftReadPrec2Options opts name
--                     `appE` (rs2pExpr `appE` rp1Expr)
--                     `appE` (rs2pExpr `appE` (constExpr `appE` rl1Expr))
--                     `appE` (rs2pExpr `appE` rp2Expr)
--                     `appE` (rs2pExpr `appE` (constExpr `appE` rl2Expr))))
--             `appE` integerE 0

# if __GLASGOW_HASKELL__ >= 801
-- | Generates a lambda expression which behaves like 'liftReadPrec2' (without
-- requiring a 'Read2' instance).
--
-- This function is only available with @base-4.10@ or later.
makeLiftReadPrec2 :: Name -> Q Exp
makeLiftReadPrec2 = makeReadPrecClass Read2 True

-- -- | Like 'makeLiftReadPrec2', but takes a 'ReadOptions' argument.
-- --
-- -- This function is only available with @base-4.10@ or later.
-- makeLiftReadPrec2Options :: ReadOptions -> Name -> Q Exp
-- makeLiftReadPrec2Options _ = makeReadPrecClass Read2 True
--
-- -- | Generates a lambda expression which behaves like 'liftReadListPrec2' (without
-- -- requiring a 'Read2' instance).
-- --
-- -- This function is only available with @base-4.10@ or later.
-- makeLiftReadListPrec2 :: Name -> Q Exp
-- makeLiftReadListPrec2 = makeLiftReadListPrec2Options defaultReadOptions
--
-- -- | Like 'makeLiftReadListPrec2', but takes a 'ReadOptions' argument.
-- --
-- -- This function is only available with @base-4.10@ or later.
-- makeLiftReadListPrec2Options :: ReadOptions -> Name -> Q Exp
-- makeLiftReadListPrec2Options = undefined

-- | Generates a lambda expression which behaves like 'readPrec2' (without
-- requiring a 'Read2' instance).
--
-- This function is only available with @base-4.10@ or later.
makeReadPrec2 :: Name -> Q Exp
makeReadPrec2 name = makeLiftReadPrec2 name
                     `appE` varE readPrecValName
                     `appE` varE readListPrecValName
                     `appE` varE readPrecValName
                     `appE` varE readListPrecValName

-- -- | Like 'makeReadPrec2', but takes a 'ReadOptions' argument.
-- --
-- -- This function is only available with @base-4.10@ or later.
-- makeReadPrec2Options :: ReadOptions -> Name -> Q Exp
-- makeReadPrec2Options opts name = makeLiftReadPrec2Options opts name
--                           `appE` varE readPrecValName
--                           `appE` varE readListPrecValName
--                           `appE` varE readPrecValName
--                           `appE` varE readListPrecValName
# endif

-- | Generates a lambda expression which behaves like 'readsPrec2' (without
-- requiring a 'Read2' instance).
--
-- This function is not available with @transformers-0.4@.
makeReadsPrec2 :: Name -> Q Exp
makeReadsPrec2 name = makeLiftReadsPrec2 name
                      `appE` varE readsPrecValName
                      `appE` varE readListValName
                      `appE` varE readsPrecValName
                      `appE` varE readListValName

-- -- | Like 'makeReadsPrec2', but takes a 'ReadOptions' argument.
-- --
-- -- This function is not available with @transformers-0.4@.
-- makeReadsPrec2Options :: ReadOptions -> Name -> Q Exp
-- makeReadsPrec2Options opts name = makeLiftReadsPrec2Options opts name
--                           `appE` varE readsPrecValName
--                           `appE` varE readListValName
--                           `appE` varE readsPrecValName
--                           `appE` varE readListValName
#endif

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive a Read(1)(2) instance declaration (depending on the ReadClass
-- argument's value).
deriveReadClass :: ReadClass -> ReadOptions -> Name -> Q [Dec]
deriveReadClass rClass opts name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (instanceCxt, instanceType)
          <- buildTypeInstance rClass parentName ctxt instTypes variant
      (:[]) `fmap` instanceD (return instanceCxt)
                             (return instanceType)
                             (readPrecDecs rClass opts instTypes cons)

-- | Generates a declaration defining the primary function corresponding to a
-- particular class (read(s)Prec for Read, liftRead(s)Prec for Read1, and
-- liftRead(s)Prec2 for Read2).
readPrecDecs :: ReadClass -> ReadOptions -> [Type] -> [ConstructorInfo] -> [Q Dec]
readPrecDecs rClass opts instTypes cons =
    [ funD ((if defineReadPrec then readPrecName else readsPrecName) rClass)
           [ clause []
                    (normalB $ makeReadForCons rClass defineReadPrec instTypes cons)
                    []
           ]
    ] ++ if defineReadPrec
            then [ funD (readListPrecName rClass)
                        [ clause []
                                 (normalB . varE $ readListPrecDefaultName rClass)
                                 []
                        ]
                 ]
            else []
  where
    defineReadPrec :: Bool
    defineReadPrec = shouldDefineReadPrec rClass opts

-- | Generates a lambda expression which behaves like read(s)Prec (for Read),
-- liftRead(s)Prec (for Read1), or liftRead(s)Prec2 (for Read2).
makeReadPrecClass :: ReadClass -> Bool -> Name -> Q Exp
makeReadPrecClass rClass urp name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      -- We force buildTypeInstance here since it performs some checks for whether
      -- or not the provided datatype can actually have
      -- read(s)Prec/liftRead(s)Prec/etc. implemented for it, and produces errors
      -- if it can't.
      buildTypeInstance rClass parentName ctxt instTypes variant
        >> makeReadForCons rClass urp instTypes cons

-- | Generates a lambda expression for read(s)Prec/liftRead(s)Prec/etc. for the
-- given constructors. All constructors must be from the same type.
makeReadForCons :: ReadClass -> Bool -> [Type] -> [ConstructorInfo] -> Q Exp
makeReadForCons rClass urp instTypes cons = do
    p   <- newName "p"
    rps <- newNameList "rp" $ arity rClass
    rls <- newNameList "rl" $ arity rClass
    let rpls       = zip rps rls
        _rpsAndRls = interleave rps rls
        lastTyVars = map varTToName $ drop (length instTypes - fromEnum rClass) instTypes
        rplMap     = Map.fromList $ zipWith (\x (y, z) -> (x, TwoNames y z)) lastTyVars rpls

    let nullaryCons, nonNullaryCons :: [ConstructorInfo]
        (nullaryCons, nonNullaryCons) = partition isNullaryCon cons

        readConsExpr :: Q Exp
        readConsExpr = do
          readNonNullaryCons <- mapM (makeReadForCon rClass urp rplMap)
                                     nonNullaryCons
          foldr1 mkAlt (readNullaryCons ++ map return readNonNullaryCons)

        readNullaryCons :: [Q Exp]
        readNullaryCons = case nullaryCons of
          [] -> []
          [con]
            | nameBase (constructorName con) == "()"
           -> [varE parenValName `appE`
                    mkDoStmts [] (varE returnValName `appE` tupE [])]
            | otherwise -> [mkDoStmts (matchCon con)
                                      (resultExpr (constructorName con) [])]
          _ -> [varE chooseValName `appE` listE (map mkPair nullaryCons)]

        mkAlt :: Q Exp -> Q Exp -> Q Exp
        mkAlt e1 e2 = infixApp e1 (varE altValName) e2

        mkPair :: ConstructorInfo -> Q Exp
        mkPair con = tupE [ stringE $ dataConStr con
                          , resultExpr (constructorName con) []
                          ]

        matchCon :: ConstructorInfo -> [Q Stmt]
        matchCon con
          | isSym conStr = [symbolPat conStr]
          | otherwise    = identHPat conStr
          where
            conStr = dataConStr con

        mainRhsExpr :: Q Exp
        mainRhsExpr
          | null cons = varE pfailValName
          | otherwise = varE parensValName `appE` readConsExpr

    lamE (map varP $
#if defined(NEW_FUNCTOR_CLASSES)
                     _rpsAndRls ++
#endif
                     if urp then [] else [p]
         ) . appsE
         $ [ varE $ (if urp then readPrecConstName else readsPrecConstName) rClass
           , if urp
                then mainRhsExpr
                else varE readPrec_to_SValName `appE` mainRhsExpr `appE` varE p
           ]
#if defined(NEW_FUNCTOR_CLASSES)
             ++ map varE _rpsAndRls
#endif
             ++ if urp then [] else [varE p]

makeReadForCon :: ReadClass
               -> Bool
               -> TyVarMap2
               -> ConstructorInfo
               -> Q Exp
makeReadForCon rClass urp tvMap
  (ConstructorInfo { constructorName    = conName
                   , constructorContext = ctxt
                   , constructorVariant = NormalConstructor
                   , constructorFields  = argTys }) = do
    argTys' <- mapM resolveTypeSynonyms argTys
    args    <- newNameList "arg" $ length argTys'
    let conStr = nameBase conName
        isTup  = isNonUnitTupleString conStr
    (readStmts, varExps) <-
        zipWithAndUnzipM (makeReadForArg rClass isTup urp tvMap conName) argTys' args
    let body = resultExpr conName varExps

    checkExistentialContext rClass tvMap ctxt conName $
      if isTup
         then let tupleStmts = intersperse (readPunc ",") readStmts
              in varE parenValName `appE` mkDoStmts tupleStmts body
         else let prefixStmts = readPrefixCon conStr ++ readStmts
              in mkParser appPrec prefixStmts body
makeReadForCon rClass urp tvMap
  (ConstructorInfo { constructorName    = conName
                   , constructorContext = ctxt
                   , constructorVariant = RecordConstructor argNames
                   , constructorFields  = argTys }) = do
    argTys' <- mapM resolveTypeSynonyms argTys
    args    <- newNameList "arg" $ length argTys'
    (readStmts, varExps) <- zipWith3AndUnzipM
        (\argName argTy arg -> makeReadForField rClass urp tvMap conName
                                           (nameBase argName) argTy arg)
        argNames argTys' args
    let body        = resultExpr conName varExps
        conStr      = nameBase conName
        recordStmts = readPrefixCon conStr ++ [readPunc "{"]
                      ++ concat (intersperse [readPunc ","] readStmts)
                      ++ [readPunc "}"]

    checkExistentialContext rClass tvMap ctxt conName $
      mkParser appPrec1 recordStmts body
makeReadForCon rClass urp tvMap
  (ConstructorInfo { constructorName    = conName
                   , constructorContext = ctxt
                   , constructorVariant = InfixConstructor
                   , constructorFields  = argTys }) = do
    [alTy, arTy] <- mapM resolveTypeSynonyms argTys
    al <- newName "argL"
    ar <- newName "argR"
    fi <- fromMaybe defaultFixity `fmap` reifyFixityCompat conName
    ([readStmt1, readStmt2], varExps) <-
        zipWithAndUnzipM (makeReadForArg rClass False urp tvMap conName)
                         [alTy, arTy] [al, ar]

    let conPrec = case fi of Fixity prec _ -> prec
        body    = resultExpr conName varExps
        conStr  = nameBase conName
        readInfixCon
          | isSym conStr = [symbolPat conStr]
          | otherwise    = [readPunc "`"] ++ identHPat conStr ++ [readPunc "`"]
        infixStmts = [readStmt1] ++ readInfixCon ++ [readStmt2]

    checkExistentialContext rClass tvMap ctxt conName $
      mkParser conPrec infixStmts body

makeReadForArg :: ReadClass
               -> Bool
               -> Bool
               -> TyVarMap2
               -> Name
               -> Type
               -> Name
               -> Q (Q Stmt, Exp)
makeReadForArg rClass isTup urp tvMap conName ty tyExpName = do
    (rExp, varExp) <- makeReadForType rClass urp tvMap conName tyExpName False ty
    let readStmt = bindS (varP tyExpName) $
                         (if (not isTup) then appE (varE stepValName) else id) $
                            wrapReadS urp (return rExp)
    return (readStmt, varExp)

makeReadForField :: ReadClass
                 -> Bool
                 -> TyVarMap2
                 -> Name
                 -> String
                 -> Type
                 -> Name
                 -> Q ([Q Stmt], Exp)
makeReadForField rClass urp tvMap conName lblStr ty tyExpName = do
    (rExp, varExp) <- makeReadForType rClass urp tvMap conName tyExpName False ty
    let readStmt = bindS (varP tyExpName) $
                     read_field `appE`
                     (varE resetValName `appE` wrapReadS urp (return rExp))
    return ([readStmt], varExp)
  where
    mk_read_field readFieldName lbl
      = varE readFieldName `appE` stringE lbl
    read_field
      | isSym lblStr
      = mk_read_field readSymFieldValName lblStr
      | Just (ss, '#') <- snocView lblStr
      = mk_read_field readFieldHashValName ss
      | otherwise
      = mk_read_field readFieldValName lblStr

makeReadForType :: ReadClass
                -> Bool
                -> TyVarMap2
                -> Name
                -> Name
                -> Bool
                -> Type
                -> Q (Exp, Exp)
#if defined(NEW_FUNCTOR_CLASSES)
makeReadForType _ urp tvMap _ tyExpName rl (VarT tyName) =
    let tyExp = VarE tyExpName
    in return $ case Map.lookup tyName tvMap of
      Just (TwoNames rpExp rlExp) -> (VarE $ if rl then rlExp else rpExp, tyExp)
      Nothing                     -> (VarE $ readsOrReadName urp rl Read, tyExp)
#else
makeReadForType _ urp _ _ tyExpName _ VarT{} =
    return (VarE $ readsOrReadName urp False Read, VarE tyExpName)
#endif
makeReadForType rClass urp tvMap conName tyExpName rl (SigT ty _) =
    makeReadForType rClass urp tvMap conName tyExpName rl ty
makeReadForType rClass urp tvMap conName tyExpName rl (ForallT _ _ ty) =
    makeReadForType rClass urp tvMap conName tyExpName rl ty
#if defined(NEW_FUNCTOR_CLASSES)
makeReadForType rClass urp tvMap conName tyExpName rl ty = do
    let tyCon :: Type
        tyArgs :: [Type]
        (tyCon, tyArgs) = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (arity rClass) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNames :: [Name]
        tyVarNames = Map.keys tvMap

    itf <- isInTypeFamilyApp tyVarNames tyCon tyArgs
    if any (`mentionsName` tyVarNames) lhsArgs
          || itf && any (`mentionsName` tyVarNames) tyArgs
       then outOfPlaceTyVarError rClass conName
       else if any (`mentionsName` tyVarNames) rhsArgs
               then do
                 readExp <- appsE $ [ varE . readsOrReadName urp rl $ toEnum numLastArgs]
                            ++ zipWith (\b -> fmap fst
                                            . makeReadForType rClass urp tvMap conName tyExpName b)
                                       (cycle [False,True])
                                       (interleave rhsArgs rhsArgs)
                 return (readExp, VarE tyExpName)
               else return (VarE $ readsOrReadName urp rl Read, VarE tyExpName)
#else
makeReadForType rClass urp tvMap conName tyExpName _ ty = do
  let varNames = Map.keys tvMap
      rpExpr   = VarE $ readsOrReadName urp False Read
      rp1Expr  = VarE $ readsOrReadName urp False Read1
      tyExpr   = VarE tyExpName

  case varNames of
    [] -> return (rpExpr, tyExpr)
    varName:_ -> do
      if mentionsName ty varNames
         then do
             applyExp <- makeFmapApplyPos rClass conName ty varName
             return (rp1Expr, applyExp `AppE` tyExpr)
         else return (rpExpr, tyExpr)
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

readPrecConstName :: ReadClass -> Name
readPrecConstName Read  = readPrecConstValName
readPrecConstName Read1 = liftReadPrecConstValName
#if defined(NEW_FUNCTOR_CLASSES)
readPrecConstName Read2 = liftReadPrec2ConstValName
#endif

readsPrecName :: ReadClass -> Name
readsPrecName Read  = readsPrecValName
#if defined(NEW_FUNCTOR_CLASSES)
readsPrecName Read1 = liftReadsPrecValName
readsPrecName Read2 = liftReadsPrec2ValName
#else
readsPrecName Read1 = readsPrec1ValName
#endif

readPrecName :: ReadClass -> Name
readPrecName Read  = readPrecValName
readPrecName Read1 = liftReadPrecValName
#if defined(NEW_FUNCTOR_CLASSES)
readPrecName Read2 = liftReadPrec2ValName
#endif

readListPrecDefaultName :: ReadClass -> Name
readListPrecDefaultName Read  = readListPrecDefaultValName
readListPrecDefaultName Read1 = liftReadListPrecDefaultValName
#if defined(NEW_FUNCTOR_CLASSES)
readListPrecDefaultName Read2 = liftReadListPrec2DefaultValName
#endif

readListPrecName :: ReadClass -> Name
readListPrecName Read  = readListPrecValName
readListPrecName Read1 = liftReadListPrecValName
#if defined(NEW_FUNCTOR_CLASSES)
readListPrecName Read2 = liftReadListPrec2ValName
#endif

readListName :: ReadClass -> Name
readListName Read  = readListValName
#if defined(NEW_FUNCTOR_CLASSES)
readListName Read1 = liftReadListValName
readListName Read2 = liftReadList2ValName
#else
readListName Read1 = error "Text.Read.Deriving.Internal.readListName"
#endif

readsPrecOrListName :: Bool -- ^ readsListName if True, readsPrecName if False
                    -> ReadClass
                    -> Name
readsPrecOrListName False = readsPrecName
readsPrecOrListName True  = readListName

readPrecOrListName :: Bool -- ^ readListPrecName if True, readPrecName if False
                   -> ReadClass
                   -> Name
readPrecOrListName False = readPrecName
readPrecOrListName True  = readListPrecName

readsOrReadName :: Bool -- ^ readPrecOrListName if True, readsPrecOrListName if False
                -> Bool -- ^ read(s)List(Prec)Name if True, read(s)PrecName if False
                -> ReadClass
                -> Name
readsOrReadName False = readsPrecOrListName
readsOrReadName True  = readPrecOrListName

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

mkParser :: Int -> [Q Stmt] -> Q Exp -> Q Exp
mkParser p ss b = varE precValName `appE` integerE p `appE` mkDoStmts ss b

mkDoStmts :: [Q Stmt] -> Q Exp -> Q Exp
mkDoStmts ss b = doE (ss ++ [noBindS b])

resultExpr :: Name -> [Exp] -> Q Exp
resultExpr conName as = varE returnValName `appE` conApp
  where
    conApp :: Q Exp
    conApp = appsE $ conE conName : map return as

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

dataConStr :: ConstructorInfo -> String
dataConStr = nameBase . constructorName

readPrefixCon :: String -> [Q Stmt]
readPrefixCon conStr
  | isSym conStr = [readPunc "(", symbolPat conStr, readPunc ")"]
  | otherwise    = identHPat conStr

wrapReadS :: Bool -> Q Exp -> Q Exp
wrapReadS urp e = if urp then e
                         else varE readS_to_PrecValName `appE` e

shouldDefineReadPrec :: ReadClass -> ReadOptions -> Bool
shouldDefineReadPrec rClass opts = useReadPrec opts && baseCompatible
  where
    base4'10OrLater :: Bool
#if __GLASGOW_HASKELL__ >= 801
    base4'10OrLater = True
#else
    base4'10OrLater = False
#endif

    baseCompatible :: Bool
    baseCompatible = case rClass of
        Read  -> True
        Read1 -> base4'10OrLater
#if defined(NEW_FUNCTOR_CLASSES)
        Read2 -> base4'10OrLater
#endif
