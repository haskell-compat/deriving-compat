{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module:      Data.Functor.Deriving.Internal
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

The machinery needed to derive 'Foldable', 'Functor', and 'Traversable' instances.

For more info on how deriving @Functor@ works, see
<https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor this GHC wiki page>.

Note: this is an internal module, and as such, the API presented here is not
guaranteed to be stable, even between minor releases of this library.
-}
module Data.Functor.Deriving.Internal (
      -- * 'Foldable'
      deriveFoldable
    , deriveFoldableOptions
    , makeFoldMap
    , makeFoldMapOptions
    , makeFoldr
    , makeFoldrOptions
    , makeFold
    , makeFoldOptions
    , makeFoldl
    , makeFoldlOptions
    , makeNull
    , makeNullOptions
      -- * 'Functor'
    , deriveFunctor
    , deriveFunctorOptions
    , makeFmap
    , makeFmapOptions
    , makeReplace
    , makeReplaceOptions
      -- * 'Traversable'
    , deriveTraversable
    , deriveTraversableOptions
    , makeTraverse
    , makeTraverseOptions
    , makeSequenceA
    , makeSequenceAOptions
    , makeMapM
    , makeMapMOptions
    , makeSequence
    , makeSequenceOptions
      -- * 'FFTOptions'
    , FFTOptions(..)
    , defaultFFTOptions
    ) where

import           Control.Monad (guard)

import           Data.Deriving.Internal
import           Data.List
import qualified Data.Map as Map ((!), keys, lookup, member, singleton)
import           Data.Maybe

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Datatype.TyVarBndr
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

-- | Options that further configure how the functions in "Data.Functor.Deriving"
-- should behave. (@FFT@ stands for 'Functor'/'Foldable'/'Traversable'.)
newtype FFTOptions = FFTOptions
  { fftEmptyCaseBehavior :: Bool
    -- ^ If 'True', derived instances for empty data types (i.e., ones with
    --   no data constructors) will use the @EmptyCase@ language extension.
    --   If 'False', derived instances will simply use 'seq' instead.
    --   (This has no effect on GHCs before 7.8, since @EmptyCase@ is only
    --   available in 7.8 or later.)
  } deriving (Eq, Ord, Read, Show)

-- | Conservative 'FFTOptions' that doesn't attempt to use @EmptyCase@ (to
-- prevent users from having to enable that extension at use sites.)
defaultFFTOptions :: FFTOptions
defaultFFTOptions = FFTOptions { fftEmptyCaseBehavior = False }

-- | Generates a 'Foldable' instance declaration for the given data type or data
-- family instance.
deriveFoldable :: Name -> Q [Dec]
deriveFoldable = deriveFoldableOptions defaultFFTOptions

-- | Like 'deriveFoldable', but takes an 'FFTOptions' argument.
deriveFoldableOptions :: FFTOptions -> Name -> Q [Dec]
deriveFoldableOptions = deriveFunctorClass Foldable

-- | Generates a lambda expression which behaves like 'foldMap' (without requiring a
-- 'Foldable' instance).
makeFoldMap :: Name -> Q Exp
makeFoldMap = makeFoldMapOptions defaultFFTOptions

-- | Like 'makeFoldMap', but takes an 'FFTOptions' argument.
makeFoldMapOptions :: FFTOptions -> Name -> Q Exp
makeFoldMapOptions = makeFunctorFun FoldMap

-- | Generates a lambda expression which behaves like 'null' (without requiring a
-- 'Foldable' instance).
makeNull :: Name -> Q Exp
makeNull = makeNullOptions defaultFFTOptions

-- | Like 'makeNull', but takes an 'FFTOptions' argument.
makeNullOptions :: FFTOptions -> Name -> Q Exp
makeNullOptions = makeFunctorFun Null

-- | Generates a lambda expression which behaves like 'foldr' (without requiring a
-- 'Foldable' instance).
makeFoldr :: Name -> Q Exp
makeFoldr = makeFoldrOptions defaultFFTOptions

-- | Like 'makeFoldr', but takes an 'FFTOptions' argument.
makeFoldrOptions :: FFTOptions -> Name -> Q Exp
makeFoldrOptions = makeFunctorFun Foldr

-- | Generates a lambda expression which behaves like 'fold' (without requiring a
-- 'Foldable' instance).
makeFold :: Name -> Q Exp
makeFold = makeFoldOptions defaultFFTOptions

-- | Like 'makeFold', but takes an 'FFTOptions' argument.
makeFoldOptions :: FFTOptions -> Name -> Q Exp
makeFoldOptions opts name = makeFoldMapOptions opts name `appE` varE idValName

-- | Generates a lambda expression which behaves like 'foldl' (without requiring a
-- 'Foldable' instance).
makeFoldl :: Name -> Q Exp
makeFoldl = makeFoldlOptions defaultFFTOptions

-- | Like 'makeFoldl', but takes an 'FFTOptions' argument.
makeFoldlOptions :: FFTOptions -> Name -> Q Exp
makeFoldlOptions opts name = do
  f <- newName "f"
  z <- newName "z"
  t <- newName "t"
  lamE [varP f, varP z, varP t] $
    appsE [ varE appEndoValName
          , appsE [ varE getDualValName
                  , appsE [ makeFoldMapOptions opts name, foldFun f, varE t]
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
deriveFunctor = deriveFunctorOptions defaultFFTOptions

-- | Like 'deriveFunctor', but takes an 'FFTOptions' argument.
deriveFunctorOptions :: FFTOptions -> Name -> Q [Dec]
deriveFunctorOptions = deriveFunctorClass Functor

-- | Generates a lambda expression which behaves like 'fmap' (without requiring a
-- 'Functor' instance).
makeFmap :: Name -> Q Exp
makeFmap = makeFmapOptions defaultFFTOptions

-- | Like 'makeFmap', but takes an 'FFTOptions' argument.
makeFmapOptions :: FFTOptions -> Name -> Q Exp
makeFmapOptions = makeFunctorFun Fmap

-- | Generates a lambda expression which behaves like ('<$') (without requiring a
-- 'Functor' instance).
makeReplace :: Name -> Q Exp
makeReplace = makeReplaceOptions defaultFFTOptions

-- | Like 'makeReplace', but takes an 'FFTOptions' argument.
makeReplaceOptions :: FFTOptions -> Name -> Q Exp
makeReplaceOptions = makeFunctorFun Replace

-- | Generates a 'Traversable' instance declaration for the given data type or data
-- family instance.
deriveTraversable :: Name -> Q [Dec]
deriveTraversable = deriveTraversableOptions defaultFFTOptions

-- | Like 'deriveTraverse', but takes an 'FFTOptions' argument.
deriveTraversableOptions :: FFTOptions -> Name -> Q [Dec]
deriveTraversableOptions = deriveFunctorClass Traversable

-- | Generates a lambda expression which behaves like 'traverse' (without requiring a
-- 'Traversable' instance).
makeTraverse :: Name -> Q Exp
makeTraverse = makeTraverseOptions defaultFFTOptions

-- | Like 'makeTraverse', but takes an 'FFTOptions' argument.
makeTraverseOptions :: FFTOptions -> Name -> Q Exp
makeTraverseOptions = makeFunctorFun Traverse

-- | Generates a lambda expression which behaves like 'sequenceA' (without requiring a
-- 'Traversable' instance).
makeSequenceA :: Name -> Q Exp
makeSequenceA = makeSequenceAOptions defaultFFTOptions

-- | Like 'makeSequenceA', but takes an 'FFTOptions' argument.
makeSequenceAOptions :: FFTOptions -> Name -> Q Exp
makeSequenceAOptions opts name = makeTraverseOptions opts name `appE` varE idValName

-- | Generates a lambda expression which behaves like 'mapM' (without requiring a
-- 'Traversable' instance).
makeMapM :: Name -> Q Exp
makeMapM = makeMapMOptions defaultFFTOptions

-- | Like 'makeMapM', but takes an 'FFTOptions' argument.
makeMapMOptions :: FFTOptions -> Name -> Q Exp
makeMapMOptions opts name = do
  f <- newName "f"
  lam1E (varP f) . infixApp (varE unwrapMonadValName) (varE composeValName) $
                   makeTraverseOptions opts name `appE` wrapMonadExp f
  where
    wrapMonadExp :: Name -> Q Exp
    wrapMonadExp n = infixApp (conE wrapMonadDataName) (varE composeValName) (varE n)

-- | Generates a lambda expression which behaves like 'sequence' (without requiring a
-- 'Traversable' instance).
makeSequence :: Name -> Q Exp
makeSequence = makeSequenceOptions defaultFFTOptions

-- | Like 'makeSequence', but takes an 'FFTOptions' argument.
makeSequenceOptions :: FFTOptions -> Name -> Q Exp
makeSequenceOptions opts name = makeMapMOptions opts name `appE` varE idValName

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive a class instance declaration (depending on the FunctorClass argument's value).
deriveFunctorClass :: FunctorClass -> FFTOptions -> Name -> Q [Dec]
deriveFunctorClass fc opts name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (instanceCxt, instanceType)
          <- buildTypeInstance fc parentName ctxt instTypes variant
      (:[]) `fmap` instanceD (return instanceCxt)
                             (return instanceType)
                             (functorFunDecs fc opts parentName instTypes cons)

-- | Generates a declaration defining the primary function(s) corresponding to a
-- particular class (fmap for Functor, foldr and foldMap for Foldable, and
-- traverse for Traversable).
--
-- For why both foldr and foldMap are derived for Foldable, see Trac #7436.
functorFunDecs
  :: FunctorClass -> FFTOptions -> Name -> [Type] -> [ConstructorInfo]
  -> [Q Dec]
functorFunDecs fc opts parentName instTypes cons =
  map makeFunD $ functorClassToFuns fc
  where
    makeFunD :: FunctorFun -> Q Dec
    makeFunD ff =
      funD (functorFunName ff)
           [ clause []
                    (normalB $ makeFunctorFunForCons ff opts parentName instTypes cons)
                    []
           ]

-- | Generates a lambda expression which behaves like the FunctorFun argument.
makeFunctorFun :: FunctorFun -> FFTOptions -> Name -> Q Exp
makeFunctorFun ff opts name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTypes
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      -- We force buildTypeInstance here since it performs some checks for whether
      -- or not the provided datatype can actually have fmap/foldr/traverse/etc.
      -- implemented for it, and produces errors if it can't.
      buildTypeInstance (functorFunToClass ff) parentName ctxt instTypes variant
        >> makeFunctorFunForCons ff opts parentName instTypes cons

-- | Generates a lambda expression for the given constructors.
-- All constructors must be from the same type.
makeFunctorFunForCons
  :: FunctorFun -> FFTOptions -> Name -> [Type] -> [ConstructorInfo]
  -> Q Exp
makeFunctorFunForCons ff opts _parentName instTypes cons = do
  mapFun <- newName "f"
  z      <- newName "z" -- Only used for deriving foldr
  value  <- newName "value"
  let argNames  = catMaybes [ guard (ff /= Null)  >> Just mapFun
                            , guard (ff == Foldr) >> Just z
                            , Just value
                            ]
      lastTyVar = varTToName $ last instTypes
      tvMap     = Map.singleton lastTyVar $ OneName mapFun
  lamE (map varP argNames)
      . appsE
      $ [ varE $ functorFunConstName ff
        , makeFun z value tvMap
        ] ++ map varE argNames
  where
    makeFun :: Name -> Name -> TyVarMap1 -> Q Exp
    makeFun z value tvMap = do
#if MIN_VERSION_template_haskell(2,9,0)
      roles <- reifyRoles _parentName
#endif
      case () of
        _

#if MIN_VERSION_template_haskell(2,9,0)
          | Just (_, PhantomR) <- unsnoc roles
         -> functorFunPhantom z value
#endif

          | null cons && fftEmptyCaseBehavior opts && ghc7'8OrLater
         -> functorFunEmptyCase ff z value

          | null cons
         -> functorFunNoCons ff z value

          | otherwise
         -> caseE (varE value)
                  (map (makeFunctorFunForCon ff z tvMap) cons)

#if MIN_VERSION_template_haskell(2,9,0)
    functorFunPhantom :: Name -> Name -> Q Exp
    functorFunPhantom z value =
        functorFunTrivial coerce
                          (varE pureValName `appE` coerce)
                          ff z
      where
        coerce :: Q Exp
        coerce = varE coerceValName `appE` varE value
#endif

-- | Generates a match for a single constructor.
makeFunctorFunForCon :: FunctorFun -> Name -> TyVarMap1 -> ConstructorInfo -> Q Match
makeFunctorFunForCon ff z tvMap
  con@(ConstructorInfo { constructorName    = conName
                       , constructorContext = ctxt }) = do
    checkExistentialContext (functorFunToClass ff) tvMap ctxt conName $
      case ff of
        Fmap     -> makeFmapMatch tvMap con
        Replace  -> makeReplaceMatch tvMap con
        Foldr    -> makeFoldrMatch z tvMap con
        FoldMap  -> makeFoldMapMatch tvMap con
        Null     -> makeNullMatch tvMap con
        Traverse -> makeTraverseMatch tvMap con

-- | Generates a match whose right-hand side implements @fmap@.
makeFmapMatch :: TyVarMap1 -> ConstructorInfo -> Q Match
makeFmapMatch tvMap con@(ConstructorInfo{constructorName = conName}) = do
  parts <- foldDataConArgs tvMap ft_fmap con
  match_for_con_functor conName parts
  where
    ft_fmap :: FFoldType (Exp -> Q Exp)
    ft_fmap = FT { ft_triv = return
                 , ft_var  = \v x -> case tvMap Map.! v of
                                       OneName f -> return $ VarE f `AppE` x
                 , ft_fun  = \g h x -> mkSimpleLam $ \b -> do
                     gg <- g b
                     h $ x `AppE` gg
                 , ft_tup  = mkSimpleTupleCase match_for_con_functor
                 , ft_ty_app = \argTy g x -> do
                     case varTToName_maybe argTy of
                       -- If the argument type is a bare occurrence of the
                       -- data type's last type variable, then we can
                       -- generate more efficient code.
                       -- This was inspired by GHC#17880.
                       Just argVar
                         |  Just (OneName f) <- Map.lookup argVar tvMap
                         -> return $ VarE fmapValName `AppE` VarE f `AppE` x
                       _ -> do gg <- mkSimpleLam g
                               return $ VarE fmapValName `AppE` gg `AppE` x
                 , ft_forall  = \_ g x -> g x
                 , ft_bad_app = \_ -> outOfPlaceTyVarError Functor conName
                 , ft_co_var  = \_ _ -> contravarianceError conName
                 }

-- | Generates a match whose right-hand side implements @(<$)@.
makeReplaceMatch :: TyVarMap1 -> ConstructorInfo -> Q Match
makeReplaceMatch tvMap con@(ConstructorInfo{constructorName = conName}) = do
  parts <- foldDataConArgs tvMap ft_replace con
  match_for_con_functor conName parts
  where
    ft_replace :: FFoldType (Exp -> Q Exp)
    ft_replace = FT { ft_triv = return
                    , ft_var  = \v _ -> case tvMap Map.! v of
                                          OneName z -> return $ VarE z
                    , ft_fun  = \g h x -> mkSimpleLam $ \b -> do
                        gg <- g b
                        h $ x `AppE` gg
                    , ft_tup  = mkSimpleTupleCase match_for_con_functor
                    , ft_ty_app = \argTy g x -> do
                        case varTToName_maybe argTy of
                          -- If the argument type is a bare occurrence of the
                          -- data type's last type variable, then we can
                          -- generate more efficient code.
                          -- This was inspired by GHC#17880.
                          Just argVar
                            |  Just (OneName z) <- Map.lookup argVar tvMap
                            -> return $ VarE replaceValName `AppE` VarE z `AppE` x
                          _ -> do gg <- mkSimpleLam g
                                  return $ VarE fmapValName `AppE` gg `AppE` x
                    , ft_forall  = \_ g x -> g x
                    , ft_bad_app = \_ -> outOfPlaceTyVarError Functor conName
                    , ft_co_var  = \_ _ -> contravarianceError conName
                    }

match_for_con_functor :: Name -> [Exp -> Q Exp] -> Q Match
match_for_con_functor = mkSimpleConMatch $ \conName' xs ->
  appsE (conE conName':xs) -- Con x1 x2 ..

-- | Generates a match whose right-hand side implements @foldr@.
makeFoldrMatch :: Name -> TyVarMap1 -> ConstructorInfo -> Q Match
makeFoldrMatch z tvMap con@(ConstructorInfo{constructorName = conName}) = do
  parts  <- foldDataConArgs tvMap ft_foldr con
  parts' <- sequence parts
  match_for_con (VarE z) conName parts'
  where
    -- The Bool is True if the type mentions the last type parameter, False
    -- otherwise. Later, match_for_con uses mkSimpleConMatch2 to filter out
    -- expressions that do not mention the last parameter by checking for False.
    ft_foldr :: FFoldType (Q (Bool, Exp))
    ft_foldr = FT { ft_triv = do lam <- mkSimpleLam2 $ \_ z' -> return z'
                                 return (False, lam)
                  , ft_var  = \v -> case tvMap Map.! v of
                                      OneName f -> return (True, VarE f)
                  , ft_tup  = \t gs -> do
                      gg  <- sequence gs
                      lam <- mkSimpleLam2 $ \x z' ->
                        mkSimpleTupleCase (match_for_con z') t gg x
                      return (True, lam)
                  , ft_ty_app = \_ g -> do
                      (b, gg) <- g
                      e <- mkSimpleLam2 $ \x z' -> return $
                           VarE foldrValName `AppE` gg `AppE` z' `AppE` x
                      return (b, e)
                  , ft_forall  = \_ g -> g
                  , ft_co_var  = \_ -> contravarianceError conName
                  , ft_fun     = \_ _ -> noFunctionsError conName
                  , ft_bad_app = outOfPlaceTyVarError Foldable conName
                  }

    match_for_con :: Exp -> Name -> [(Bool, Exp)] -> Q Match
    match_for_con zExp = mkSimpleConMatch2 $ \_ xs -> return $ mkFoldr xs
      where
        -- g1 v1 (g2 v2 (.. z))
        mkFoldr :: [Exp] -> Exp
        mkFoldr = foldr AppE zExp

-- | Generates a match whose right-hand side implements @foldMap@.
makeFoldMapMatch :: TyVarMap1 -> ConstructorInfo -> Q Match
makeFoldMapMatch tvMap con@(ConstructorInfo{constructorName = conName}) = do
  parts  <- foldDataConArgs tvMap ft_foldMap con
  parts' <- sequence parts
  match_for_con conName parts'
  where
    -- The Bool is True if the type mentions the last type parameter, False
    -- otherwise. Later, match_for_con uses mkSimpleConMatch2 to filter out
    -- expressions that do not mention the last parameter by checking for False.
    ft_foldMap :: FFoldType (Q (Bool, Exp))
    ft_foldMap = FT { ft_triv = do lam <- mkSimpleLam $ \_ -> return $ VarE memptyValName
                                   return (False, lam)
                    , ft_var  = \v -> case tvMap Map.! v of
                                        OneName f -> return (True, VarE f)
                    , ft_tup  = \t gs -> do
                        gg  <- sequence gs
                        lam <- mkSimpleLam $ mkSimpleTupleCase match_for_con t gg
                        return (True, lam)
                    , ft_ty_app = \_ g -> do
                        fmap (\(b, e) -> (b, VarE foldMapValName `AppE` e)) g
                    , ft_forall  = \_ g -> g
                    , ft_co_var  = \_ -> contravarianceError conName
                    , ft_fun     = \_ _ -> noFunctionsError conName
                    , ft_bad_app = outOfPlaceTyVarError Foldable conName
                    }

    match_for_con :: Name -> [(Bool, Exp)] -> Q Match
    match_for_con = mkSimpleConMatch2 $ \_ xs -> return $ mkFoldMap xs
      where
        -- mappend v1 (mappend v2 ..)
        mkFoldMap :: [Exp] -> Exp
        mkFoldMap [] = VarE memptyValName
        mkFoldMap es = foldr1 (AppE . AppE (VarE mappendValName)) es

-- | Generates a match whose right-hand side implements @null@.
makeNullMatch :: TyVarMap1 -> ConstructorInfo -> Q Match
makeNullMatch tvMap con@(ConstructorInfo{constructorName = conName}) = do
  parts  <- foldDataConArgs tvMap ft_null con
  parts' <- sequence parts
  case convert parts' of
    Nothing -> return $ Match (conWildPat con) (NormalB $ ConE falseDataName) []
    Just cp -> match_for_con conName cp
  where
    ft_null :: FFoldType (Q (NullM Exp))
    ft_null = FT { ft_triv = return $ IsNull $ ConE trueDataName
                 , ft_var  = \_ -> return NotNull
                 , ft_tup = \t g -> do
                     gg <- sequence g
                     case convert gg of
                       Nothing  -> return NotNull
                       Just ggg ->
                         fmap NullM $ mkSimpleLam
                                    $ mkSimpleTupleCase match_for_con t ggg
                 , ft_ty_app = \_ g -> flip fmap g $ \nestedResult ->
                     case nestedResult of
                       -- If e definitely contains the parameter, then we can
                       -- test if (G e) contains it by simply checking if (G e)
                       -- is null
                       NotNull -> NullM $ VarE nullValName
                       -- This case is unreachable--it will actually be caught
                       -- by ft_triv
                       r@IsNull{} -> r
                       -- The general case uses (all null), (all (all null)),
                       -- etc.
                       NullM nestedTest -> NullM $
                                           VarE allValName `AppE` nestedTest
                 , ft_forall = \_ g -> g
                 , ft_co_var  = \_ -> contravarianceError conName
                 , ft_fun     = \_ _ -> noFunctionsError conName
                 , ft_bad_app = outOfPlaceTyVarError Foldable conName
                 }

    match_for_con :: Name -> [(Bool, Exp)] -> Q Match
    match_for_con = mkSimpleConMatch2 $ \_ xs -> return $ mkNull xs
      where
        -- v1 && v2 && ..
        mkNull :: [Exp] -> Exp
        mkNull [] = ConE trueDataName
        mkNull xs = foldr1 (\x y -> VarE andValName `AppE` x `AppE` y) xs

-- Given a list of NullM results, produce Nothing if any of them is NotNull,
-- and otherwise produce a list of (Bool, a) with True entries representing
-- unknowns and False entries representing things that are definitely null.
convert :: [NullM a] -> Maybe [(Bool, a)]
convert = mapM go where
  go (IsNull a) = Just (False, a)
  go NotNull    = Nothing
  go (NullM a)  = Just (True, a)

data NullM a =
    IsNull a -- Definitely null
  | NotNull  -- Definitely not null
  | NullM a  -- Unknown

-- | Generates a match whose right-hand side implements @traverse@.
makeTraverseMatch :: TyVarMap1 -> ConstructorInfo -> Q Match
makeTraverseMatch tvMap con@(ConstructorInfo{constructorName = conName}) = do
  parts  <- foldDataConArgs tvMap ft_trav con
  parts' <- sequence parts
  match_for_con conName parts'
  where
    -- The Bool is True if the type mentions the last type parameter, False
    -- otherwise. Later, match_for_con uses mkSimpleConMatch2 to filter out
    -- expressions that do not mention the last parameter by checking for False.
    ft_trav :: FFoldType (Q (Bool, Exp))
    ft_trav = FT { -- See Note [ft_triv for Bifoldable and Bitraversable]
                   ft_triv = return (False, VarE pureValName)
                 , ft_var  = \v -> case tvMap Map.! v of
                                     OneName f -> return (True, VarE f)
                 , ft_tup  = \t gs -> do
                     gg  <- sequence gs
                     lam <- mkSimpleLam $ mkSimpleTupleCase match_for_con t gg
                     return (True, lam)
                 , ft_ty_app = \_ g ->
                     fmap (\(b, e) -> (b, VarE traverseValName `AppE` e)) g
                 , ft_forall  = \_ g -> g
                 , ft_co_var  = \_ -> contravarianceError conName
                 , ft_fun     = \_ _ -> noFunctionsError conName
                 , ft_bad_app = outOfPlaceTyVarError Traversable conName
                 }

    -- Con a1 a2 ... -> liftA2 (\b1 b2 ... -> Con b1 b2 ...) (g1 a1)
    --                    (g2 a2) <*> ...
    match_for_con :: Name -> [(Bool, Exp)] -> Q Match
    match_for_con = mkSimpleConMatch2 $ \conExp xs -> return $ mkApCon conExp xs
      where
        -- liftA2 (\b1 b2 ... -> Con b1 b2 ...) x1 x2 <*> ..
        mkApCon :: Exp -> [Exp] -> Exp
        mkApCon conExp []  = VarE pureValName `AppE` conExp
        mkApCon conExp [e] = VarE fmapValName `AppE` conExp `AppE` e
        mkApCon conExp (e1:e2:es) = foldl' appAp
          (VarE liftA2ValName `AppE` conExp `AppE` e1 `AppE` e2) es
          where appAp se1 se2 = InfixE (Just se1) (VarE apValName) (Just se2)

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
data FunctorFun
  = Fmap
  | Replace -- (<$)
  | Foldr
  | FoldMap
  | Null
  | Traverse
  deriving Eq

instance Show FunctorFun where
    showsPrec _ Fmap     = showString "fmap"
    showsPrec _ Replace  = showString "(<$)"
    showsPrec _ Foldr    = showString "foldr"
    showsPrec _ FoldMap  = showString "foldMap"
    showsPrec _ Null     = showString "null"
    showsPrec _ Traverse = showString "traverse"

functorFunConstName :: FunctorFun -> Name
functorFunConstName Fmap     = fmapConstValName
functorFunConstName Replace  = replaceConstValName
functorFunConstName Foldr    = foldrConstValName
functorFunConstName FoldMap  = foldMapConstValName
functorFunConstName Null     = nullConstValName
functorFunConstName Traverse = traverseConstValName

functorFunName :: FunctorFun -> Name
functorFunName Fmap     = fmapValName
functorFunName Replace  = replaceValName
functorFunName Foldr    = foldrValName
functorFunName FoldMap  = foldMapValName
functorFunName Null     = nullValName
functorFunName Traverse = traverseValName

functorClassToFuns :: FunctorClass -> [FunctorFun]
functorClassToFuns Functor     = [ Fmap, Replace ]
functorClassToFuns Foldable    = [ Foldr, FoldMap
#if MIN_VERSION_base(4,8,0)
                                 , Null
#endif
                                 ]
functorClassToFuns Traversable = [ Traverse ]

functorFunToClass :: FunctorFun -> FunctorClass
functorFunToClass Fmap     = Functor
functorFunToClass Replace  = Functor
functorFunToClass Foldr    = Foldable
functorFunToClass FoldMap  = Foldable
functorFunToClass Null     = Foldable
functorFunToClass Traverse = Traversable

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

functorFunEmptyCase :: FunctorFun -> Name -> Name -> Q Exp
functorFunEmptyCase ff z value =
    functorFunTrivial emptyCase
                      (varE pureValName `appE` emptyCase)
                      ff z
  where
    emptyCase :: Q Exp
    emptyCase = caseE (varE value) []

functorFunNoCons :: FunctorFun -> Name -> Name -> Q Exp
functorFunNoCons ff z value =
    functorFunTrivial seqAndError
                      (varE pureValName `appE` seqAndError)
                      ff z
  where
    seqAndError :: Q Exp
    seqAndError = appE (varE seqValName) (varE value) `appE`
                  appE (varE errorValName)
                       (stringE $ "Void " ++ nameBase (functorFunName ff))

functorFunTrivial :: Q Exp -> Q Exp -> FunctorFun -> Name -> Q Exp
functorFunTrivial fmapE traverseE ff z = go ff
  where
    go :: FunctorFun -> Q Exp
    go Fmap     = fmapE
    go Replace  = fmapE
    go Foldr    = varE z
    go FoldMap  = varE memptyValName
    go Null     = conE trueDataName
    go Traverse = traverseE

conWildPat :: ConstructorInfo -> Pat
conWildPat (ConstructorInfo { constructorName = conName
                            , constructorFields = ts }) =
  ConP conName $ replicate (length ts) WildP

-------------------------------------------------------------------------------
-- Generic traversal for functor-like deriving
-------------------------------------------------------------------------------

-- Much of the code below is cargo-culted from the TcGenFunctor module in GHC.

data FFoldType a      -- Describes how to fold over a Type in a functor like way
   = FT { ft_triv    :: a
          -- ^ Does not contain variable
        , ft_var     :: Name -> a
          -- ^ The variable itself
        , ft_co_var  :: Name -> a
          -- ^ The variable itself, contravariantly
        , ft_fun     :: a -> a -> a
          -- ^ Function type
        , ft_tup     :: TupleSort -> [a] -> a
          -- ^ Tuple type. The @[a]@ is the result of folding over the
          --   arguments of the tuple.
        , ft_ty_app  :: Type -> a -> a
          -- ^ Type app, variable only in last argument. The 'Type' is the
          --   @arg_ty@ in @fun_ty arg_ty@.
        , ft_bad_app :: a
          -- ^ Type app, variable other than in last argument
        , ft_forall  :: [TyVarBndrSpec] -> a -> a
          -- ^ Forall type
     }

-- Note that in GHC, this function is pure. It must be monadic here since we:
--
-- (1) Expand type synonyms
-- (2) Detect type family applications
--
-- Which require reification in Template Haskell, but are pure in Core.
functorLikeTraverse :: forall a.
                       TyVarMap1   -- ^ Variable to look for
                    -> FFoldType a -- ^ How to fold
                    -> Type        -- ^ Type to process
                    -> Q a
functorLikeTraverse tvMap (FT { ft_triv = caseTrivial,     ft_var = caseVar
                              , ft_co_var = caseCoVar,     ft_fun = caseFun
                              , ft_tup = caseTuple,        ft_ty_app = caseTyApp
                              , ft_bad_app = caseWrongArg, ft_forall = caseForAll })
                    ty
  = do ty' <- resolveTypeSynonyms ty
       (res, _) <- go False ty'
       return res
  where
    go :: Bool        -- Covariant or contravariant context
       -> Type
       -> Q (a, Bool) -- (result of type a, does type contain var)
    go co t@AppT{}
      | (ArrowT, [funArg, funRes]) <- unapplyTy t
      = do (funArgR, funArgC) <- go (not co) funArg
           (funResR, funResC) <- go      co  funRes
           if funArgC || funResC
              then return (caseFun funArgR funResR, True)
              else trivial
    go co t@AppT{} = do
      let (f, args) = unapplyTy t
      (_,   fc)  <- go co f
      (xrs, xcs) <- fmap unzip $ mapM (go co) args
      let tuple :: TupleSort -> Q (a, Bool)
          tuple tupSort = return (caseTuple tupSort xrs, True)

          wrongArg :: Q (a, Bool)
          wrongArg = return (caseWrongArg, True)

      case () of
        _ |  not (or xcs)
          -> trivial -- Variable does not occur
          -- At this point we know that xrs, xcs is not empty,
          -- and at least one xr is True
          |  TupleT len <- f
          -> tuple $ Boxed len
#if MIN_VERSION_template_haskell(2,6,0)
          |  UnboxedTupleT len <- f
          -> tuple $ Unboxed len
#endif
          |  fc || or (init xcs)
          -> wrongArg                    -- T (..var..)    ty
          |  otherwise                   -- T (..no var..) ty
          -> do itf <- isInTypeFamilyApp tyVarNames f args
                if itf -- We can't decompose type families, so
                       -- error if we encounter one here.
                   then wrongArg
                   else return (caseTyApp (last args) (last xrs), True)
    go co (SigT t k) = do
      (_, kc) <- go_kind co k
      if kc
         then return (caseWrongArg, True)
         else go co t
    go co (VarT v)
      | Map.member v tvMap
      = return (if co then caseCoVar v else caseVar v, True)
      | otherwise
      = trivial
    go co (ForallT tvbs _ t) = do
      (tr, tc) <- go co t
      let tvbNames = map tvName tvbs
      if not tc || any (`elem` tvbNames) tyVarNames
         then trivial
         else return (caseForAll tvbs tr, True)
    go _ _ = trivial

    go_kind :: Bool
            -> Kind
            -> Q (a, Bool)
#if MIN_VERSION_template_haskell(2,9,0)
    go_kind = go
#else
    go_kind _ _ = trivial
#endif

    trivial :: Q (a, Bool)
    trivial = return (caseTrivial, False)

    tyVarNames :: [Name]
    tyVarNames = Map.keys tvMap

-- Fold over the arguments of a data constructor in a Functor-like way.
foldDataConArgs :: forall a. TyVarMap1 -> FFoldType a -> ConstructorInfo -> Q [a]
foldDataConArgs tvMap ft con = do
  fieldTys <- mapM resolveTypeSynonyms $ constructorFields con
  mapM foldArg fieldTys
  where
    foldArg :: Type -> Q a
    foldArg = functorLikeTraverse tvMap ft

-- Make a 'LamE' using a fresh variable.
mkSimpleLam :: (Exp -> Q Exp) -> Q Exp
mkSimpleLam lam = do
  n <- newName "n"
  body <- lam (VarE n)
  return $ LamE [VarP n] body

-- Make a 'LamE' using two fresh variables.
mkSimpleLam2 :: (Exp -> Exp -> Q Exp) -> Q Exp
mkSimpleLam2 lam = do
  n1 <- newName "n1"
  n2 <- newName "n2"
  body <- lam (VarE n1) (VarE n2)
  return $ LamE [VarP n1, VarP n2] body

-- "Con a1 a2 a3 -> fold [x1 a1, x2 a2, x3 a3]"
--
-- @mkSimpleConMatch fold conName insides@ produces a match clause in
-- which the LHS pattern-matches on @extraPats@, followed by a match on the
-- constructor @conName@ and its arguments. The RHS folds (with @fold@) over
-- @conName@ and its arguments, applying an expression (from @insides@) to each
-- of the respective arguments of @conName@.
mkSimpleConMatch :: (Name -> [a] -> Q Exp)
                 -> Name
                 -> [Exp -> a]
                 -> Q Match
mkSimpleConMatch fold conName insides = do
  varsNeeded <- newNameList "_arg" $ length insides
  let pat = ConP conName (map VarP varsNeeded)
  rhs <- fold conName (zipWith (\i v -> i $ VarE v) insides varsNeeded)
  return $ Match pat (NormalB rhs) []

-- "Con a1 a2 a3 -> fmap (\b2 -> Con a1 b2 a3) (traverse f a2)"
--
-- @mkSimpleConMatch2 fold conName insides@ behaves very similarly to
-- 'mkSimpleConMatch', with two key differences:
--
-- 1. @insides@ is a @[(Bool, Exp)]@ instead of a @[Exp]@. This is because it
--    filters out the expressions corresponding to arguments whose types do not
--    mention the last type variable in a derived 'Foldable' or 'Traversable'
--    instance (i.e., those elements of @insides@ containing @False@).
--
-- 2. @fold@ takes an expression as its first argument instead of a
--    constructor name. This is because it uses a specialized
--    constructor function expression that only takes as many parameters as
--    there are argument types that mention the last type variable.
mkSimpleConMatch2 :: (Exp -> [Exp] -> Q Exp)
                  -> Name
                  -> [(Bool, Exp)]
                  -> Q Match
mkSimpleConMatch2 fold conName insides = do
  varsNeeded <- newNameList "_arg" lengthInsides
  let pat = ConP conName (map VarP varsNeeded)
      -- Make sure to zip BEFORE invoking catMaybes. We want the variable
      -- indicies in each expression to match up with the argument indices
      -- in conExpr (defined below).
      exps = catMaybes $ zipWith (\(m, i) v -> if m then Just (i `AppE` VarE v)
                                                    else Nothing)
                                 insides varsNeeded
      -- An element of argTysTyVarInfo is True if the constructor argument
      -- with the same index has a type which mentions the last type
      -- variable.
      argTysTyVarInfo = map (\(m, _) -> m) insides
      (asWithTyVar, asWithoutTyVar) = partitionByList argTysTyVarInfo varsNeeded

      conExpQ
        | null asWithTyVar = appsE (conE conName:map varE asWithoutTyVar)
        | otherwise = do
            bs <- newNameList "b" lengthInsides
            let bs'  = filterByList  argTysTyVarInfo bs
                vars = filterByLists argTysTyVarInfo
                                     (map varE bs) (map varE varsNeeded)
            lamE (map varP bs') (appsE (conE conName:vars))

  conExp <- conExpQ
  rhs <- fold conExp exps
  return $ Match pat (NormalB rhs) []
  where
    lengthInsides = length insides

-- Indicates whether a tuple is boxed or unboxed, as well as its number of
-- arguments. For instance, (a, b) corresponds to @Boxed 2@, and (# a, b, c #)
-- corresponds to @Unboxed 3@.
data TupleSort
  = Boxed   Int
#if MIN_VERSION_template_haskell(2,6,0)
  | Unboxed Int
#endif

-- "case x of (a1,a2,a3) -> fold [x1 a1, x2 a2, x3 a3]"
mkSimpleTupleCase :: (Name -> [a] -> Q Match)
                  -> TupleSort -> [a] -> Exp -> Q Exp
mkSimpleTupleCase matchForCon tupSort insides x = do
  let tupDataName = case tupSort of
                      Boxed   len -> tupleDataName len
#if MIN_VERSION_template_haskell(2,6,0)
                      Unboxed len -> unboxedTupleDataName len
#endif
  m <- matchForCon tupDataName insides
  return $ CaseE x [m]
