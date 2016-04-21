{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
module Data.Functor.Deriving.Internal (
    -- * 'Foldable'
    deriveFoldable
  , makeFoldMap
  , makeFoldr
    -- * 'Functor'
  , deriveFunctor
  , makeFmap
    -- * 'Traversable'
  , deriveTraversable
  , makeTraverse
  ) where

import           Control.Monad (guard, unless, when, zipWithM)

import           Data.Deriving.Internal
import           Data.Either (rights)
#if MIN_VERSION_template_haskell(2,8,0) && !(MIN_VERSION_template_haskell(2,10,0))
import           Data.Foldable (foldr')
#endif
import           Data.List
import qualified Data.Map as Map (fromList, keys, lookup, size)
import           Data.Maybe

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr
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

-- -- | Generates a lambda expression which behaves like 'bifold' (without requiring a
-- -- 'Bifoldable' instance).
-- makeBifold :: Name -> Q Exp
-- makeBifold name = appsE [ makeBifoldMap name
--                         , varE idValName
--                         , varE idValName
--                         ]
--
-- -- | Generates a lambda expression which behaves like 'bifoldl' (without requiring a
-- -- 'Bifoldable' instance).
-- makeBifoldl :: Name -> Q Exp
-- makeBifoldl name = do
--   f <- newName "f"
--   g <- newName "g"
--   z <- newName "z"
--   t <- newName "t"
--   lamE [varP f, varP g, varP z, varP t] $
--     appsE [ varE appEndoValName
--           , appsE [ varE getDualValName
--                   , appsE [ makeBifoldMap name, foldFun f, foldFun g, varE t]
--                   ]
--           , varE z
--           ]
--   where
--     foldFun :: Name -> Q Exp
--     foldFun n = infixApp (conE dualDataName)
--                          (varE composeValName)
--                          (infixApp (conE endoDataName)
--                                    (varE composeValName)
--                                    (varE flipValName `appE` varE n)
--                          )
--
-- -- | Generates a lambda expression which behaves like 'bisequenceA' (without requiring a
-- -- 'Bitraversable' instance).
-- makeBisequenceA :: Name -> Q Exp
-- makeBisequenceA name = appsE [ makeBitraverse name
--                              , varE idValName
--                              , varE idValName
--                              ]
--
-- -- | Generates a lambda expression which behaves like 'bimapM' (without requiring a
-- -- 'Bitraversable' instance).
-- makeBimapM :: Name -> Q Exp
-- makeBimapM name = do
--   f <- newName "f"
--   g <- newName "g"
--   lamE [varP f, varP g] . infixApp (varE unwrapMonadValName) (varE composeValName) $
--                           appsE [makeBitraverse name, wrapMonadExp f, wrapMonadExp g]
--   where
--     wrapMonadExp :: Name -> Q Exp
--     wrapMonadExp n = infixApp (conE wrapMonadDataName) (varE composeValName) (varE n)
--
-- -- | Generates a lambda expression which behaves like 'bisequence' (without requiring a
-- -- 'Bitraversable' instance).
-- makeBisequence :: Name -> Q Exp
-- makeBisequence name = appsE [ makeBimapM name
--                             , varE idValName
--                             , varE idValName
--                             ]

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive a class instance declaration (depending on the FunctorClass argument's value).
deriveFunctorClass :: FunctorClass -> Name -> Q [Dec]
deriveFunctorClass fc name = withType name fromCons where
  fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q [Dec]
  fromCons name' ctxt tvbs cons mbTys = (:[]) `fmap` do
    (instanceCxt, instanceType)
        <- buildTypeInstance fc name' ctxt tvbs mbTys
    instanceD (return instanceCxt)
              (return instanceType)
              (functorFunDecs fc cons)

-- | Generates a declaration defining the primary function(s) corresponding to a
-- particular class (fmap for Functor, foldr and foldMap for Foldable, and
-- traverse for Traversable).
--
-- For why both foldr and foldMap are derived for Foldable, see Trac #7436.
functorFunDecs :: FunctorClass -> [Con] -> [Q Dec]
functorFunDecs fc cons = map makeFunD $ functorClassToFuns fc where
  makeFunD :: FunctorFun -> Q Dec
  makeFunD ff =
    funD (functorFunName ff)
         [ clause []
                  (normalB $ makeFunctorFunForCons ff cons)
                  []
         ]

-- | Generates a lambda expression which behaves like the FunctorFun argument.
makeFunctorFun :: FunctorFun -> Name -> Q Exp
makeFunctorFun ff name = withType name fromCons where
  fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q Exp
  fromCons name' ctxt tvbs cons mbTys =
    -- We force buildTypeInstance here since it performs some checks for whether
    -- or not the provided datatype can actually have fmap/foldr/traverse/etc.
    -- implemented for it, and produces errors if it can't.
    buildTypeInstance (functorFunToClass ff) name' ctxt tvbs mbTys
      `seq` makeFunctorFunForCons ff cons

-- | Generates a lambda expression for the given constructors.
-- All constructors must be from the same type.
makeFunctorFunForCons :: FunctorFun -> [Con] -> Q Exp
makeFunctorFunForCons ff cons = do
  argNames <- mapM newName $ catMaybes [ Just "f"
                                       , guard (ff == Foldr) >> Just "z"
                                       , Just "value"
                                       ]
  let mapFun:others = argNames
      z     = head others -- If we're deriving foldr, this will be well defined
                          -- and useful. Otherwise, it'll be ignored.
      value = last others
  lamE (map varP argNames)
      . appsE
      $ [ varE $ functorFunConstName ff
        , if null cons
             then appE (varE errorValName)
                       (stringE $ "Void " ++ nameBase (functorFunName ff))
             else caseE (varE value)
                        (map (makeFunctorFunForCon ff z mapFun) cons)
        ] ++ map varE argNames

-- | Generates a lambda expression for a single constructor.
makeFunctorFunForCon :: FunctorFun -> Name -> Name -> Con -> Q Match
makeFunctorFunForCon ff z mapFun con = do
  let conName = constructorName con
  (ts, tvMap) <- reifyConTys ff conName mapFun
  argNames    <- newNameList "_arg" $ length ts
  makeFunctorFunForArgs ff z tvMap conName ts argNames

-- | Generates a lambda expression for a single constructor's arguments.
makeFunctorFunForArgs :: FunctorFun
                      -> Name
                      -> TyVarMap
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
                     -> TyVarMap
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
                      -> TyVarMap
                      -> Name
                      -> Bool
                      -> Type
                      -> Q (Either Exp Exp)
makeFunctorFunForType ff tvMap conName covariant (VarT tyName) =
  case Map.lookup tyName tvMap of
    Just mapName -> fmap Right $
                        if covariant
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

      makeFunctorFunTuple :: Type -> Name -> Q (Either Exp Exp)
      makeFunctorFunTuple fieldTy fieldName =
        makeFunctorFunForType ff tvMap conName covariant fieldTy
          `appEitherE` varE fieldName

   in case tyCon of
     ArrowT
       | not (allowFunTys (functorFunToClass ff)) -> noFunctionsError conName
       | mentionsTyArgs, [argTy, resTy] <- tyArgs ->
         do x <- newName "x"
            b <- newName "b"
            fmap Right . lamE [varP x, varP b] $
              covBiFun covariant resTy `appE` (varE x `appE`
                (covBiFun (not covariant) argTy `appE` varE b))
         where
           covBiFun :: Bool -> Type -> Q Exp
           covBiFun cov = fmap fromEither . makeFunctorFunForType ff tvMap conName cov
     TupleT n
       | n > 0 && mentionsTyArgs -> do
         args <- mapM newName $ catMaybes [ Just "x"
                                          , guard (ff == Foldr) >> Just "z"
                                          ]
         xs <- newNameList "_tup" n

         let x = head args
             z = last args
         fmap Right $ lamE (map varP args) $ caseE (varE x)
              [ match (tupP $ map varP xs)
                      (normalB $ functorFunCombine ff
                                                   (tupleDataName n)
                                                   z
                                                   xs
                                                   (zipWithM makeFunctorFunTuple tyArgs xs)
                      )
                      []
              ]
     _ -> do
         itf <- isTyFamily tyCon
         if any (`mentionsName` tyVarNames) lhsArgs || (itf && mentionsTyArgs)
           then outOfPlaceTyVarError conName
           else if any (`mentionsName` tyVarNames) rhsArgs
                  then fmap Right . functorFunApp ff . appsE $
                         ( varE (functorFunName ff)
                         : map (fmap fromEither . makeFunctorFunForType ff tvMap conName covariant)
                                rhsArgs
                         )
                  else fmap Left $ functorFunTriv ff

-------------------------------------------------------------------------------
-- Template Haskell reifying and AST manipulation
-------------------------------------------------------------------------------

-- | Boilerplate for top level splices.
--
-- The given Name must meet one of two criteria:
--
-- 1. It must be the name of a type constructor of a plain data type or newtype.
-- 2. It must be the name of a data family instance or newtype instance constructor.
--
-- Any other value will result in an exception.
withType :: Name
         -> (Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q a)
         -> Q a
withType name f = do
  info <- reify name
  case info of
    TyConI dec ->
      case dec of
        DataD ctxt _ tvbs
#if MIN_VERSION_template_haskell(2,11,0)
              _
#endif
              cons _ -> f name ctxt tvbs cons Nothing
        NewtypeD ctxt _ tvbs
#if MIN_VERSION_template_haskell(2,11,0)
                 _
#endif
                 con _ -> f name ctxt tvbs [con] Nothing
        _ -> fail $ ns ++ "Unsupported type: " ++ show dec
#if MIN_VERSION_template_haskell(2,7,0)
# if MIN_VERSION_template_haskell(2,11,0)
    DataConI _ _ parentName   -> do
# else
    DataConI _ _ parentName _ -> do
# endif
      parentInfo <- reify parentName
      case parentInfo of
# if MIN_VERSION_template_haskell(2,11,0)
        FamilyI (DataFamilyD _ tvbs _) decs ->
# else
        FamilyI (FamilyD DataFam _ tvbs _) decs ->
# endif
          let instDec = flip find decs $ \dec -> case dec of
                DataInstD _ _ _
# if MIN_VERSION_template_haskell(2,11,0)
                          _
# endif
                          cons _ -> any ((name ==) . constructorName) cons
                NewtypeInstD _ _ _
# if MIN_VERSION_template_haskell(2,11,0)
                             _
# endif
                             con _ -> name == constructorName con
                _ -> error $ ns ++ "Must be a data or newtype instance."
           in case instDec of
                Just (DataInstD ctxt _ instTys
# if MIN_VERSION_template_haskell(2,11,0)
                                _
# endif
                                cons _)
                  -> f parentName ctxt tvbs cons $ Just instTys
                Just (NewtypeInstD ctxt _ instTys
# if MIN_VERSION_template_haskell(2,11,0)
                                   _
# endif
                                   con _)
                  -> f parentName ctxt tvbs [con] $ Just instTys
                _ -> fail $ ns ++
                  "Could not find data or newtype instance constructor."
        _ -> fail $ ns ++ "Data constructor " ++ show name ++
          " is not from a data family instance constructor."
# if MIN_VERSION_template_haskell(2,11,0)
    FamilyI DataFamilyD{} _ ->
# else
    FamilyI (FamilyD DataFam _ _ _) _ ->
# endif
      fail $ ns ++
        "Cannot use a data family name. Use a data family instance constructor instead."
    _ -> fail $ ns ++ "The name must be of a plain data type constructor, "
                    ++ "or a data family instance constructor."
#else
    DataConI{} -> dataConIError
    _          -> fail $ ns ++ "The name must be of a plain type constructor."
#endif
  where
    ns :: String
    ns = "Data.Bifunctor.TH.withType: "

-- | Deduces the instance context and head for an instance.
buildTypeInstance :: FunctorClass
                  -- ^ Functor, Foldable, or Traversable
                  -> Name
                  -- ^ The type constructor or data family name
                  -> Cxt
                  -- ^ The datatype context
                  -> [TyVarBndr]
                  -- ^ The type variables from the data type/data family declaration
                  -> Maybe [Type]
                  -- ^ 'Just' the types used to instantiate a data family instance,
                  -- or 'Nothing' if it's a plain data type
                  -> Q (Cxt, Type)
-- Plain data type/newtype case
buildTypeInstance fc tyConName dataCxt tvbs Nothing =
    let varTys :: [Type]
        varTys = map tvbToType tvbs
    in buildTypeInstanceFromTys fc tyConName dataCxt varTys False
-- Data family instance case
--
-- The CPP is present to work around a couple of annoying old GHC bugs.
-- See Note [Polykinded data families in Template Haskell]
buildTypeInstance fc parentName dataCxt tvbs (Just instTysAndKinds) = do
#if !(MIN_VERSION_template_haskell(2,8,0)) || MIN_VERSION_template_haskell(2,10,0)
    let instTys :: [Type]
        instTys = zipWith stealKindForType tvbs instTysAndKinds
#else
    let kindVarNames :: [Name]
        kindVarNames = nub $ concatMap (tyVarNamesOfType . tvbKind) tvbs

        numKindVars :: Int
        numKindVars = length kindVarNames

        givenKinds, givenKinds' :: [Kind]
        givenTys                :: [Type]
        (givenKinds, givenTys) = splitAt numKindVars instTysAndKinds
        givenKinds' = map sanitizeStars givenKinds

        -- A GHC 7.6-specific bug requires us to replace all occurrences of
        -- (ConT GHC.Prim.*) with StarT, or else Template Haskell will reject it.
        -- Luckily, (ConT GHC.Prim.*) only seems to occur in this one spot.
        sanitizeStars :: Kind -> Kind
        sanitizeStars = go
          where
            go :: Kind -> Kind
            go (AppT t1 t2)                 = AppT (go t1) (go t2)
            go (SigT t k)                   = SigT (go t) (go k)
            go (ConT n) | n == starKindName = StarT
            go t                            = t

    -- If we run this code with GHC 7.8, we might have to generate extra type
    -- variables to compensate for any type variables that Template Haskell
    -- eta-reduced away.
    -- See Note [Polykinded data families in Template Haskell]
    xTypeNames <- newNameList "tExtra" (length tvbs - length givenTys)

    let xTys   :: [Type]
        xTys = map VarT xTypeNames
        -- ^ Because these type variables were eta-reduced away, we can only
        --   determine their kind by using stealKindForType. Therefore, we mark
        --   them as VarT to ensure they will be given an explicit kind annotation
        --   (and so the kind inference machinery has the right information).

        substNamesWithKinds :: [(Name, Kind)] -> Type -> Type
        substNamesWithKinds nks t = foldr' (uncurry substNameWithKind) t nks

        -- The types from the data family instance might not have explicit kind
        -- annotations, which the kind machinery needs to work correctly. To
        -- compensate, we use stealKindForType to explicitly annotate any
        -- types without kind annotations.
        instTys :: [Type]
        instTys = map (substNamesWithKinds (zip kindVarNames givenKinds'))
                  -- ^ Note that due to a GHC 7.8-specific bug
                  --   (see Note [Polykinded data families in Template Haskell]),
                  --   there may be more kind variable names than there are kinds
                  --   to substitute. But this is OK! If a kind is eta-reduced, it
                  --   means that is was not instantiated to something more specific,
                  --   so we need not substitute it. Using stealKindForType will
                  --   grab the correct kind.
                $ zipWith stealKindForType tvbs (givenTys ++ xTys)
#endif
    buildTypeInstanceFromTys fc parentName dataCxt instTys True

-- For the given Types, generate an instance context and head. Coming up with
-- the instance type isn't as simple as dropping the last type, as you need to
-- be wary of kinds being instantiated with *.
-- See Note [Type inference in derived instances]
buildTypeInstanceFromTys :: FunctorClass
                         -- ^ Functor, Foldable, or Traversable
                         -> Name
                         -- ^ The type constructor or data family name
                         -> Cxt
                         -- ^ The datatype context
                         -> [Type]
                         -- ^ The types to instantiate the instance with
                         -> Bool
                         -- ^ True if it's a data family, False otherwise
                         -> Q (Cxt, Type)
buildTypeInstanceFromTys fc tyConName dataCxt varTysOrig isDataFamily = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM expandSyn varTysOrig

    let remainingLength :: Int
        remainingLength = length varTysOrig - 1

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    -- Check there are enough types to drop and that all of them are either of
    -- kind * or kind k (for some kind variable k). If not, throw an error.
    when (remainingLength < 0 || any (== NotKindStar) droppedStarKindStati) $
      derivingKindError fc tyConName

    let droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati

        -- Substitute kind * for any dropped kind variables
        varTysExpSubst :: [Type]
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

        remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

        -- All of the type variables mentioned in the dropped types
        -- (post-synonym expansion)
        droppedTyVarNames :: [Name]
        droppedTyVarNames = concatMap tyVarNamesOfType droppedTysExpSubst

    -- If any of the dropped types were polykinded, ensure that they are of kind *
    -- after substituting * for the dropped kind variables. If not, throw an error.
    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError fc tyConName

    let preds    :: [Maybe Pred]
        kvNames  :: [[Name]]
        kvNames' :: [Name]
        -- Derive instance constraints (and any kind variables which are specialized
        -- to * in those constraints)
        (preds, kvNames) = unzip $ map (deriveConstraint fc) remainingTysExpSubst
        kvNames' = concat kvNames

        -- Substitute the kind variables specialized in the constraints with *
        remainingTysExpSubst' :: [Type]
        remainingTysExpSubst' =
          map (substNamesWithKindStar kvNames') remainingTysExpSubst

        -- We now substitute all of the specialized-to-* kind variable names with
        -- *, but in the original types, not the synonym-expanded types. The reason
        -- we do this is a superficial one: we want the derived instance to resemble
        -- the datatype written in source code as closely as possible. For example,
        -- for the following data family instance:
        --
        --   data family Fam a
        --   newtype instance Fam String = Fam String
        --
        -- We'd want to generate the instance:
        --
        --   instance C (Fam String)
        --
        -- Not:
        --
        --   instance C (Fam [Char])
        remainingTysOrigSubst :: [Type]
        remainingTysOrigSubst =
          map (substNamesWithKindStar (union droppedKindVarNames kvNames'))
            $ take remainingLength varTysOrig

        remainingTysOrigSubst' :: [Type]
        -- See Note [Kind signatures in derived instances] for an explanation
        -- of the isDataFamily check.
        remainingTysOrigSubst' =
          if isDataFamily
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceCxt :: Cxt
        instanceCxt = catMaybes preds

        instanceType :: Type
        instanceType = AppT (ConT $ functorClassName fc)
                     $ applyTyCon tyConName remainingTysOrigSubst'

    -- If the datatype context mentions any of the dropped type variables,
    -- we can't derive an instance, so throw an error.
    when (any (`predMentionsName` droppedTyVarNames) dataCxt) $
      datatypeContextError tyConName instanceType
    -- Also ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst' droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceCxt, instanceType)

-- | Attempt to derive a constraint on a Type. If successful, return
-- Just the constraint and any kind variable names constrained to *.
-- Otherwise, return Nothing and the empty list.
--
-- See Note [Type inference in derived instances] for the heuristics used to
-- come up with constraints.
deriveConstraint :: FunctorClass -> Type -> (Maybe Pred, [Name])
deriveConstraint fc t
  | not (isTyVar t) = (Nothing, [])
  | otherwise = case hasKindVarChain 1 t of
      Just ns -> (Just (applyClass (functorClassName fc) tName), ns)
      Nothing -> (Nothing, [])
  where
    tName :: Name
    tName = varTToName t

{-
Note [Polykinded data families in Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to come up with the correct instance context and head for an instance, e.g.,

  instance C a => C (Data a) where ...

We need to know the exact types and kinds used to instantiate the instance. For
plain old datatypes, this is simple: every type must be a type variable, and
Template Haskell reliably tells us the type variables and their kinds.

Doing the same for data families proves to be much harder for three reasons:

1. On any version of Template Haskell, it may not tell you what an instantiated
   type's kind is. For instance, in the following data family instance:

     data family Fam (f :: * -> *) (a :: *)
     data instance Fam f a

   Then if we use TH's reify function, it would tell us the TyVarBndrs of the
   data family declaration are:

     [KindedTV f (AppT (AppT ArrowT StarT) StarT),KindedTV a StarT]

   and the instantiated types of the data family instance are:

     [VarT f1,VarT a1]

   We can't just pass [VarT f1,VarT a1] to buildTypeInstanceFromTys, since we
   have no way of knowing their kinds. Luckily, the TyVarBndrs tell us what the
   kind is in case an instantiated type isn't a SigT, so we use the stealKindForType
   function to ensure all of the instantiated types are SigTs before passing them
   to buildTypeInstanceFromTys.
2. On GHC 7.6 and 7.8, a bug is present in which Template Haskell lists all of
   the specified kinds of a data family instance efore any of the instantiated
   types. Fortunately, this is easy to deal with: you simply count the number of
   distinct kind variables in the data family declaration, take that many elements
   from the front of the  Types list of the data family instance, substitute the
   kind variables with their respective instantiated kinds (which you took earlier),
   and proceed as normal.
3. On GHC 7.8, an even uglier bug is present (GHC Trac #9692) in which Template
   Haskell might not even list all of the Types of a data family instance, since
   they are eta-reduced away! And yes, kinds can be eta-reduced too.

   The simplest workaround is to count how many instantiated types are missing from
   the list and generate extra type variables to use in their place. Luckily, we
   needn't worry much if its kind was eta-reduced away, since using stealKindForType
   will get it back.

Note [Kind signatures in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to put explicit kind signatures into the derived instances, e.g.,

  instance C a => C (Data (f :: * -> *)) where ...

But it is preferable to avoid this if possible. If we come up with an incorrect
kind signature (which is entirely possible, since our type inferencer is pretty
unsophisticated - see Note [Type inference in derived instances]), then GHC will
flat-out reject the instance, which is quite unfortunate.

Plain old datatypes have the advantage that you can avoid using any kind signatures
at all in their instances. This is because a datatype declaration uses all type
variables, so the types that we use in a derived instance uniquely determine their
kinds. As long as we plug in the right types, the kind inferencer can do the rest
of the work. For this reason, we use unSigT to remove all kind signatures before
splicing in the instance context and head.

Data family instances are trickier, since a data family can have two instances that
are distinguished by kind alone, e.g.,

  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)

If we dropped the kind signatures for C (Fam a), then GHC will have no way of
knowing which instance we are talking about. To avoid this scenario, we always
include explicit kind signatures in data family instances. There is a chance that
the inferred kind signatures will be incorrect, but if so, we can always fall back
on the make- functions.

Note [Type inference in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Type inference is can be tricky to get right, and we want to avoid recreating the
entirety of GHC's type inferencer in Template Haskell. For this reason, we will
probably never come up with derived instance contexts that are as accurate as
GHC's. But that doesn't mean we can't do anything! There are a couple of simple
things we can do to make instance contexts that work for 80% of use cases:

1. If one of the last type parameters is polykinded, then its kind will be
   specialized to * in the derived instance. We note what kind variable the type
   parameter had and substitute it with * in the other types as well. For example,
   imagine you had

     data Data (a :: k) (b :: k) (c :: k)

   Then you'd want to derived instance to be:

     instance C (Data (a :: *))

   Not:

     instance C (Data (a :: k))

2. We naïvely come up with instance constraints using the following criterion:

   (i)  If there's a type parameter n of kind k1 -> k2 (where k1/k2 are * or kind
        variables), then generate a Functor n constraint, and if k1/k2 are kind
        variables, then substitute k1/k2 with * elsewhere in the types. We must
        consider the case where they are kind variables because you might have a
        scenario like this:

          newtype Compose (f :: k2 -> *) (g :: k1 -> k2) (a :: k1)
            = Compose (f (g a))

        Which would have a derived Functor instance of:

          instance (Functor f, Functor g) => Functor (Compose f g) where ...
-}

-- Determines the types of a constructor's arguments as well as the last type
-- parameters (along with their map functions), expanding through any type synonyms.
-- The type parameters are determined on a constructor-by-constructor basis since
-- they may be refined to be particular types in a GADT.
reifyConTys :: FunctorFun
            -> Name
            -> Name
            -> Q ([Type], TyVarMap)
reifyConTys ff conName mapFun = do
    info          <- reify conName
    (ctxt, uncTy) <- case info of
        DataConI _ ty _
#if !(MIN_VERSION_template_haskell(2,11,0))
                 _
#endif
                 -> fmap uncurryTy (expandSyn ty)
        _ -> fail "Must be a data constructor"
    let (argTys, [resTy]) = splitAt (length uncTy - 1) uncTy
        unapResTy = unapplyTy resTy
        -- If one of the last type variables is refined to a particular type
        -- (i.e., not truly polymorphic), we mark it with Nothing and filter
        -- it out later, since we only apply map functions to arguments of
        -- a type that it (1) one of the last type variables, and (2)
        -- of a truly polymorphic type.
        mbTvNames = map varTToName_maybe $
                        drop (length unapResTy - 1) unapResTy
        tvMap = Map.fromList
                    . catMaybes -- Drop refined types
                    $ zipWith (\mbTvName sp ->
                                  fmap (\tvName -> (tvName, sp)) mbTvName)
                              mbTvNames [mapFun]
    if (any (`predMentionsName` Map.keys tvMap) ctxt
         || Map.size tvMap < 1)
         && not (allowExQuant (functorFunToClass ff))
       then existentialContextError conName
       else return (argTys, tvMap)

-------------------------------------------------------------------------------
-- Error messages
-------------------------------------------------------------------------------

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: FunctorClass -> Name -> Q a
derivingKindError fc tyConName = fail
  . showString "Cannot derive well-kinded instance of form ‘"
  . showString className
  . showChar ' '
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass "
  . showString className
  . showString " expects an argument of kind * -> *"
  $ ""
  where
    className :: String
    className = nameBase $ functorClassName fc

-- | The last type variable appeared in a contravariant position
-- when deriving Functor.
contravarianceError :: Name -> Q a
contravarianceError conName = fail
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must not use the last type variable in a function argument"
  $ ""

-- | A constructor has a function argument in a derived Foldable or Traversable
-- instance.
noFunctionsError :: Name -> Q a
noFunctionsError conName = fail
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must not contain function types"
  $ ""

-- | The data type has a DatatypeContext which mentions one of the eta-reduced
-- type variables.
datatypeContextError :: Name -> Type -> Q a
datatypeContextError dataName instanceType = fail
  . showString "Can't make a derived instance of ‘"
  . showString (pprint instanceType)
  . showString "‘:\n\tData type ‘"
  . showString (nameBase dataName)
  . showString "‘ must not have a class context involving the last type argument(s)"
  $ ""

-- | The data type has an existential constraint which mentions one of the
-- eta-reduced type variables.
existentialContextError :: Name -> Q a
existentialContextError conName = fail
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must be truly polymorphic in the last argument(s) of the data type"
  $ ""

-- | The data type mentions one of the n eta-reduced type variables in a place other
-- than the last nth positions of a data type in a constructor's field.
outOfPlaceTyVarError :: Name -> Q a
outOfPlaceTyVarError conName = fail
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must only use its last two type variable(s) within"
  . showString " the last two argument(s) of a data type"
  $ ""

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> Q a
etaReductionError instanceType = fail $
  "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
  ++ pprint instanceType

#if !(MIN_VERSION_template_haskell(2,7,0))
-- | Template Haskell didn't list all of a data family's instances upon reification
-- until template-haskell-2.7.0.0, which is necessary for a derived instance to work.
dataConIError :: Q a
dataConIError = fail
  . showString "Cannot use a data constructor."
  . showString "\n\t(Note: if you are trying to derive for a data family instance,"
  . showString "\n\tuse GHC >= 7.4 instead.)"
  $ ""
#endif

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which class is being derived.
data FunctorClass = Functor | Foldable | Traversable

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

functorClassName :: FunctorClass -> Name
functorClassName Functor     = functorTypeName
functorClassName Foldable    = foldableTypeName
functorClassName Traversable = traversableTypeName

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

allowExQuant :: FunctorClass -> Bool
allowExQuant Foldable = True
allowExQuant _        = False

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
        go (e:es) = foldl' (\e1 e2 -> InfixE (Just e1) (VarE apValName) (Just e2))
          (VarE fmapValName `AppE` conExp `AppE` e) es

    return . go . rights $ ess
