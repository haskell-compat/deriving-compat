{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}

#if !(MIN_VERSION_base(4,9,0))
# if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
# else
{-# LANGUAGE TemplateHaskell #-}
# endif
#endif

{-|
Module:      Data.Deriving.Internal
Copyright:   (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Template Haskell-related utilities.
-}
module Data.Deriving.Internal where

import           Control.Monad (liftM, when, unless)

import           Data.Foldable (foldr')
#if !(MIN_VERSION_base(4,9,0))
import           Data.Functor.Classes (Eq1(..), Ord1(..), Read1(..), Show1(..))
# if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
import           Data.Functor.Classes (Eq2(..), Ord2(..), Read2(..), Show2(..))
# endif
#endif
import           Data.List
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Set (Set)

import           Text.ParserCombinators.ReadPrec (ReadPrec)

#if !(MIN_VERSION_base(4,7,0))
import           GHC.Read (lexP)

import           Text.Read (pfail)
import           Text.Read.Lex (Lexeme)
#endif

#if MIN_VERSION_ghc_prim(0,3,1)
import           GHC.Prim (Int#, tagToEnum#)
#endif

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr (pprint)
import           Language.Haskell.TH.Syntax

-- Ensure, beyond a shadow of a doubt, that the instances are in-scope
import           Data.Functor ()
import           Data.Functor.Classes ()
import           Data.Foldable ()
import           Data.Traversable ()

#ifndef CURRENT_PACKAGE_KEY
import           Data.Version (showVersion)
import           Paths_deriving_compat (version)
#endif

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

-- | Expands all type synonyms in a type. Written by Dan Rosén in the
-- @genifunctors@ package (licensed under BSD3).
expandSyn :: Type -> Q Type
expandSyn (ForallT tvs ctx t) = fmap (ForallT tvs ctx) $ expandSyn t
expandSyn t@AppT{}            = expandSynApp t []
expandSyn t@ConT{}            = expandSynApp t []
expandSyn (SigT t k)          = do t' <- expandSyn t
                                   k' <- expandSynKind k
                                   return (SigT t' k')
expandSyn t                   = return t

expandSynKind :: Kind -> Q Kind
#if MIN_VERSION_template_haskell(2,8,0)
expandSynKind = expandSyn
#else
expandSynKind = return -- There are no kind synonyms to deal with
#endif

expandSynApp :: Type -> [Type] -> Q Type
expandSynApp (AppT t1 t2) ts = do
    t2' <- expandSyn t2
    expandSynApp t1 (t2':ts)
expandSynApp (ConT n) ts | nameBase n == "[]" = return $ foldl' AppT ListT ts
expandSynApp t@(ConT n) ts = do
    info <- reify n
    case info of
        TyConI (TySynD _ tvs rhs) ->
            let (ts', ts'') = splitAt (length tvs) ts
                subs = mkSubst tvs ts'
                rhs' = substType subs rhs
             in expandSynApp rhs' ts''
        _ -> return $ foldl' AppT t ts
expandSynApp t ts = do
    t' <- expandSyn t
    return $ foldl' AppT t' ts

type TypeSubst = Map Name Type
type KindSubst = Map Name Kind

mkSubst :: [TyVarBndr] -> [Type] -> TypeSubst
mkSubst vs ts =
   let vs' = map un vs
       un (PlainTV v)    = v
       un (KindedTV v _) = v
   in Map.fromList $ zip vs' ts

substType :: TypeSubst -> Type -> Type
substType subs (ForallT v c t) = ForallT v c $ substType subs t
substType subs t@(VarT n)      = Map.findWithDefault t n subs
substType subs (AppT t1 t2)    = AppT (substType subs t1) (substType subs t2)
substType subs (SigT t k)      = SigT (substType subs t)
#if MIN_VERSION_template_haskell(2,8,0)
                                      (substType subs k)
#else
                                      k
#endif
substType _ t                  = t

substKind :: KindSubst -> Type -> Type
#if MIN_VERSION_template_haskell(2,8,0)
substKind = substType
#else
substKind _ = id -- There are no kind variables!
#endif

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = substKind (Map.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (flip substNameWithKind starK) t ns

-------------------------------------------------------------------------------
-- Type-specialized const functions
-------------------------------------------------------------------------------

fmapConst :: f b -> (a -> b) -> f a -> f b
fmapConst x _ _ = x
{-# INLINE fmapConst #-}

foldrConst :: b -> (a -> b -> b) -> b -> t a -> b
foldrConst x _ _ _ = x
{-# INLINE foldrConst #-}

foldMapConst :: m -> (a -> m) -> t a -> m
foldMapConst x _ _ = x
{-# INLINE foldMapConst #-}

traverseConst :: f (t b) -> (a -> f b) -> t a -> f (t b)
traverseConst x _ _ = x
{-# INLINE traverseConst #-}

eqConst :: Bool
        -> a -> a -> Bool
eqConst x _ _ = x
{-# INLINE eqConst #-}

eq1Const :: Bool
         -> f a -> f a-> Bool
eq1Const x _ _ = x
{-# INLINE eq1Const #-}

liftEqConst :: Bool
            -> (a -> b -> Bool) -> f a -> f b -> Bool
liftEqConst x _ _ _ = x
{-# INLINE liftEqConst #-}

liftEq2Const :: Bool
             -> (a -> b -> Bool) -> (c -> d -> Bool)
             -> f a c -> f b d -> Bool
liftEq2Const x _ _ _ _ = x
{-# INLINE liftEq2Const #-}

compareConst :: Ordering -> a -> a -> Ordering
compareConst x _ _ = x
{-# INLINE compareConst #-}

ltConst :: Bool -> a -> a -> Bool
ltConst x _ _ = x
{-# INLINE ltConst #-}

compare1Const :: Ordering -> f a -> f a -> Ordering
compare1Const x _ _ = x
{-# INLINE compare1Const #-}

liftCompareConst :: Ordering
                 -> (a -> b -> Ordering) -> f a -> f b -> Ordering
liftCompareConst x _ _ _ = x
{-# INLINE liftCompareConst #-}

liftCompare2Const :: Ordering
                  -> (a -> b -> Ordering) -> (c -> d -> Ordering)
                  -> f a c -> f b d -> Ordering
liftCompare2Const x _ _ _ _ = x
{-# INLINE liftCompare2Const #-}

readsPrecConst :: ReadS a -> Int -> ReadS a
readsPrecConst x _ = x
{-# INLINE readsPrecConst #-}

-- This isn't really necessary, but it makes for an easier implementation
readPrecConst :: ReadPrec a -> ReadPrec a
readPrecConst x = x
{-# INLINE readPrecConst #-}

readsPrec1Const :: ReadS (f a) -> Int -> ReadS (f a)
readsPrec1Const x _ = x
{-# INLINE readsPrec1Const #-}

liftReadsPrecConst :: ReadS (f a)
                   -> (Int -> ReadS a) -> ReadS [a]
                   -> Int -> ReadS (f a)
liftReadsPrecConst x _ _ _ = x
{-# INLINE liftReadsPrecConst #-}

liftReadPrecConst :: ReadPrec (f a)
                  -> ReadPrec a -> ReadPrec [a]
                  -> ReadPrec (f a)
liftReadPrecConst x _ _ = x
{-# INLINE liftReadPrecConst #-}

liftReadsPrec2Const :: ReadS (f a b)
                    -> (Int -> ReadS a) -> ReadS [a]
                    -> (Int -> ReadS b) -> ReadS [b]
                    -> Int -> ReadS (f a b)
liftReadsPrec2Const x _ _ _ _ _ = x
{-# INLINE liftReadsPrec2Const #-}

liftReadPrec2Const :: ReadPrec (f a b)
                   -> ReadPrec a -> ReadPrec [a]
                   -> ReadPrec b -> ReadPrec [b]
                   -> ReadPrec (f a b)
liftReadPrec2Const x _ _ _ _ = x
{-# INLINE liftReadPrec2Const #-}

showsPrecConst :: ShowS
               -> Int -> a -> ShowS
showsPrecConst x _ _ = x
{-# INLINE showsPrecConst #-}

showsPrec1Const :: ShowS
                -> Int -> f a -> ShowS
showsPrec1Const x _ _ = x
{-# INLINE showsPrec1Const #-}

liftShowsPrecConst :: ShowS
                   -> (Int -> a -> ShowS) -> ([a] -> ShowS)
                   -> Int -> f a -> ShowS
liftShowsPrecConst x _ _ _ _ = x
{-# INLINE liftShowsPrecConst #-}

liftShowsPrec2Const :: ShowS
                    -> (Int -> a -> ShowS) -> ([a] -> ShowS)
                    -> (Int -> b -> ShowS) -> ([b] -> ShowS)
                    -> Int -> f a b -> ShowS
liftShowsPrec2Const x _ _ _ _ _ _ = x
{-# INLINE liftShowsPrec2Const #-}

-------------------------------------------------------------------------------
-- StarKindStatus
-------------------------------------------------------------------------------

-- | Whether a type is not of kind *, is of kind *, or is a kind variable.
data StarKindStatus = NotKindStar
                    | KindStar
                    | IsKindVar Name
  deriving Eq

-- | Does a Type have kind * or k (for some kind variable k)?
canRealizeKindStar :: Type -> StarKindStatus
canRealizeKindStar t
  | hasKindStar t = KindStar
  | otherwise = case t of
#if MIN_VERSION_template_haskell(2,8,0)
                     SigT _ (VarT k) -> IsKindVar k
#endif
                     _               -> NotKindStar

-- | Returns 'Just' the kind variable 'Name' of a 'StarKindStatus' if it exists.
-- Otherwise, returns 'Nothing'.
starKindStatusToName :: StarKindStatus -> Maybe Name
starKindStatusToName (IsKindVar n) = Just n
starKindStatusToName _             = Nothing

-- | Concat together all of the StarKindStatuses that are IsKindVar and extract
-- the kind variables' Names out.
catKindVarNames :: [StarKindStatus] -> [Name]
catKindVarNames = mapMaybe starKindStatusToName

-------------------------------------------------------------------------------
-- ClassRep
-------------------------------------------------------------------------------

class ClassRep a where
    arity           :: a -> Int
    allowExQuant    :: a -> Bool
    fullClassName   :: a -> Name
    classConstraint :: a -> Int -> Maybe Name

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
    ns = "Data.Deriving.Internal.withType: "

-- | Deduces the instance context and head for an instance.
buildTypeInstance :: ClassRep a
                  => a
                  -- ^ The typeclass for which an instance should be derived
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
buildTypeInstance cRep tyConName dataCxt tvbs Nothing =
    let varTys :: [Type]
        varTys = map tvbToType tvbs
    in buildTypeInstanceFromTys cRep tyConName dataCxt varTys False
-- Data family instance case
--
-- The CPP is present to work around a couple of annoying old GHC bugs.
-- See Note [Polykinded data families in Template Haskell]
buildTypeInstance cRep parentName dataCxt tvbs (Just instTysAndKinds) = do
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
                  -- Note that due to a GHC 7.8-specific bug
                  -- (see Note [Polykinded data families in Template Haskell]),
                  -- there may be more kind variable names than there are kinds
                  -- to substitute. But this is OK! If a kind is eta-reduced, it
                  -- means that is was not instantiated to something more specific,
                  -- so we need not substitute it. Using stealKindForType will
                  -- grab the correct kind.
                $ zipWith stealKindForType tvbs (givenTys ++ xTys)
#endif
    buildTypeInstanceFromTys cRep parentName dataCxt instTys True

-- For the given Types, generate an instance context and head. Coming up with
-- the instance type isn't as simple as dropping the last types, as you need to
-- be wary of kinds being instantiated with *.
-- See Note [Type inference in derived instances]
buildTypeInstanceFromTys :: ClassRep a
                         => a
                         -- ^ The typeclass for which an instance should be derived
                         -> Name
                         -- ^ The type constructor or data family name
                         -> Cxt
                         -- ^ The datatype context
                         -> [Type]
                         -- ^ The types to instantiate the instance with
                         -> Bool
                         -- ^ True if it's a data family, False otherwise
                         -> Q (Cxt, Type)
buildTypeInstanceFromTys cRep tyConName dataCxt varTysOrig isDataFamily = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM expandSyn varTysOrig

    let remainingLength :: Int
        remainingLength = length varTysOrig - arity cRep

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    -- Check there are enough types to drop and that all of them are either of
    -- kind * or kind k (for some kind variable k). If not, throw an error.
    when (remainingLength < 0 || any (== NotKindStar) droppedStarKindStati) $
      derivingKindError cRep tyConName

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
      derivingKindError cRep tyConName

    let preds    :: [Maybe Pred]
        kvNames  :: [[Name]]
        kvNames' :: [Name]
        -- Derive instance constraints (and any kind variables which are specialized
        -- to * in those constraints)
        (preds, kvNames) = unzip $ map (deriveConstraint cRep) remainingTysExpSubst
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
        instanceType = AppT (ConT (fullClassName cRep))
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
deriveConstraint :: ClassRep a => a -> Type -> (Maybe Pred, [Name])
deriveConstraint cRep t
  | not (isTyVar t) = (Nothing, [])
  | hasKindStar t   = ((`applyClass` tName) `fmap` classConstraint cRep 0, [])
  | otherwise = case hasKindVarChain 1 t of
      Just ns | cRepArity >= 1
              -> ((`applyClass` tName) `fmap` classConstraint cRep 1, ns)
      _ -> case hasKindVarChain 2 t of
           Just ns | cRepArity == 2
                   -> ((`applyClass` tName) `fmap` classConstraint cRep 2, ns)
           _ -> (Nothing, [])
  where
    tName :: Name
    tName     = varTToName t

    cRepArity :: Int
    cRepArity = arity cRep

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

     data Data (a :: k) (b :: k)

   Then you'd want to derived instance to be:

     instance C (Data (a :: *))

   Not:

     instance C (Data (a :: k))

2. We naïvely come up with instance constraints using the following criteria, using
   Show(1)(2) as the example typeclasses:

   (i)   If there's a type parameter n of kind *, generate a Show n constraint.
   (ii)  If there's a type parameter n of kind k1 -> k2 (where k1/k2 are * or kind
         variables), then generate a Show1 n constraint, and if k1/k2 are kind
         variables, then substitute k1/k2 with * elsewhere in the types. We must
         consider the case where they are kind variables because you might have a
         scenario like this:

           newtype Compose (f :: k2 -> *) (g :: k1 -> k2) (a :: k1)
             = Compose (f (g a))

         Which would have a derived Show1 instance of:

           instance (Show1 f, Show1 g) => Show1 (Compose f g) where ...
   (iii) If there's a type parameter n of kind k1 -> k2 -> k3 (where k1/k2/k3 are
         * or kind variables), then generate a Show2 constraint and perform
         kind substitution as in the other cases.
-}

-- Determines the types of a constructor's arguments as well as the last type
-- parameters (mapped to their auxiliary functions), expanding through any type synonyms.
-- The type parameters are determined on a constructor-by-constructor basis since
-- they may be refined to be particular types in a GADT.
reifyConTys :: ClassRep a
            => a
            -> [OneOrTwoNames b]
            -> Name
            -> Q ([Type], TyVarMap b)
reifyConTys cRep auxs conName = do
    info          <- reify conName
    (ctxt, uncTy) <- case info of
        DataConI _ ty _
#if !(MIN_VERSION_template_haskell(2,11,0))
                 _
#endif
                 -> fmap uncurryTy (expandSyn ty)
        _ -> error "Must be a data constructor"
    let (argTys, [resTy]) = splitAt (length uncTy - 1) uncTy
        unapResTy = unapplyTy resTy
        cRepArity = arity cRep
        -- If one of the last type variables is refined to a particular type
        -- (i.e., not truly polymorphic), we mark it with Nothing and filter
        -- it out later, since we only apply auxiliary functions to arguments of
        -- a type that it (1) one of the last type variables, and (2)
        -- of a truly polymorphic type.
        mbTvNames = map varTToName_maybe $
                        drop (length unapResTy - cRepArity) unapResTy
        -- We use Map.fromList to ensure that if there are any duplicate type
        -- variables (as can happen in a GADT), the rightmost type variable gets
        -- associated with the auxiliary function.
        --
        -- See Note [Matching functions with GADT type variables]
        tvMap = Map.fromList
                    . catMaybes -- Drop refined types
                    $ zipWith (\mbTvName aux ->
                                  fmap (\tvName -> (tvName, aux)) mbTvName)
                              mbTvNames auxs
    if (any (`predMentionsName` Map.keys tvMap) ctxt
         || Map.size tvMap < cRepArity)
         && not (allowExQuant cRep)
       then existentialContextError conName
       else return (argTys, tvMap)

reifyConTys1 :: ClassRep a
             => a
             -> [Name]
             -> Name
             -> Q ([Type], TyVarMap1)
reifyConTys1 cRep auxs = reifyConTys cRep (map OneName auxs)

reifyConTys2 :: ClassRep a
             => a
             -> [(Name, Name)]
             -> Name
             -> Q ([Type], TyVarMap2)
reifyConTys2 cRep auxs = reifyConTys cRep (map (\(x, y) -> TwoNames x y) auxs)

{-
Note [Matching functions with GADT type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When deriving category-2 classes like Show2, there is a tricky corner case to consider:

  data Both a b where
    BothCon :: x -> x -> Both x x

Which show functions should be applied to which arguments of BothCon? We have a
choice, since both the function of type (Int -> a -> ShowS) and of type
(Int -> b -> ShowS) can be applied to either argument. In such a scenario, the
second show function takes precedence over the first show function, so the
derived Show2 instance would be:

  instance Show2 Both where
    liftShowsPrec2 sp1 sp2 p (BothCon x1 x2) =
      showsParen (p > appPrec) $
        showString "BothCon " . sp2 appPrec1 x1 . showSpace . sp2 appPrec1 x2

This is not an arbitrary choice, as this definition ensures that
liftShowsPrec2 showsPrec = liftShowsPrec for a derived Show1 instance for
Both.
-}

-------------------------------------------------------------------------------
-- Error messages
-------------------------------------------------------------------------------

-- | The given datatype has no constructors, and we don't know what to do with it.
noConstructorsError :: Q a
noConstructorsError = fail "Must have at least one data constructor"

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: ClassRep a => a ->  Name -> Q b
derivingKindError cRep tyConName = fail
  . showString "Cannot derive well-kinded instance of form ‘"
  . showString className
  . showChar ' '
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass "
  . showString className
  . showString " expects an argument of kind "
  . showString (pprint . createKindChain $ arity cRep)
  $ ""
  where
    className :: String
    className = nameBase $ fullClassName cRep

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

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> Q a
etaReductionError instanceType = fail $
  "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
  ++ pprint instanceType

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
outOfPlaceTyVarError :: ClassRep a => a -> Name -> Q b
outOfPlaceTyVarError cRep conName = fail
    . showString "Constructor ‘"
    . showString (nameBase conName)
    . showString "‘ must only use its last "
    . shows n
    . showString " type variable(s) within the last "
    . shows n
    . showString " argument(s) of a data type"
    $ ""
  where
    n :: Int
    n = arity cRep

-- | Template Haskell didn't list all of a data family's instances upon reification
-- until template-haskell-2.7.0.0, which is necessary for a derived instance to work.
dataConIError :: Q a
dataConIError = fail
  . showString "Cannot use a data constructor."
  . showString "\n\t(Note: if you are trying to derive for a data family instance,"
  . showString "\n\tuse GHC >= 7.4 instead.)"
  $ ""

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

-- | A mapping of type variable Names to their auxiliary function Names.
type TyVarMap a = Map Name (OneOrTwoNames a)
type TyVarMap1 = TyVarMap One
type TyVarMap2 = TyVarMap Two

data OneOrTwoNames a where
    OneName  :: Name         -> OneOrTwoNames One
    TwoNames :: Name -> Name -> OneOrTwoNames Two

data One
data Two

interleave :: [a] -> [a] -> [a]
interleave (a1:a1s) (a2:a2s) = a1:a2:interleave a1s a2s
interleave _        _        = []

#if MIN_VERSION_ghc_prim(0,3,1)
tagToEnum :: Int# -> Bool
tagToEnum x = tagToEnum# x
#else
tagToEnum :: Bool -> Bool
tagToEnum x = x
#endif
{-# INLINE tagToEnum #-}

-- isRight and fromEither taken from the extra package (BSD3-licensed)

-- | Test if an 'Either' value is the 'Right' constructor.
--   Provided as standard with GHC 7.8 and above.
isRight :: Either l r -> Bool
isRight Right{} = True; isRight _ = False

-- | Pull the value out of an 'Either' where both alternatives
--   have the same type.
--
-- > \x -> fromEither (Left x ) == x
-- > \x -> fromEither (Right x) == x
fromEither :: Either a a -> a
fromEither = either id id

-- filterByList, filterByLists, and partitionByList taken from GHC (BSD3-licensed)

-- | 'filterByList' takes a list of Bools and a list of some elements and
-- filters out these elements for which the corresponding value in the list of
-- Bools is False. This function does not check whether the lists have equal
-- length.
filterByList :: [Bool] -> [a] -> [a]
filterByList (True:bs)  (x:xs) = x : filterByList bs xs
filterByList (False:bs) (_:xs) =     filterByList bs xs
filterByList _          _      = []

-- | 'filterByLists' takes a list of Bools and two lists as input, and
-- outputs a new list consisting of elements from the last two input lists. For
-- each Bool in the list, if it is 'True', then it takes an element from the
-- former list. If it is 'False', it takes an element from the latter list.
-- The elements taken correspond to the index of the Bool in its list.
-- For example:
--
-- @
-- filterByLists [True, False, True, False] \"abcd\" \"wxyz\" = \"axcz\"
-- @
--
-- This function does not check whether the lists have equal length.
filterByLists :: [Bool] -> [a] -> [a] -> [a]
filterByLists (True:bs)  (x:xs) (_:ys) = x : filterByLists bs xs ys
filterByLists (False:bs) (_:xs) (y:ys) = y : filterByLists bs xs ys
filterByLists _          _      _      = []

-- | 'partitionByList' takes a list of Bools and a list of some elements and
-- partitions the list according to the list of Bools. Elements corresponding
-- to 'True' go to the left; elements corresponding to 'False' go to the right.
-- For example, @partitionByList [True, False, True] [1,2,3] == ([1,3], [2])@
-- This function does not check whether the lists have equal
-- length.
partitionByList :: [Bool] -> [a] -> ([a], [a])
partitionByList = go [] []
  where
    go trues falses (True  : bs) (x : xs) = go (x:trues) falses bs xs
    go trues falses (False : bs) (x : xs) = go trues (x:falses) bs xs
    go trues falses _ _ = (reverse trues, reverse falses)

-- | Apply an @Either Exp Exp@ expression to an 'Exp' expression,
-- preserving the 'Either'-ness.
appEitherE :: Q (Either Exp Exp) -> Q Exp -> Q (Either Exp Exp)
appEitherE e1Q e2Q = do
    e2 <- e2Q
    let e2' :: Exp -> Exp
        e2' = (`AppE` e2)
    either (Left . e2') (Right . e2') `fmap` e1Q

integerE :: Int -> Q Exp
integerE = litE . integerL . fromIntegral

-- | Returns True if a Type has kind *.
hasKindStar :: Type -> Bool
hasKindStar VarT{}         = True
#if MIN_VERSION_template_haskell(2,8,0)
hasKindStar (SigT _ StarT) = True
#else
hasKindStar (SigT _ StarK) = True
#endif
hasKindStar _              = False

-- Returns True is a kind is equal to *, or if it is a kind variable.
isStarOrVar :: Kind -> Bool
#if MIN_VERSION_template_haskell(2,8,0)
isStarOrVar StarT  = True
isStarOrVar VarT{} = True
#else
isStarOrVar StarK  = True
#endif
isStarOrVar _      = False

-- | Gets all of the type/kind variable names mentioned somewhere in a Type.
tyVarNamesOfType :: Type -> [Name]
tyVarNamesOfType = go
  where
    go :: Type -> [Name]
    go (AppT t1 t2) = go t1 ++ go t2
    go (SigT t _k)  = go t
#if MIN_VERSION_template_haskell(2,8,0)
                           ++ go _k
#endif
    go (VarT n)     = [n]
    go _            = []

-- | Gets all of the type/kind variable names mentioned somewhere in a Kind.
tyVarNamesOfKind :: Kind -> [Name]
#if MIN_VERSION_template_haskell(2,8,0)
tyVarNamesOfKind = tyVarNamesOfType
#else
tyVarNamesOfKind _ = [] -- There are no kind variables
#endif

-- | @hasKindVarChain n kind@ Checks if @kind@ is of the form
-- k_0 -> k_1 -> ... -> k_(n-1), where k0, k1, ..., and k_(n-1) can be * or
-- kind variables.
hasKindVarChain :: Int -> Type -> Maybe [Name]
hasKindVarChain kindArrows t =
  let uk = uncurryKind (tyKind t)
  in if (length uk - 1 == kindArrows) && all isStarOrVar uk
        then Just (concatMap tyVarNamesOfKind uk)
        else Nothing

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
tyKind :: Type -> Kind
tyKind (SigT _ k) = k
tyKind _          = starK

-- | If a VarT is missing an explicit kind signature, steal it from a TyVarBndr.
stealKindForType :: TyVarBndr -> Type -> Type
stealKindForType tvb t@VarT{} = SigT t (tvbKind tvb)
stealKindForType _   t        = t

-- | Monadic version of concatMap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat `liftM` mapM f xs

zipWithAndUnzipM :: Monad m
                 => (a -> b -> m (c, d)) -> [a] -> [b] -> m ([c], [d])
zipWithAndUnzipM f (x:xs) (y:ys) = do
    (c, d) <- f x y
    (cs, ds) <- zipWithAndUnzipM f xs ys
    return (c:cs, d:ds)
zipWithAndUnzipM _ _ _ = return ([], [])
{-# INLINE zipWithAndUnzipM #-}

zipWith3AndUnzipM :: Monad m
                 => (a -> b -> c -> m (d, e)) -> [a] -> [b] -> [c]
                 -> m ([d], [e])
zipWith3AndUnzipM f (x:xs) (y:ys) (z:zs) = do
    (d, e) <- f x y z
    (ds, es) <- zipWith3AndUnzipM f xs ys zs
    return (d:ds, e:es)
zipWith3AndUnzipM _ _ _ _ = return ([], [])
{-# INLINE zipWith3AndUnzipM #-}

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- | Extracts the name of a constructor.
constructorName :: Con -> Name
constructorName (NormalC name      _  ) = name
constructorName (RecC    name      _  ) = name
constructorName (InfixC  _    name _  ) = name
constructorName (ForallC _    _    con) = constructorName con
#if MIN_VERSION_template_haskell(2,11,0)
constructorName (GadtC    names _ _)    = head names
constructorName (RecGadtC names _ _)    = head names
#endif

isNullaryCon :: Con -> Bool
isNullaryCon (NormalC _ [])    = True
isNullaryCon (RecC    _ [])    = True
isNullaryCon InfixC{}          = False
isNullaryCon (ForallC _ _ con) = isNullaryCon con
#if MIN_VERSION_template_haskell(2,11,0)
isNullaryCon (GadtC    _ [] _) = True
isNullaryCon (RecGadtC _ [] _) = True
#endif
isNullaryCon _                 = False

-- | Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = mapM (newName . (prefix ++) . show) [1..n]

-- | Extracts the kind from a TyVarBndr.
tvbKind :: TyVarBndr -> Kind
tvbKind (PlainTV  _)   = starK
tvbKind (KindedTV _ k) = k

-- | Convert a TyVarBndr to a Type.
tvbToType :: TyVarBndr -> Type
tvbToType (PlainTV n)    = VarT n
tvbToType (KindedTV n k) = SigT (VarT n) k

-- | Applies a typeclass constraint to a type.
applyClass :: Name -> Name -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
applyClass con t = AppT (ConT con) (VarT t)
#else
applyClass con t = ClassP con [VarT t]
#endif

createKindChain :: Int -> Kind
createKindChain = go starK
  where
    go :: Kind -> Int -> Kind
    go k !0 = k
#if MIN_VERSION_template_haskell(2,8,0)
    go k !n = go (AppT (AppT ArrowT StarT) k) (n - 1)
#else
    go k !n = go (ArrowK StarK k) (n - 1)
#endif

-- | Checks to see if the last types in a data family instance can be safely eta-
-- reduced (i.e., dropped), given the other types. This checks for three conditions:
--
-- (1) All of the dropped types are type variables
-- (2) All of the dropped types are distinct
-- (3) None of the remaining types mention any of the dropped types
canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
    && allDistinct droppedNames -- Make sure not to pass something of type [Type], since Type
                                -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsName` droppedNames) remaining)
  where
    droppedNames :: [Name]
    droppedNames = map varTToName dropped

-- | Extract the Name from a type constructor. If the argument Type is not a
-- type variable, throw an error.
conTToName :: Type -> Name
conTToName (ConT n)   = n
conTToName (SigT t _) = conTToName t
conTToName _          = error "Not a type constructor!"

-- | Extract Just the Name from a type variable. If the argument Type is not a
-- type variable, return Nothing.
varTToName_maybe :: Type -> Maybe Name
varTToName_maybe (VarT n)   = Just n
varTToName_maybe (SigT t _) = varTToName_maybe t
varTToName_maybe _          = Nothing

-- | Extract the Name from a type variable. If the argument Type is not a
-- type variable, throw an error.
varTToName :: Type -> Name
varTToName = fromMaybe (error "Not a type variable!") . varTToName_maybe

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Is the given type a type family constructor (and not a data family constructor)?
isTyFamily :: Type -> Q Bool
isTyFamily (ConT n) = do
    info <- reify n
    return $ case info of
#if MIN_VERSION_template_haskell(2,11,0)
         FamilyI OpenTypeFamilyD{} _       -> True
#elif MIN_VERSION_template_haskell(2,7,0)
         FamilyI (FamilyD TypeFam _ _ _) _ -> True
#else
         TyConI  (FamilyD TypeFam _ _ _)   -> True
#endif
#if MIN_VERSION_template_haskell(2,9,0)
         FamilyI ClosedTypeFamilyD{} _     -> True
#endif
         _ -> False
isTyFamily _ = return False

-- | Are all of the items in a list (which have an ordering) distinct?
--
-- This uses Set (as opposed to nub) for better asymptotic time complexity.
allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

-- | Does the given type mention any of the Names in the list?
mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t _k)  names = go t names
#if MIN_VERSION_template_haskell(2,8,0)
                              || go _k names
#endif
    go (VarT n)     names = n `elem` names
    go _            _     = False

-- | Does an instance predicate mention any of the Names in the list?
predMentionsName :: Pred -> [Name] -> Bool
#if MIN_VERSION_template_haskell(2,10,0)
predMentionsName = mentionsName
#else
predMentionsName (ClassP n tys) names = n `elem` names || any (`mentionsName` names) tys
predMentionsName (EqualP t1 t2) names = mentionsName t1 names || mentionsName t2 names
#endif

-- | Construct a type via curried application.
applyTy :: Type -> [Type] -> Type
applyTy = foldl' AppT

-- | Fully applies a type constructor to its type variables.
applyTyCon :: Name -> [Type] -> Type
applyTyCon = applyTy . ConT

-- | Split an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would split to this:
--
-- @
-- [Either, Int, Char]
-- @
unapplyTy :: Type -> [Type]
unapplyTy = reverse . go
  where
    go :: Type -> [Type]
    go (AppT t1 t2)    = t2:go t1
    go (SigT t _)      = go t
    go (ForallT _ _ t) = go t
    go t               = [t]

-- | Split a type signature by the arrows on its spine. For example, this:
--
-- @
-- forall a b. (a ~ b) => (a -> b) -> Char -> ()
-- @
--
-- would split to this:
--
-- @
-- (a ~ b, [a -> b, Char, ()])
-- @
uncurryTy :: Type -> (Cxt, [Type])
uncurryTy (AppT (AppT ArrowT t1) t2) =
  let (ctxt, tys) = uncurryTy t2
  in (ctxt, t1:tys)
uncurryTy (SigT t _) = uncurryTy t
uncurryTy (ForallT _ ctxt t) =
  let (ctxt', tys) = uncurryTy t
  in (ctxt ++ ctxt', tys)
uncurryTy t = ([], [t])


-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> [Kind]
#if MIN_VERSION_template_haskell(2,8,0)
uncurryKind = snd . uncurryTy
#else
uncurryKind (ArrowK k1 k2) = k1:uncurryKind k2
uncurryKind k              = [k]
#endif

untagExpr :: [(Name, Name)] -> Q Exp -> Q Exp
untagExpr [] e = e
untagExpr ((untagThis, putTagHere) : more) e =
    caseE (varE getTagValName `appE` varE untagThis)
          [match (varP putTagHere)
                 (normalB $ untagExpr more e)
                 []]

primOpAppExpr :: Q Exp -> Name -> Q Exp -> Q Exp
primOpAppExpr e1 op e2 = varE tagToEnumValName `appE`
                           infixApp e1 (varE op) e2

-- | Checks if a 'Name' represents a tuple type constructor (other than '()')
isNonUnitTuple :: Name -> Bool
isNonUnitTuple = isNonUnitTupleString . nameBase

-- | Checks if a 'String' represents a tuple (other than '()')
isNonUnitTupleString :: String -> Bool
isNonUnitTupleString ('(':',':_) = True
isNonUnitTupleString _           = False

-------------------------------------------------------------------------------
-- Manually quoted names
-------------------------------------------------------------------------------

-- By manually generating these names we avoid needing to use the
-- TemplateHaskell language extension when compiling the deriving-compat library.
-- This allows the library to be used in stage1 cross-compilers.

derivingCompatPackageKey :: String
#ifdef CURRENT_PACKAGE_KEY
derivingCompatPackageKey = CURRENT_PACKAGE_KEY
#else
derivingCompatPackageKey = "deriving-compat-" ++ showVersion version
#endif

mkDerivingCompatName_v :: String -> Name
mkDerivingCompatName_v = mkNameG_v derivingCompatPackageKey "Data.Deriving.Internal"

fmapConstValName :: Name
fmapConstValName = mkDerivingCompatName_v "fmapConst"

foldrConstValName :: Name
foldrConstValName = mkDerivingCompatName_v "foldrConst"

foldMapConstValName :: Name
foldMapConstValName = mkDerivingCompatName_v "foldMapConst"

traverseConstValName :: Name
traverseConstValName = mkDerivingCompatName_v "traverseConst"

eqConstValName :: Name
eqConstValName = mkDerivingCompatName_v "eqConst"

eq1ConstValName :: Name
eq1ConstValName = mkDerivingCompatName_v "eq1Const"

liftEqConstValName :: Name
liftEqConstValName = mkDerivingCompatName_v "liftEqConst"

liftEq2ConstValName :: Name
liftEq2ConstValName = mkDerivingCompatName_v "liftEq2Const"

compareConstValName :: Name
compareConstValName = mkDerivingCompatName_v "compareConst"

ltConstValName :: Name
ltConstValName = mkDerivingCompatName_v "ltConst"

compare1ConstValName :: Name
compare1ConstValName = mkDerivingCompatName_v "compare1Const"

liftCompareConstValName :: Name
liftCompareConstValName = mkDerivingCompatName_v "liftCompareConst"

liftCompare2ConstValName :: Name
liftCompare2ConstValName = mkDerivingCompatName_v "liftCompare2Const"

readsPrecConstValName :: Name
readsPrecConstValName = mkDerivingCompatName_v "readsPrecConst"

readPrecConstValName :: Name
readPrecConstValName = mkDerivingCompatName_v "readPrecConst"

readsPrec1ConstValName :: Name
readsPrec1ConstValName = mkDerivingCompatName_v "readsPrec1Const"

liftReadsPrecConstValName :: Name
liftReadsPrecConstValName = mkDerivingCompatName_v "liftReadsPrecConst"

liftReadPrecConstValName :: Name
liftReadPrecConstValName = mkDerivingCompatName_v "liftReadPrecConst"

liftReadsPrec2ConstValName :: Name
liftReadsPrec2ConstValName = mkDerivingCompatName_v "liftReadsPrec2Const"

liftReadPrec2ConstValName :: Name
liftReadPrec2ConstValName = mkDerivingCompatName_v "liftReadPrec2Const"

showsPrecConstValName :: Name
showsPrecConstValName = mkDerivingCompatName_v "showsPrecConst"

showsPrec1ConstValName :: Name
showsPrec1ConstValName = mkDerivingCompatName_v "showsPrec1Const"

liftShowsPrecConstValName :: Name
liftShowsPrecConstValName = mkDerivingCompatName_v "liftShowsPrecConst"

liftShowsPrec2ConstValName :: Name
liftShowsPrec2ConstValName = mkDerivingCompatName_v "liftShowsPrec2Const"

tagToEnumValName :: Name
tagToEnumValName = mkDerivingCompatName_v "tagToEnum"

cHashDataName :: Name
cHashDataName = mkNameG_d "ghc-prim" "GHC.Types" "C#"

dHashDataName :: Name
dHashDataName = mkNameG_d "ghc-prim" "GHC.Types" "D#"

dualDataName :: Name
dualDataName = mkNameG_d "base" "Data.Monoid" "Dual"

endoDataName :: Name
endoDataName = mkNameG_d "base" "Data.Monoid" "Endo"

fHashDataName :: Name
fHashDataName = mkNameG_d "ghc-prim" "GHC.Types" "F#"

identDataName :: Name
identDataName = mkNameG_d "base" "Text.Read.Lex" "Ident"

iHashDataName :: Name
iHashDataName = mkNameG_d "ghc-prim" "GHC.Types" "I#"

puncDataName :: Name
puncDataName = mkNameG_d "base" "Text.Read.Lex" "Punc"

symbolDataName :: Name
symbolDataName = mkNameG_d "base" "Text.Read.Lex" "Symbol"

wrapMonadDataName :: Name
wrapMonadDataName = mkNameG_d "base" "Control.Applicative" "WrapMonad"

addrHashTypeName :: Name
addrHashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Addr#"

charHashTypeName :: Name
charHashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Char#"

doubleHashTypeName :: Name
doubleHashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Double#"

floatHashTypeName :: Name
floatHashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Float#"

foldableTypeName :: Name
foldableTypeName = mkNameG_tc "base" "Data.Foldable" "Foldable"

functorTypeName :: Name
functorTypeName = mkNameG_tc "base" "GHC.Base" "Functor"

intHashTypeName :: Name
intHashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Int#"

readTypeName :: Name
readTypeName = mkNameG_tc "base" "GHC.Read" "Read"

showTypeName :: Name
showTypeName = mkNameG_tc "base" "GHC.Show" "Show"

traversableTypeName :: Name
traversableTypeName = mkNameG_tc "base" "Data.Traversable" "Traversable"

wordHashTypeName :: Name
wordHashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Word#"

altValName :: Name
altValName = mkNameG_v "base" "Text.ParserCombinators.ReadPrec" "+++"

appEndoValName :: Name
appEndoValName = mkNameG_v "base" "Data.Monoid" "appEndo"

chooseValName :: Name
chooseValName = mkNameG_v "base" "GHC.Read" "choose"

composeValName :: Name
composeValName = mkNameG_v "base" "GHC.Base" "."

constValName :: Name
constValName = mkNameG_v "base" "GHC.Base" "const"

eqAddrHashValName :: Name
eqAddrHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "eqAddr#"

eqCharHashValName :: Name
eqCharHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "eqChar#"

eqDoubleHashValName :: Name
eqDoubleHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "==##"

eqFloatHashValName :: Name
eqFloatHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "eqFloat#"

eqIntHashValName :: Name
eqIntHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "==#"

eqWordHashValName :: Name
eqWordHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "eqWord#"

errorValName :: Name
errorValName = mkNameG_v "base" "GHC.Err" "error"

flipValName :: Name
flipValName = mkNameG_v "base" "GHC.Base" "flip"

fmapValName :: Name
fmapValName = mkNameG_v "base" "GHC.Base" "fmap"

foldrValName :: Name
foldrValName = mkNameG_v "base" "Data.Foldable" "foldr"

foldMapValName :: Name
foldMapValName = mkNameG_v "base" "Data.Foldable" "foldMap"

geAddrHashValName :: Name
geAddrHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "geAddr#"

geCharHashValName :: Name
geCharHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "geChar#"

geDoubleHashValName :: Name
geDoubleHashValName = mkNameG_v "ghc-prim" "GHC.Prim" ">=##"

geFloatHashValName :: Name
geFloatHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "geFloat#"

geIntHashValName :: Name
geIntHashValName = mkNameG_v "ghc-prim" "GHC.Prim" ">=#"

getDualValName :: Name
getDualValName = mkNameG_v "base" "Data.Monoid" "getDual"

getTagValName :: Name
getTagValName = mkNameG_v "base" "GHC.Base" "getTag"

geWordHashValName :: Name
geWordHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "geWord#"

gtAddrHashValName :: Name
gtAddrHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "gtAddr#"

gtCharHashValName :: Name
gtCharHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "gtChar#"

gtDoubleHashValName :: Name
gtDoubleHashValName = mkNameG_v "ghc-prim" "GHC.Prim" ">##"

gtFloatHashValName :: Name
gtFloatHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "gtFloat#"

gtIntHashValName :: Name
gtIntHashValName = mkNameG_v "ghc-prim" "GHC.Prim" ">#"

gtWordHashValName :: Name
gtWordHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "gtWord#"

idValName :: Name
idValName = mkNameG_v "base" "GHC.Base" "id"

leAddrHashValName :: Name
leAddrHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "leAddr#"

leCharHashValName :: Name
leCharHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "leChar#"

leDoubleHashValName :: Name
leDoubleHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "<=##"

leFloatHashValName :: Name
leFloatHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "leFloat#"

leIntHashValName :: Name
leIntHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "<=#"

leWordHashValName :: Name
leWordHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "leWord#"

liftReadListPrecDefaultValName :: Name
liftReadListPrecDefaultValName = mkNameG_v "base" "Data.Functor.Classes" "liftReadListPrecDefault"

liftReadListPrec2DefaultValName :: Name
liftReadListPrec2DefaultValName = mkNameG_v "base" "Data.Functor.Classes" "liftReadListPrec2Default"

liftReadListPrecValName :: Name
liftReadListPrecValName = mkNameG_v "base" "Data.Functor.Classes" "liftReadListPrec"

liftReadListPrec2ValName :: Name
liftReadListPrec2ValName = mkNameG_v "base" "Data.Functor.Classes" "liftReadListPrec2"

liftReadPrecValName :: Name
liftReadPrecValName = mkNameG_v "base" "Data.Functor.Classes" "liftReadPrec"

liftReadPrec2ValName :: Name
liftReadPrec2ValName = mkNameG_v "base" "Data.Functor.Classes" "liftReadPrec2"

listValName :: Name
listValName = mkNameG_v "base" "GHC.Read" "list"

ltAddrHashValName :: Name
ltAddrHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "ltAddr#"

ltCharHashValName :: Name
ltCharHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "ltChar#"

ltDoubleHashValName :: Name
ltDoubleHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "<##"

ltFloatHashValName :: Name
ltFloatHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "ltFloat#"

ltIntHashValName :: Name
ltIntHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "<#"

ltWordHashValName :: Name
ltWordHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "ltWord#"

parenValName :: Name
parenValName = mkNameG_v "base" "GHC.Read" "paren"

parensValName :: Name
parensValName = mkNameG_v "base" "GHC.Read" "parens"

pfailValName :: Name
pfailValName = mkNameG_v "base" "Text.ParserCombinators.ReadPrec" "pfail"

precValName :: Name
precValName = mkNameG_v "base" "Text.ParserCombinators.ReadPrec" "prec"

readListValName :: Name
readListValName = mkNameG_v "base" "GHC.Read" "readList"

readListPrecDefaultValName :: Name
readListPrecDefaultValName = mkNameG_v "base" "GHC.Read" "readListPrecDefault"

readListPrecValName :: Name
readListPrecValName = mkNameG_v "base" "GHC.Read" "readListPrec"

readPrec_to_SValName :: Name
readPrec_to_SValName = mkNameG_v "base" "Text.ParserCombinators.ReadPrec" "readPrec_to_S"

readPrecValName :: Name
readPrecValName = mkNameG_v "base" "GHC.Read" "readPrec"

readS_to_PrecValName :: Name
readS_to_PrecValName = mkNameG_v "base" "Text.ParserCombinators.ReadPrec" "readS_to_Prec"

readsPrecValName :: Name
readsPrecValName = mkNameG_v "base" "GHC.Read" "readsPrec"

resetValName :: Name
resetValName = mkNameG_v "base" "Text.ParserCombinators.ReadPrec" "reset"

returnValName :: Name
returnValName = mkNameG_v "base" "GHC.Base" "return"

showCharValName :: Name
showCharValName = mkNameG_v "base" "GHC.Show" "showChar"

showListValName :: Name
showListValName = mkNameG_v "base" "GHC.Show" "showList"

showListWithValName :: Name
showListWithValName = mkNameG_v "base" "Text.Show" "showListWith"

showParenValName :: Name
showParenValName = mkNameG_v "base" "GHC.Show" "showParen"

showsPrecValName :: Name
showsPrecValName = mkNameG_v "base" "GHC.Show" "showsPrec"

showSpaceValName :: Name
showSpaceValName = mkNameG_v "base" "GHC.Show" "showSpace"

showStringValName :: Name
showStringValName = mkNameG_v "base" "GHC.Show" "showString"

stepValName :: Name
stepValName = mkNameG_v "base" "Text.ParserCombinators.ReadPrec" "step"

traverseValName :: Name
traverseValName = mkNameG_v "base" "Data.Traversable" "traverse"

unwrapMonadValName :: Name
unwrapMonadValName = mkNameG_v "base" "Control.Applicative" "unwrapMonad"

#if MIN_VERSION_base(4,4,0)
boolTypeName :: Name
boolTypeName = mkNameG_tc "ghc-prim" "GHC.Types" "Bool"

falseDataName :: Name
falseDataName = mkNameG_v "ghc-prim" "GHC.Types" "False"

trueDataName :: Name
trueDataName = mkNameG_v "ghc-prim" "GHC.Types" "True"
#else
boolTypeName :: Name
boolTypeName = mkNameG_tc "ghc-prim" "GHC.Bool" "Bool"

falseDataName :: Name
falseDataName = mkNameG_v "ghc-prim" "GHC.Bool" "False"

trueDataName :: Name
trueDataName = mkNameG_v "ghc-prim" "GHC.Bool" "True"
#endif

#if MIN_VERSION_base(4,5,0)
eqDataName :: Name
eqDataName = mkNameG_d "ghc-prim" "GHC.Types" "EQ"

gtDataName :: Name
gtDataName = mkNameG_d "ghc-prim" "GHC.Types" "GT"

ltDataName :: Name
ltDataName = mkNameG_d "ghc-prim" "GHC.Types" "LT"

eqTypeName :: Name
eqTypeName = mkNameG_tc "ghc-prim" "GHC.Classes" "Eq"

ordTypeName :: Name
ordTypeName = mkNameG_tc "ghc-prim" "GHC.Classes" "Ord"

andValName :: Name
andValName = mkNameG_v "ghc-prim" "GHC.Classes" "&&"

compareValName :: Name
compareValName = mkNameG_v "ghc-prim" "GHC.Classes" "compare"

eqValName :: Name
eqValName = mkNameG_v "ghc-prim" "GHC.Classes" "=="

geValName :: Name
geValName = mkNameG_v "ghc-prim" "GHC.Classes" ">="

gtValName :: Name
gtValName = mkNameG_v "ghc-prim" "GHC.Classes" ">"

leValName :: Name
leValName = mkNameG_v "ghc-prim" "GHC.Classes" "<="

ltValName :: Name
ltValName = mkNameG_v "ghc-prim" "GHC.Classes" "<"

notValName :: Name
notValName = mkNameG_v "ghc-prim" "GHC.Classes" "not"
#else
eqDataName :: Name
eqDataName = mkNameG_d "ghc-prim" "GHC.Ordering" "EQ"

gtDataName :: Name
gtDataName = mkNameG_d "ghc-prim" "GHC.Ordering" "GT"

ltDataName :: Name
ltDataName = mkNameG_d "ghc-prim" "GHC.Ordering" "LT"

eqTypeName :: Name
eqTypeName = mkNameG_tc "base" "GHC.Classes" "Eq"

ordTypeName :: Name
ordTypeName = mkNameG_tc "base" "GHC.Classes" "Ord"

andValName :: Name
andValName = mkNameG_v "base" "GHC.Classes" "&&"

compareValName :: Name
compareValName = mkNameG_v "base" "GHC.Classes" "compare"

eqValName :: Name
eqValName = mkNameG_v "base" "GHC.Classes" "=="

geValName :: Name
geValName = mkNameG_v "base" "GHC.Classes" ">="

gtValName :: Name
gtValName = mkNameG_v "base" "GHC.Classes" ">"

leValName :: Name
leValName = mkNameG_v "base" "GHC.Classes" "<="

ltValName :: Name
ltValName = mkNameG_v "base" "GHC.Classes" "<"

notValName :: Name
notValName = mkNameG_v "base" "GHC.Classes" "not"
#endif

#if MIN_VERSION_base(4,6,0)
wHashDataName :: Name
wHashDataName = mkNameG_d "ghc-prim" "GHC.Types" "W#"
#else
wHashDataName :: Name
wHashDataName = mkNameG_d "base" "GHC.Word" "W#"
#endif

#if MIN_VERSION_base(4,6,0) && !(MIN_VERSION_base(4,9,0))
starKindName :: Name
starKindName = mkNameG_tc "ghc-prim" "GHC.Prim" "*"
#endif

#if MIN_VERSION_base(4,7,0)
expectPValName :: Name
expectPValName = mkNameG_v "base" "GHC.Read" "expectP"
#else
expectP :: Lexeme -> ReadPrec ()
expectP lexeme = do
  thing <- lexP
  if thing == lexeme then return () else pfail

expectPValName :: Name
expectPValName = mkDerivingCompatName_v "expectP"
#endif

#if MIN_VERSION_base(4,8,0)
pureValName :: Name
pureValName = mkNameG_v "base" "GHC.Base" "pure"

apValName :: Name
apValName = mkNameG_v "base" "GHC.Base" "<*>"

mappendValName :: Name
mappendValName = mkNameG_v "base" "GHC.Base" "mappend"

memptyValName :: Name
memptyValName = mkNameG_v "base" "GHC.Base" "mempty"
#else
pureValName :: Name
pureValName = mkNameG_v "base" "Control.Applicative" "pure"

apValName :: Name
apValName = mkNameG_v "base" "Control.Applicative" "<*>"

mappendValName :: Name
mappendValName = mkNameG_v "base" "Data.Monoid" "mappend"

memptyValName :: Name
memptyValName = mkNameG_v "base" "Data.Monoid" "mempty"
#endif

#if MIN_VERSION_base(4,9,0)
eq1TypeName :: Name
eq1TypeName = mkNameG_tc "base" "Data.Functor.Classes" "Eq1"

eq2TypeName :: Name
eq2TypeName = mkNameG_tc "base" "Data.Functor.Classes" "Eq2"

liftEqValName :: Name
liftEqValName = mkNameG_v "base" "Data.Functor.Classes" "liftEq"

liftEq2ValName :: Name
liftEq2ValName = mkNameG_v "base" "Data.Functor.Classes" "liftEq2"

ord1TypeName :: Name
ord1TypeName = mkNameG_tc "base" "Data.Functor.Classes" "Ord1"

ord2TypeName :: Name
ord2TypeName = mkNameG_tc "base" "Data.Functor.Classes" "Ord2"

liftCompareValName :: Name
liftCompareValName = mkNameG_v "base" "Data.Functor.Classes" "liftCompare"

liftCompare2ValName :: Name
liftCompare2ValName = mkNameG_v "base" "Data.Functor.Classes" "liftCompare2"

read1TypeName :: Name
read1TypeName = mkNameG_tc "base" "Data.Functor.Classes" "Read1"

read2TypeName :: Name
read2TypeName = mkNameG_tc "base" "Data.Functor.Classes" "Read2"

liftReadsPrecValName :: Name
liftReadsPrecValName = mkNameG_v "base" "Data.Functor.Classes" "liftReadsPrec"

liftReadListValName :: Name
liftReadListValName = mkNameG_v "base" "Data.Functor.Classes" "liftReadList"

liftReadsPrec2ValName :: Name
liftReadsPrec2ValName = mkNameG_v "base" "Data.Functor.Classes" "liftReadsPrec2"

liftReadList2ValName :: Name
liftReadList2ValName = mkNameG_v "base" "Data.Functor.Classes" "liftReadList2"

show1TypeName :: Name
show1TypeName = mkNameG_tc "base" "Data.Functor.Classes" "Show1"

show2TypeName :: Name
show2TypeName = mkNameG_tc "base" "Data.Functor.Classes" "Show2"

liftShowListValName :: Name
liftShowListValName = mkNameG_v "base" "Data.Functor.Classes" "liftShowList"

liftShowsPrecValName :: Name
liftShowsPrecValName = mkNameG_v "base" "Data.Functor.Classes" "liftShowsPrec"

liftShowList2ValName :: Name
liftShowList2ValName = mkNameG_v "base" "Data.Functor.Classes" "liftShowList2"

liftShowsPrec2ValName :: Name
liftShowsPrec2ValName = mkNameG_v "base" "Data.Functor.Classes" "liftShowsPrec2"
#else
-- If Data.Functor.Classes isn't located in base, then sadly we can't refer to
-- Names from that module without using -XTemplateHaskell.
# if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
eq1TypeName :: Name
eq1TypeName = ''Eq1

eq2TypeName :: Name
eq2TypeName = ''Eq2

liftEqValName :: Name
liftEqValName = 'liftEq

liftEq2ValName :: Name
liftEq2ValName = 'liftEq2

ord1TypeName :: Name
ord1TypeName = ''Ord1

ord2TypeName :: Name
ord2TypeName = ''Ord2

liftCompareValName :: Name
liftCompareValName = 'liftCompare

liftCompare2ValName :: Name
liftCompare2ValName = 'liftCompare2

read1TypeName :: Name
read1TypeName = ''Read1

read2TypeName :: Name
read2TypeName = ''Read2

liftReadsPrecValName :: Name
liftReadsPrecValName = 'liftReadsPrec

liftReadListValName :: Name
liftReadListValName = 'liftReadList

liftReadsPrec2ValName :: Name
liftReadsPrec2ValName = 'liftReadsPrec2

liftReadList2ValName :: Name
liftReadList2ValName = 'liftReadList2

show1TypeName :: Name
show1TypeName = ''Show1

show2TypeName :: Name
show2TypeName = ''Show2

liftShowListValName :: Name
liftShowListValName = 'liftShowList

liftShowsPrecValName :: Name
liftShowsPrecValName = 'liftShowsPrec

liftShowList2ValName :: Name
liftShowList2ValName = 'liftShowList2

liftShowsPrec2ValName :: Name
liftShowsPrec2ValName = 'liftShowsPrec2
# else
eq1TypeName :: Name
eq1TypeName = ''Eq1

eq1ValName :: Name
eq1ValName = 'eq1

ord1TypeName :: Name
ord1TypeName = ''Ord1

compare1ValName :: Name
compare1ValName = 'compare1

read1TypeName :: Name
read1TypeName = ''Read1

readsPrec1ValName :: Name
readsPrec1ValName = 'readsPrec1

show1TypeName :: Name
show1TypeName = ''Show1

showsPrec1ValName :: Name
showsPrec1ValName = 'showsPrec1

newtype Apply f a = Apply { unApply :: f a }

instance (Eq1 f, Eq a) => Eq (Apply f a) where
    Apply x == Apply y = eq1 x y

instance (Ord1 g, Ord a) => Ord (Apply g a) where
    compare (Apply x) (Apply y) = compare1 x y

instance (Read1 f, Read a) => Read (Apply f a) where
    readsPrec d s = [(Apply a, t) | (a, t) <- readsPrec1 d s]

instance (Show1 f, Show a) => Show (Apply f a) where
    showsPrec p (Apply x) = showsPrec1 p x

makeFmapApplyNeg :: ClassRep a => a -> Name -> Type -> Name -> Q Exp
makeFmapApplyNeg = makeFmapApply False

makeFmapApplyPos :: ClassRep a => a -> Name -> Type -> Name -> Q Exp
makeFmapApplyPos = makeFmapApply True

makeFmapApply :: ClassRep a => Bool -> a -> Name -> Type -> Name -> Q Exp
makeFmapApply pos cRep conName (SigT ty _) name = makeFmapApply pos cRep conName ty name
makeFmapApply pos cRep conName t name = do
    let tyCon :: Type
        tyArgs :: [Type]
        tyCon:tyArgs = unapplyTy t

        numLastArgs :: Int
        numLastArgs = min (arity cRep) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        inspectTy :: Type -> Q Exp
        inspectTy (SigT ty _) = inspectTy ty
        inspectTy (VarT a) | a == name = varE idValName
        inspectTy beta = varE fmapValName `appE`
                           infixApp (if pos then makeFmapApply pos cRep conName beta name
                                            else conE applyDataName)
                                    (varE composeValName)
                                    (if pos then varE unApplyValName
                                            else makeFmapApply pos cRep conName beta name)

    itf <- isTyFamily tyCon
    if any (`mentionsName` [name]) lhsArgs
          || itf && any (`mentionsName` [name]) tyArgs
       then outOfPlaceTyVarError cRep conName
       else inspectTy (head rhsArgs)

applyDataName :: Name
applyDataName = mkNameG_d derivingCompatPackageKey "Data.Deriving.Internal" "Apply"

unApplyValName :: Name
unApplyValName = mkNameG_v derivingCompatPackageKey "Data.Deriving.Internal" "unApply"
# endif
#endif
