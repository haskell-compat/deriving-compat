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
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Template Haskell-related utilities.

Note: this is an internal module, and as such, the API presented here is not
guaranteed to be stable, even between minor releases of this library.
-}
module Data.Deriving.Internal where

import           Control.Applicative (liftA2)
import           Control.Monad (when, unless)

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
import qualified Data.Traversable as T

import           Text.ParserCombinators.ReadPrec (ReadPrec)
import qualified Text.Read.Lex as L

#if MIN_VERSION_base(4,7,0)
import           GHC.Read (expectP)
#else
import           GHC.Read (lexP)

import           Text.Read (pfail)
import           Text.Read.Lex (Lexeme)
#endif

#if MIN_VERSION_ghc_prim(0,3,1)
import           GHC.Prim (Int#, tagToEnum#)
#endif

#if defined(MIN_VERSION_ghc_boot_th)
import           GHC.Lexeme (startsConSym, startsVarSym)
#else
import           Data.Char (isSymbol, ord)
#endif

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Datatype.TyVarBndr
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

applySubstitutionKind :: Map Name Kind -> Type -> Type
#if MIN_VERSION_template_haskell(2,8,0)
applySubstitutionKind = applySubstitution
#else
applySubstitutionKind _ t = t
#endif

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = applySubstitutionKind (Map.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (flip substNameWithKind starK) t ns

-------------------------------------------------------------------------------
-- Via
-------------------------------------------------------------------------------

-- | A type-level modifier intended to be used in conjunction with 'deriveVia'.
-- Refer to the documentation for 'deriveVia' for more details.
data a `Via` b
infix 0 `Via`

-------------------------------------------------------------------------------
-- Type-specialized const functions
-------------------------------------------------------------------------------

fmapConst :: f b -> (a -> b) -> f a -> f b
fmapConst x _ _ = x
{-# INLINE fmapConst #-}

replaceConst :: f a -> a -> f b -> f a
replaceConst x _ _ = x
{-# INLINE replaceConst #-}

foldrConst :: b -> (a -> b -> b) -> b -> t a -> b
foldrConst x _ _ _ = x
{-# INLINE foldrConst #-}

foldMapConst :: m -> (a -> m) -> t a -> m
foldMapConst x _ _ = x
{-# INLINE foldMapConst #-}

nullConst :: Bool -> t a -> Bool
nullConst x _ = x
{-# INLINE nullConst #-}

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

-- For the given Types, generate an instance context and head. Coming up with
-- the instance type isn't as simple as dropping the last types, as you need to
-- be wary of kinds being instantiated with *.
-- See Note [Type inference in derived instances]
buildTypeInstance :: ClassRep a
                  => a
                  -- ^ The typeclass for which an instance should be derived
                  -> Name
                  -- ^ The type constructor or data family name
                  -> Cxt
                  -- ^ The datatype context
                  -> [Type]
                  -- ^ The types to instantiate the instance with
                  -> DatatypeVariant
                  -- ^ Are we dealing with a data family instance or not
                  -> Q (Cxt, Type)
buildTypeInstance cRep tyConName dataCxt varTysOrig variant = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM resolveTypeSynonyms varTysOrig

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
        droppedTyVarNames = freeVariables droppedTysExpSubst

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

        isDataFamily :: Bool
        isDataFamily = case variant of
                         Datatype        -> False
                         Newtype         -> False
                         DataInstance    -> True
                         NewtypeInstance -> True

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

checkExistentialContext :: ClassRep a => a -> TyVarMap b -> Cxt -> Name
                        -> Q c -> Q c
checkExistentialContext cRep tvMap ctxt conName q =
  if (any (`predMentionsName` Map.keys tvMap) ctxt
       || Map.size tvMap < arity cRep)
       && not (allowExQuant cRep)
     then existentialContextError conName
     else q

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

enumerationError :: String -> Q a
enumerationError = fail . enumerationErrorStr

enumerationOrProductError :: String -> Q a
enumerationOrProductError nb = fail $ unlines
    [ enumerationErrorStr nb
    , "\tor a product type (precisely one constructor)"
    ]

enumerationErrorStr :: String -> String
enumerationErrorStr nb =
    '\'':nb ++ "’ must be an enumeration type"
            ++ " (one or more nullary, non-GADT constructors)"

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
isTrue# :: Int# -> Bool
isTrue# x = tagToEnum# x
#else
isTrue# :: Bool -> Bool
isTrue# x = x
#endif
{-# INLINE isTrue# #-}

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

-- | @hasKindVarChain n kind@ Checks if @kind@ is of the form
-- k_0 -> k_1 -> ... -> k_(n-1), where k0, k1, ..., and k_(n-1) can be * or
-- kind variables.
hasKindVarChain :: Int -> Type -> Maybe [Name]
hasKindVarChain kindArrows t =
  let uk = uncurryKind (tyKind t)
  in if (length uk - 1 == kindArrows) && all isStarOrVar uk
        then Just (freeVariables uk)
        else Nothing

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
tyKind :: Type -> Kind
tyKind (SigT _ k) = k
tyKind _ = starK

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

unsnoc :: [a] -> Maybe ([a], a)
unsnoc []     = Nothing
unsnoc (x:xs) = case unsnoc xs of
                  Nothing    -> Just ([], x)
                  Just (a,b) -> Just (x:a, b)

isNullaryCon :: ConstructorInfo -> Bool
isNullaryCon (ConstructorInfo { constructorFields = tys }) = null tys

-- | Returns the number of fields for the constructor.
conArity :: ConstructorInfo -> Int
conArity (ConstructorInfo { constructorFields = tys }) = length tys

-- | Returns 'True' if it's a datatype with exactly one, non-existential constructor.
isProductType :: [ConstructorInfo] -> Bool
isProductType [con] = null (constructorVars con)
isProductType _     = False

-- | Returns 'True' if it's a datatype with one or more nullary, non-GADT
-- constructors.
isEnumerationType :: [ConstructorInfo] -> Bool
isEnumerationType cons@(_:_) = all (liftA2 (&&) isNullaryCon isVanillaCon) cons
isEnumerationType _          = False

-- | Returns 'False' if we're dealing with existential quantification or GADTs.
isVanillaCon :: ConstructorInfo -> Bool
isVanillaCon (ConstructorInfo { constructorContext = ctxt, constructorVars = vars }) =
  null ctxt && null vars

-- | Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = mapM (newName . (prefix ++) . show) [1..n]

-- | Extracts the kind from a TyVarBndr.
tvbKind :: TyVarBndr_ flag -> Kind
tvbKind = elimTV (\_ -> starK) (\_ k -> k)

-- | Convert a TyVarBndr to a Type.
tvbToType :: TyVarBndr_ flag -> Type
tvbToType = elimTV VarT (\n k -> SigT (VarT n) k)

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

-- | Detect if a Name in a list of provided Names occurs as an argument to some
-- type family. This makes an effort to exclude /oversaturated/ arguments to
-- type families. For instance, if one declared the following type family:
--
-- @
-- type family F a :: Type -> Type
-- @
--
-- Then in the type @F a b@, we would consider @a@ to be an argument to @F@,
-- but not @b@.
isInTypeFamilyApp :: [Name] -> Type -> [Type] -> Q Bool
isInTypeFamilyApp names tyFun tyArgs =
  case tyFun of
    ConT tcName -> go tcName
    _           -> return False
  where
    go :: Name -> Q Bool
    go tcName = do
      info <- reify tcName
      case info of
#if MIN_VERSION_template_haskell(2,11,0)
        FamilyI (OpenTypeFamilyD (TypeFamilyHead _ bndrs _ _)) _
          -> withinFirstArgs bndrs
#elif MIN_VERSION_template_haskell(2,7,0)
        FamilyI (FamilyD TypeFam _ bndrs _) _
          -> withinFirstArgs bndrs
#else
        TyConI (FamilyD TypeFam _ bndrs _)
          -> withinFirstArgs bndrs
#endif

#if MIN_VERSION_template_haskell(2,11,0)
        FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ bndrs _ _) _) _
          -> withinFirstArgs bndrs
#elif MIN_VERSION_template_haskell(2,9,0)
        FamilyI (ClosedTypeFamilyD _ bndrs _ _) _
          -> withinFirstArgs bndrs
#endif

        _ -> return False
      where
        withinFirstArgs :: [a] -> Q Bool
        withinFirstArgs bndrs =
          let firstArgs = take (length bndrs) tyArgs
              argFVs    = freeVariables firstArgs
          in return $ any (`elem` argFVs) names

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
unapplyTy :: Type -> (Type, [Type])
unapplyTy ty = go ty ty []
  where
    go :: Type -> Type -> [Type] -> (Type, [Type])
    go _      (AppT ty1 ty2)     args = go ty1 ty1 (ty2:args)
    go origTy (SigT ty' _)       args = go origTy ty' args
#if MIN_VERSION_template_haskell(2,11,0)
    go origTy (InfixT ty1 n ty2) args = go origTy (ConT n `AppT` ty1 `AppT` ty2) args
    go origTy (ParensT ty')      args = go origTy ty' args
#endif
    go origTy _                  args = (origTy, args)

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

tag2ConExpr :: Type -> Q Exp
tag2ConExpr ty = do
    iHash  <- newName "i#"
    ty' <- freshenType ty
    let tvbs = avoidTypeInType $ freeVariablesWellScoped [ty']
    lam1E (conP iHashDataName [varP iHash]) $
        varE tagToEnumHashValName `appE` varE iHash
            `sigE` return (ForallT (changeTVFlags SpecifiedSpec tvbs) [] ty')
            -- tagToEnum# is a hack, and won't typecheck unless it's in the
            -- immediate presence of a type ascription like so:
            --
            --   tagToEnum# x :: Foo
            --
            -- We have to be careful when dealing with datatypes with type
            -- variables, since Template Haskell might reject the type variables
            -- we use for being out-of-scope. To avoid this, we explicitly
            -- collect the type variable binders and shove them into a ForallT
            -- (using th-abstraction's quantifyType function). Also make sure
            -- to freshen the bound type variables to avoid shadowed variable
            -- warnings on old versions of GHC when -Wall is enabled.
  where
    -- Somewhat annoyingly, it's possible to generate code that requires
    -- TypeInType (on old versions of GHC) for data types which didn't require
    -- TypeInType to define. To avoid users having to turn on more language
    -- extensions than is necessary, we filter out all kind variable binders.
    -- Fortunately, old versions of GHC are quite alright with implicitly
    -- quantifying kind variables, even in the type of a SigE.
    --
    -- This is rather tiresome, and while writing this function, I debated
    -- whether to just forget about this nonsense and require users to
    -- enable TypeInType to use the generated code. Alas, that would entail
    -- a breaking change, so I decided against it at the time. If we ever make
    -- some breaking change in the future, however, this would be at the top
    -- of the list of things that I'd rip out.
    avoidTypeInType :: [TyVarBndrUnit] -> [TyVarBndrUnit]
#if __GLASGOW_HASKELL__ >= 806
    avoidTypeInType = id
#else
    avoidTypeInType = go . map attachFreeKindVars
      where
        attachFreeKindVars :: TyVarBndrUnit -> (TyVarBndrUnit, [Name])
        attachFreeKindVars tvb = (tvb, freeVariables (tvKind tvb))

        go :: [(TyVarBndrUnit, [Name])] -> [TyVarBndrUnit]
        go [] = []
        go ((tvb, _):tvbsAndFVs)
          | any (\(_, kindVars) -> tvName tvb `elem` kindVars) tvbsAndFVs
          = tvbs'
          | otherwise
          = tvb:tvbs'
          where
            tvbs' = go tvbsAndFVs
#endif

primOrdFunTbl :: Map Name (Name, Name, Name, Name, Name)
primOrdFunTbl = Map.fromList
    [ (addrHashTypeName,   ( ltAddrHashValName
                           , leAddrHashValName
                           , eqAddrHashValName
                           , geAddrHashValName
                           , gtAddrHashValName
                           ))
    , (charHashTypeName,   ( ltCharHashValName
                           , leCharHashValName
                           , eqCharHashValName
                           , geCharHashValName
                           , gtCharHashValName
                           ))
    , (doubleHashTypeName, ( ltDoubleHashValName
                           , leDoubleHashValName
                           , eqDoubleHashValName
                           , geDoubleHashValName
                           , gtDoubleHashValName
                           ))
    , (floatHashTypeName,  ( ltFloatHashValName
                           , leFloatHashValName
                           , eqFloatHashValName
                           , geFloatHashValName
                           , gtFloatHashValName
                           ))
    , (intHashTypeName,    ( ltIntHashValName
                           , leIntHashValName
                           , eqIntHashValName
                           , geIntHashValName
                           , gtIntHashValName
                           ))
    , (wordHashTypeName,   ( ltWordHashValName
                           , leWordHashValName
                           , eqWordHashValName
                           , geWordHashValName
                           , gtWordHashValName
                           ))
#if MIN_VERSION_base(4,13,0)
    , (int8HashTypeName,   ( ltInt8HashValName
                           , leInt8HashValName
                           , eqInt8HashValName
                           , geInt8HashValName
                           , gtInt8HashValName
                           ))
    , (int16HashTypeName,  ( ltInt16HashValName
                           , leInt16HashValName
                           , eqInt16HashValName
                           , geInt16HashValName
                           , gtInt16HashValName
                           ))
    , (word8HashTypeName,  ( ltWord8HashValName
                           , leWord8HashValName
                           , eqWord8HashValName
                           , geWord8HashValName
                           , gtWord8HashValName
                           ))
    , (word16HashTypeName, ( ltWord16HashValName
                           , leWord16HashValName
                           , eqWord16HashValName
                           , geWord16HashValName
                           , gtWord16HashValName
                           ))
#endif
    ]

removeClassApp :: Type -> Type
removeClassApp (AppT _ t2) = t2
removeClassApp t           = t

-- This is an ugly, but unfortunately necessary hack on older versions of GHC which
-- don't have a properly working newName. On those GHCs, even running newName on a
-- variable isn't enought to avoid shadowed variable warnings, so we "fix" the issue by
-- appending an uncommonly used string to the end of the name. This isn't foolproof,
-- since a user could freshen a variable named x and still have another x_' variable in
-- scope, but at least it's unlikely.
freshen :: Name -> Q Name
freshen n = newName (nameBase n ++ "_'")

freshenType :: Type -> Q Type
freshenType t =
  do let xs = [(n, VarT `fmap` freshen n) | n <- freeVariables t]
     subst <- T.sequence (Map.fromList xs)
     return (applySubstitution subst t)

enumFromToExpr :: Q Exp -> Q Exp -> Q Exp
enumFromToExpr f t = varE enumFromToValName `appE` f `appE` t

primOpAppExpr :: Q Exp -> Name -> Q Exp -> Q Exp
primOpAppExpr e1 op e2 = varE isTrueHashValName `appE`
                           infixApp e1 (varE op) e2

-- | Checks if a 'Name' represents a tuple type constructor (other than '()')
isNonUnitTuple :: Name -> Bool
isNonUnitTuple = isNonUnitTupleString . nameBase

-- | Checks if a 'String' represents a tuple (other than '()')
isNonUnitTupleString :: String -> Bool
isNonUnitTupleString ('(':',':_) = True
isNonUnitTupleString _           = False

-- | Checks if a 'String' names a valid Haskell infix data constructor (i.e., does
-- it begin with a colon?).
isInfixDataCon :: String -> Bool
isInfixDataCon (':':_) = True
isInfixDataCon _       = False

isSym :: String -> Bool
isSym ""      = False
isSym (c : _) = startsVarSym c || startsConSym c

#if !defined(MIN_VERSION_ghc_boot_th)
startsVarSym, startsConSym :: Char -> Bool
startsVarSym c = startsVarSymASCII c || (ord c > 0x7f && isSymbol c) -- Infix Ids
startsConSym c = c == ':' -- Infix data constructors

startsVarSymASCII :: Char -> Bool
startsVarSymASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"
#endif

ghc7'8OrLater :: Bool
#if __GLASGOW_HASKELL__ >= 708
ghc7'8OrLater = True
#else
ghc7'8OrLater = False
#endif

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

gHC_IX :: String
#if MIN_VERSION_base(4,14,0)
gHC_IX = "GHC.Ix"
#else
gHC_IX = "GHC.Arr"
#endif

mkDerivingCompatName_v :: String -> Name
mkDerivingCompatName_v = mkNameG_v derivingCompatPackageKey "Data.Deriving.Internal"

mkDerivingCompatName_tc :: String -> Name
mkDerivingCompatName_tc = mkNameG_tc derivingCompatPackageKey "Data.Deriving.Internal"

isTrueHashValName :: Name
isTrueHashValName = mkDerivingCompatName_v "isTrue#"

fmapConstValName :: Name
fmapConstValName = mkDerivingCompatName_v "fmapConst"

replaceConstValName :: Name
replaceConstValName = mkDerivingCompatName_v "replaceConst"

foldrConstValName :: Name
foldrConstValName = mkDerivingCompatName_v "foldrConst"

foldMapConstValName :: Name
foldMapConstValName = mkDerivingCompatName_v "foldMapConst"

nullConstValName :: Name
nullConstValName = mkDerivingCompatName_v "nullConst"

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

viaTypeName :: Name
viaTypeName = mkDerivingCompatName_tc "Via"

cHashDataName :: Name
cHashDataName = mkNameG_d "ghc-prim" "GHC.Types" "C#"

dHashDataName :: Name
dHashDataName = mkNameG_d "ghc-prim" "GHC.Types" "D#"

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

boundedTypeName :: Name
boundedTypeName = mkNameG_tc "base" "GHC.Enum" "Bounded"

charHashTypeName :: Name
charHashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Char#"

doubleHashTypeName :: Name
doubleHashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Double#"

enumTypeName :: Name
enumTypeName = mkNameG_tc "base" "GHC.Enum" "Enum"

floatHashTypeName :: Name
floatHashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Float#"

foldableTypeName :: Name
foldableTypeName = mkNameG_tc "base" "Data.Foldable" "Foldable"

functorTypeName :: Name
functorTypeName = mkNameG_tc "base" "GHC.Base" "Functor"

intTypeName :: Name
intTypeName = mkNameG_tc "ghc-prim" "GHC.Types" "Int"

intHashTypeName :: Name
intHashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Int#"

ixTypeName :: Name
ixTypeName = mkNameG_tc "base" gHC_IX "Ix"

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

appendValName :: Name
appendValName = mkNameG_v "base" "GHC.Base" "++"

chooseValName :: Name
chooseValName = mkNameG_v "base" "GHC.Read" "choose"

coerceValName :: Name
coerceValName = mkNameG_v "ghc-prim" "GHC.Prim" "coerce"

composeValName :: Name
composeValName = mkNameG_v "base" "GHC.Base" "."

constValName :: Name
constValName = mkNameG_v "base" "GHC.Base" "const"

enumFromValName :: Name
enumFromValName = mkNameG_v "base" "GHC.Enum" "enumFrom"

enumFromThenValName :: Name
enumFromThenValName = mkNameG_v "base" "GHC.Enum" "enumFromThen"

enumFromThenToValName :: Name
enumFromThenToValName = mkNameG_v "base" "GHC.Enum" "enumFromThenTo"

enumFromToValName :: Name
enumFromToValName = mkNameG_v "base" "GHC.Enum" "enumFromTo"

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

fromEnumValName :: Name
fromEnumValName = mkNameG_v "base" "GHC.Enum" "fromEnum"

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

indexValName :: Name
indexValName = mkNameG_v "base" gHC_IX "index"

inRangeValName :: Name
inRangeValName = mkNameG_v "base" gHC_IX "inRange"

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

minBoundValName :: Name
minBoundValName = mkNameG_v "base" "GHC.Enum" "minBound"

mapValName :: Name
mapValName = mkNameG_v "base" "GHC.Base" "map"

maxBoundValName :: Name
maxBoundValName = mkNameG_v "base" "GHC.Enum" "maxBound"

minusIntHashValName :: Name
minusIntHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "-#"

parenValName :: Name
parenValName = mkNameG_v "base" "GHC.Read" "paren"

parensValName :: Name
parensValName = mkNameG_v "base" "GHC.Read" "parens"

pfailValName :: Name
pfailValName = mkNameG_v "base" "Text.ParserCombinators.ReadPrec" "pfail"

plusValName :: Name
plusValName = mkNameG_v "base" "GHC.Num" "+"

precValName :: Name
precValName = mkNameG_v "base" "Text.ParserCombinators.ReadPrec" "prec"

predValName :: Name
predValName = mkNameG_v "base" "GHC.Enum" "pred"

rangeSizeValName :: Name
rangeSizeValName = mkNameG_v "base" gHC_IX "rangeSize"

rangeValName :: Name
rangeValName = mkNameG_v "base" gHC_IX "range"

readFieldHash :: String -> ReadPrec a -> ReadPrec a
readFieldHash fieldName readVal = do
        expectP (L.Ident fieldName)
        expectP (L.Symbol "#")
        expectP (L.Punc "=")
        readVal
{-# NOINLINE readFieldHash #-}

readFieldHashValName :: Name
readFieldHashValName = mkNameG_v derivingCompatPackageKey "Data.Deriving.Internal" "readFieldHash"

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

replaceValName :: Name
replaceValName = mkNameG_v "base" "GHC.Base" "<$"

resetValName :: Name
resetValName = mkNameG_v "base" "Text.ParserCombinators.ReadPrec" "reset"

returnValName :: Name
returnValName = mkNameG_v "base" "GHC.Base" "return"

seqValName :: Name
seqValName = mkNameG_v "ghc-prim" "GHC.Prim" "seq"

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

succValName :: Name
succValName = mkNameG_v "base" "GHC.Enum" "succ"

tagToEnumHashValName :: Name
tagToEnumHashValName = mkNameG_v "ghc-prim" "GHC.Prim" "tagToEnum#"

timesValName :: Name
timesValName = mkNameG_v "base" "GHC.Num" "*"

toEnumValName :: Name
toEnumValName = mkNameG_v "base" "GHC.Enum" "toEnum"

traverseValName :: Name
traverseValName = mkNameG_v "base" "Data.Traversable" "traverse"

unsafeIndexValName :: Name
unsafeIndexValName = mkNameG_v "base" gHC_IX "unsafeIndex"

unsafeRangeSizeValName :: Name
unsafeRangeSizeValName = mkNameG_v "base" gHC_IX "unsafeRangeSize"

unwrapMonadValName :: Name
unwrapMonadValName = mkNameG_v "base" "Control.Applicative" "unwrapMonad"

#if MIN_VERSION_base(4,4,0)
boolTypeName :: Name
boolTypeName = mkNameG_tc "ghc-prim" "GHC.Types" "Bool"

falseDataName :: Name
falseDataName = mkNameG_d "ghc-prim" "GHC.Types" "False"

trueDataName :: Name
trueDataName = mkNameG_d "ghc-prim" "GHC.Types" "True"
#else
boolTypeName :: Name
boolTypeName = mkNameG_tc "ghc-prim" "GHC.Bool" "Bool"

falseDataName :: Name
falseDataName = mkNameG_d "ghc-prim" "GHC.Bool" "False"

trueDataName :: Name
trueDataName = mkNameG_d "ghc-prim" "GHC.Bool" "True"
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
allValName :: Name
allValName = mkNameG_v "base" "Data.Foldable" "all"

apValName :: Name
apValName = mkNameG_v "base" "GHC.Base" "<*>"

pureValName :: Name
pureValName = mkNameG_v "base" "GHC.Base" "pure"

liftA2ValName :: Name
liftA2ValName = mkNameG_v "base" "GHC.Base" "liftA2"

mappendValName :: Name
mappendValName = mkNameG_v "base" "GHC.Base" "mappend"

memptyValName :: Name
memptyValName = mkNameG_v "base" "GHC.Base" "mempty"

nullValName :: Name
nullValName = mkNameG_v "base" "Data.Foldable" "null"
#else
allValName :: Name
allValName = mkNameG_v "base" "GHC.List" "all"

apValName :: Name
apValName = mkNameG_v "base" "Control.Applicative" "<*>"

pureValName :: Name
pureValName = mkNameG_v "base" "Control.Applicative" "pure"

liftA2ValName :: Name
liftA2ValName = mkNameG_v "base" "Control.Applicative" "liftA2"

mappendValName :: Name
mappendValName = mkNameG_v "base" "Data.Monoid" "mappend"

memptyValName :: Name
memptyValName = mkNameG_v "base" "Data.Monoid" "mempty"

nullValName :: Name
nullValName = mkNameG_v "base" "GHC.List" "null"
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
        (tyCon, tyArgs) = unapplyTy t

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

    itf <- isInTypeFamilyApp [name] tyCon tyArgs
    if any (`mentionsName` [name]) lhsArgs || itf
       then outOfPlaceTyVarError cRep conName
       else inspectTy (head rhsArgs)

applyDataName :: Name
applyDataName = mkNameG_d derivingCompatPackageKey "Data.Deriving.Internal" "Apply"

unApplyValName :: Name
unApplyValName = mkNameG_v derivingCompatPackageKey "Data.Deriving.Internal" "unApply"
# endif
#endif

#if MIN_VERSION_base(4,10,0)
showCommaSpaceValName :: Name
showCommaSpaceValName = mkNameG_v "base" "GHC.Show" "showCommaSpace"
#else
showCommaSpace :: ShowS
showCommaSpace = showString ", "

showCommaSpaceValName :: Name
showCommaSpaceValName = mkNameG_v derivingCompatPackageKey "Data.Deriving.Internal" "showCommaSpace"
#endif

#if MIN_VERSION_base(4,11,0)
appEndoValName :: Name
appEndoValName = mkNameG_v "base" "Data.Semigroup.Internal" "appEndo"

dualDataName :: Name
dualDataName = mkNameG_d "base" "Data.Semigroup.Internal" "Dual"

endoDataName :: Name
endoDataName = mkNameG_d "base" "Data.Semigroup.Internal" "Endo"

getDualValName :: Name
getDualValName = mkNameG_v "base" "Data.Semigroup.Internal" "getDual"

readFieldValName :: Name
readFieldValName = mkNameG_v "base" "GHC.Read" "readField"

readSymFieldValName :: Name
readSymFieldValName = mkNameG_v "base" "GHC.Read" "readSymField"
#else
appEndoValName :: Name
appEndoValName = mkNameG_v "base" "Data.Monoid" "appEndo"

dualDataName :: Name
dualDataName = mkNameG_d "base" "Data.Monoid" "Dual"

endoDataName :: Name
endoDataName = mkNameG_d "base" "Data.Monoid" "Endo"

getDualValName :: Name
getDualValName = mkNameG_v "base" "Data.Monoid" "getDual"

readField :: String -> ReadPrec a -> ReadPrec a
readField fieldName readVal = do
        expectP (L.Ident fieldName)
        expectP (L.Punc "=")
        readVal
{-# NOINLINE readField #-}

readFieldValName :: Name
readFieldValName = mkNameG_v derivingCompatPackageKey "Data.Deriving.Internal" "readField"

readSymField :: String -> ReadPrec a -> ReadPrec a
readSymField fieldName readVal = do
        expectP (L.Punc "(")
        expectP (L.Symbol fieldName)
        expectP (L.Punc ")")
        expectP (L.Punc "=")
        readVal
{-# NOINLINE readSymField #-}

readSymFieldValName :: Name
readSymFieldValName = mkNameG_v derivingCompatPackageKey "Data.Deriving.Internal" "readSymField"
#endif

#if MIN_VERSION_base(4,13,0)
eqInt8HashValName :: Name
eqInt8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "eqInt8#"

eqInt16HashValName :: Name
eqInt16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "eqInt16#"

eqWord8HashValName :: Name
eqWord8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "eqWord8#"

eqWord16HashValName :: Name
eqWord16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "eqWord16#"

extendInt8HashValName :: Name
extendInt8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "extendInt8#"

extendInt16HashValName :: Name
extendInt16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "extendInt16#"

extendWord8HashValName :: Name
extendWord8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "extendWord8#"

extendWord16HashValName :: Name
extendWord16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "extendWord16#"

geInt8HashValName :: Name
geInt8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "geInt8#"

geInt16HashValName :: Name
geInt16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "geInt16#"

geWord8HashValName :: Name
geWord8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "geWord8#"

geWord16HashValName :: Name
geWord16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "geWord16#"

gtInt8HashValName :: Name
gtInt8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "gtInt8#"

gtInt16HashValName :: Name
gtInt16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "gtInt16#"

gtWord8HashValName :: Name
gtWord8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "gtWord8#"

gtWord16HashValName :: Name
gtWord16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "gtWord16#"

int8HashTypeName :: Name
int8HashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Int8#"

int16HashTypeName :: Name
int16HashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Int16#"

leInt8HashValName :: Name
leInt8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "leInt8#"

leInt16HashValName :: Name
leInt16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "leInt16#"

leWord8HashValName :: Name
leWord8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "leWord8#"

leWord16HashValName :: Name
leWord16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "leWord16#"

ltInt8HashValName :: Name
ltInt8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "ltInt8#"

ltInt16HashValName :: Name
ltInt16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "ltInt16#"

ltWord8HashValName :: Name
ltWord8HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "ltWord8#"

ltWord16HashValName :: Name
ltWord16HashValName = mkNameG_v "ghc-prim" "GHC.Prim" "ltWord16#"

word8HashTypeName :: Name
word8HashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Word8#"

word16HashTypeName :: Name
word16HashTypeName = mkNameG_tc "ghc-prim" "GHC.Prim" "Word16#"
#endif
