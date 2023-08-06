{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
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

import qualified Control.Applicative as App
import           Control.Monad (when, unless)

import qualified Data.Foldable as F
import           Data.Functor.Classes
                   ( Eq1(..), Ord1(..), Read1(..), Show1(..)
#if MIN_VERSION_base(4,10,0)
                   , liftReadListPrecDefault
#endif
                   )
#if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
import           Data.Functor.Classes
                   ( Eq2(..), Ord2(..), Read2(..), Show2(..)
#if MIN_VERSION_base(4,10,0)
                   , liftReadListPrec2Default
#endif
                   )
#endif
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.Monoid (Dual(..), Endo(..))
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Traversable as T

import           GHC.Arr (Ix(..))
import           GHC.Base (getTag)
import           GHC.Exts
import           GHC.Read (choose, list, paren)
import           GHC.Show (showSpace)
#if MIN_VERSION_base(4,19,0)
import           GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import           GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))
#endif

import           Text.ParserCombinators.ReadPrec
                   ( ReadPrec, (+++), pfail, prec, readPrec_to_S, readS_to_Prec
                   , reset, step
                   )
import           Text.Read (Read(..), parens, readListPrecDefault)
import qualified Text.Read.Lex as L
import           Text.Show (showListWith)

#if MIN_VERSION_base(4,7,0)
import           GHC.Read (expectP)
#else
import           GHC.Read (lexP)
import           Text.Read.Lex (Lexeme)
#endif

#if !(MIN_VERSION_base(4,8,0))
import           Control.Applicative (Applicative(..))
import           Data.Foldable (Foldable(..))
import           Data.Functor (Functor(..))
import           Data.Monoid (Monoid(..))
import           Data.Traversable (Traversable(..))
#endif

#if MIN_VERSION_base(4,10,0)
import           GHC.Show (showCommaSpace)
#endif

#if MIN_VERSION_base(4,11,0)
import           GHC.Read (readField, readSymField)
#endif

#if defined(MIN_VERSION_ghc_boot_th)
import           GHC.Lexeme (startsConSym, startsVarSym)
#else
import           Data.Char (isSymbol, ord)
#endif

import           Language.Haskell.TH.Datatype as Datatype
import           Language.Haskell.TH.Datatype.TyVarBndr
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr (pprint)
import           Language.Haskell.TH.Syntax

-- Ensure, beyond a shadow of a doubt, that the instances are in-scope
import           Data.Functor ()
import           Data.Functor.Classes ()
import           Data.Foldable ()
import           Data.Traversable ()

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
substNamesWithKindStar ns t = F.foldr' (flip substNameWithKind starK) t ns

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
    varTysExp <- T.mapM resolveTypeSynonyms varTysOrig

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
          map (substNamesWithKindStar (List.union droppedKindVarNames kvNames'))
            $ take remainingLength varTysOrig

    isDataFamily <-
      case variant of
        Datatype        -> return False
        Newtype         -> return False
        DataInstance    -> return True
        NewtypeInstance -> return True
#if MIN_VERSION_th_abstraction(0,5,0)
        Datatype.TypeData -> typeDataError tyConName
#endif

    let remainingTysOrigSubst' :: [Type]
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

typeDataError :: Name -> Q a
typeDataError dataName = fail
  . showString "Cannot derive instance for ‘"
  . showString (nameBase dataName)
  . showString "‘, which is a ‘type data‘ declaration"
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

#if !(MIN_VERSION_ghc_prim(0,3,1))
isTrue# :: Bool -> Bool
isTrue# x = x
{-# INLINE isTrue# #-}
#endif

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
isProductType :: NonEmpty ConstructorInfo -> Bool
isProductType (con :| []) = null (constructorVars con)
isProductType _           = False

-- | Returns 'True' if it's a datatype with one or more nullary, non-GADT
-- constructors.
isEnumerationType :: NonEmpty ConstructorInfo -> Bool
isEnumerationType cons = F.all (App.liftA2 (&&) isNullaryCon isVanillaCon) cons

-- | Returns 'False' if we're dealing with existential quantification or GADTs.
isVanillaCon :: ConstructorInfo -> Bool
isVanillaCon (ConstructorInfo { constructorContext = ctxt, constructorVars = vars }) =
  null ctxt && null vars

-- | Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = T.mapM (newName . (prefix ++) . show) [1..n]

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
applyTy = List.foldl' AppT

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
    lam1E (conP iHashDataName [varP iHash]) $
        varE tagToEnumHashValName `appE` varE iHash
            `sigE` return (quantifyType ty')
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
#if MIN_VERSION_base(4,16,0)
    , (int32HashTypeName,  ( ltInt32HashValName
                           , leInt32HashValName
                           , eqInt32HashValName
                           , geInt32HashValName
                           , gtInt32HashValName
                           ))
    , (word32HashTypeName, ( ltWord32HashValName
                           , leWord32HashValName
                           , eqWord32HashValName
                           , geWord32HashValName
                           , gtWord32HashValName
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
-- Quoted names
-------------------------------------------------------------------------------

-- With GHC 8.0 or later, we can simply use TemplateHaskellQuotes to quote each
-- name, which allows deriving-compat to be built with compilers that do not
-- support Template Haskell (e.g., stage-1 cross compilers). Unfortunately,
-- older versions of GHC must fall back on full-blown Template Haskell.

isTrueHashValName :: Name
isTrueHashValName = 'isTrue#

fmapConstValName :: Name
fmapConstValName = 'fmapConst

replaceConstValName :: Name
replaceConstValName = 'replaceConst

foldrConstValName :: Name
foldrConstValName = 'foldrConst

foldMapConstValName :: Name
foldMapConstValName = 'foldMapConst

nullConstValName :: Name
nullConstValName = 'nullConst

traverseConstValName :: Name
traverseConstValName = 'traverseConst

eqConstValName :: Name
eqConstValName = 'eqConst

eq1ConstValName :: Name
eq1ConstValName = 'eq1Const

liftEqConstValName :: Name
liftEqConstValName = 'liftEqConst

liftEq2ConstValName :: Name
liftEq2ConstValName = 'liftEq2Const

compareConstValName :: Name
compareConstValName = 'compareConst

ltConstValName :: Name
ltConstValName = 'ltConst

compare1ConstValName :: Name
compare1ConstValName = 'compare1Const

liftCompareConstValName :: Name
liftCompareConstValName = 'liftCompareConst

liftCompare2ConstValName :: Name
liftCompare2ConstValName = 'liftCompare2Const

readsPrecConstValName :: Name
readsPrecConstValName = 'readsPrecConst

readPrecConstValName :: Name
readPrecConstValName = 'readPrecConst

readsPrec1ConstValName :: Name
readsPrec1ConstValName = 'readsPrec1Const

liftReadsPrecConstValName :: Name
liftReadsPrecConstValName = 'liftReadsPrecConst

liftReadPrecConstValName :: Name
liftReadPrecConstValName = 'liftReadPrecConst

liftReadsPrec2ConstValName :: Name
liftReadsPrec2ConstValName = 'liftReadsPrec2Const

liftReadPrec2ConstValName :: Name
liftReadPrec2ConstValName = 'liftReadPrec2Const

showsPrecConstValName :: Name
showsPrecConstValName = 'showsPrecConst

showsPrec1ConstValName :: Name
showsPrec1ConstValName = 'showsPrec1Const

liftShowsPrecConstValName :: Name
liftShowsPrecConstValName = 'liftShowsPrecConst

liftShowsPrec2ConstValName :: Name
liftShowsPrec2ConstValName = 'liftShowsPrec2Const

viaTypeName :: Name
viaTypeName = ''Via

cHashDataName :: Name
cHashDataName = 'C#

dHashDataName :: Name
dHashDataName = 'D#

fHashDataName :: Name
fHashDataName = 'F#

identDataName :: Name
identDataName = 'L.Ident

iHashDataName :: Name
iHashDataName = 'I#

puncDataName :: Name
puncDataName = 'L.Punc

symbolDataName :: Name
symbolDataName = 'L.Symbol

wrapMonadDataName :: Name
wrapMonadDataName = 'App.WrapMonad

addrHashTypeName :: Name
addrHashTypeName = ''Addr#

boundedTypeName :: Name
boundedTypeName = ''Bounded

charHashTypeName :: Name
charHashTypeName = ''Char#

doubleHashTypeName :: Name
doubleHashTypeName = ''Double#

enumTypeName :: Name
enumTypeName = ''Enum

floatHashTypeName :: Name
floatHashTypeName = ''Float#

foldableTypeName :: Name
foldableTypeName = ''Foldable

functorTypeName :: Name
functorTypeName = ''Functor

intTypeName :: Name
intTypeName = ''Int

intHashTypeName :: Name
intHashTypeName = ''Int#

ixTypeName :: Name
ixTypeName = ''Ix

readTypeName :: Name
readTypeName = ''Read

showTypeName :: Name
showTypeName = ''Show

traversableTypeName :: Name
traversableTypeName = ''Traversable

wordHashTypeName :: Name
wordHashTypeName = ''Word#

altValName :: Name
altValName = '(+++)

appendValName :: Name
appendValName = '(++)

chooseValName :: Name
chooseValName = 'choose

composeValName :: Name
composeValName = '(.)

constValName :: Name
constValName = 'const

enumFromValName :: Name
enumFromValName = 'enumFrom

enumFromThenValName :: Name
enumFromThenValName = 'enumFromThen

enumFromThenToValName :: Name
enumFromThenToValName = 'enumFromThenTo

enumFromToValName :: Name
enumFromToValName = 'enumFromTo

eqAddrHashValName :: Name
eqAddrHashValName = 'eqAddr#

eqCharHashValName :: Name
eqCharHashValName = 'eqChar#

eqDoubleHashValName :: Name
eqDoubleHashValName = '(==##)

eqFloatHashValName :: Name
eqFloatHashValName = 'eqFloat#

eqIntHashValName :: Name
eqIntHashValName = '(==#)

eqWordHashValName :: Name
eqWordHashValName = 'eqWord#

errorValName :: Name
errorValName = 'error

flipValName :: Name
flipValName = 'flip

fmapValName :: Name
fmapValName = 'fmap

foldrValName :: Name
foldrValName = 'F.foldr

foldMapValName :: Name
foldMapValName = 'foldMap

fromEnumValName :: Name
fromEnumValName = 'fromEnum

geAddrHashValName :: Name
geAddrHashValName = 'geAddr#

geCharHashValName :: Name
geCharHashValName = 'geChar#

geDoubleHashValName :: Name
geDoubleHashValName = '(>=##)

geFloatHashValName :: Name
geFloatHashValName = 'geFloat#

geIntHashValName :: Name
geIntHashValName = '(>=#)

getTagValName :: Name
getTagValName = 'getTag

geWordHashValName :: Name
geWordHashValName = 'geWord#

gtAddrHashValName :: Name
gtAddrHashValName = 'gtAddr#

gtCharHashValName :: Name
gtCharHashValName = 'gtChar#

gtDoubleHashValName :: Name
gtDoubleHashValName = '(>##)

gtFloatHashValName :: Name
gtFloatHashValName = 'gtFloat#

gtIntHashValName :: Name
gtIntHashValName = '(>#)

gtWordHashValName :: Name
gtWordHashValName = 'gtWord#

idValName :: Name
idValName = 'id

indexValName :: Name
indexValName = 'index

inRangeValName :: Name
inRangeValName = 'inRange

leAddrHashValName :: Name
leAddrHashValName = 'leAddr#

leCharHashValName :: Name
leCharHashValName = 'leChar#

leDoubleHashValName :: Name
leDoubleHashValName = '(<=##)

leFloatHashValName :: Name
leFloatHashValName = 'leFloat#

leIntHashValName :: Name
leIntHashValName = '(<=#)

leWordHashValName :: Name
leWordHashValName = 'leWord#

listValName :: Name
listValName = 'list

ltAddrHashValName :: Name
ltAddrHashValName = 'ltAddr#

ltCharHashValName :: Name
ltCharHashValName = 'ltChar#

ltDoubleHashValName :: Name
ltDoubleHashValName = '(<##)

ltFloatHashValName :: Name
ltFloatHashValName = 'ltFloat#

ltIntHashValName :: Name
ltIntHashValName = '(<#)

ltWordHashValName :: Name
ltWordHashValName = 'ltWord#

minBoundValName :: Name
minBoundValName = 'minBound

mapValName :: Name
mapValName = 'map

maxBoundValName :: Name
maxBoundValName = 'maxBound

minusIntHashValName :: Name
minusIntHashValName = '(-#)

neqIntHashValName :: Name
neqIntHashValName = '(/=#)

parenValName :: Name
parenValName = 'paren

parensValName :: Name
parensValName = 'parens

pfailValName :: Name
pfailValName = 'pfail

plusValName :: Name
plusValName = '(+)

precValName :: Name
precValName = 'prec

predValName :: Name
predValName = 'pred

rangeSizeValName :: Name
rangeSizeValName = 'rangeSize

rangeValName :: Name
rangeValName = 'range

readFieldHash :: String -> ReadPrec a -> ReadPrec a
readFieldHash fieldName readVal = do
        expectP (L.Ident fieldName)
        expectP (L.Symbol "#")
        expectP (L.Punc "=")
        readVal
{-# NOINLINE readFieldHash #-}

readFieldHashValName :: Name
readFieldHashValName = 'readFieldHash

readListValName :: Name
readListValName = 'readList

readListPrecDefaultValName :: Name
readListPrecDefaultValName = 'readListPrecDefault

readListPrecValName :: Name
readListPrecValName = 'readListPrec

readPrec_to_SValName :: Name
readPrec_to_SValName = 'readPrec_to_S

readPrecValName :: Name
readPrecValName = 'readPrec

readS_to_PrecValName :: Name
readS_to_PrecValName = 'readS_to_Prec

readsPrecValName :: Name
readsPrecValName = 'readsPrec

replaceValName :: Name
replaceValName = '(<$)

resetValName :: Name
resetValName = 'reset

returnValName :: Name
returnValName = 'return

seqValName :: Name
seqValName = 'seq

showCharValName :: Name
showCharValName = 'showChar

showListValName :: Name
showListValName = 'showList

showListWithValName :: Name
showListWithValName = 'showListWith

showParenValName :: Name
showParenValName = 'showParen

showsPrecValName :: Name
showsPrecValName = 'showsPrec

showSpaceValName :: Name
showSpaceValName = 'showSpace

showStringValName :: Name
showStringValName = 'showString

stepValName :: Name
stepValName = 'step

succValName :: Name
succValName = 'succ

tagToEnumHashValName :: Name
tagToEnumHashValName = 'tagToEnum#

timesValName :: Name
timesValName = '(*)

toEnumValName :: Name
toEnumValName = 'toEnum

traverseValName :: Name
traverseValName = 'traverse

unsafeIndexValName :: Name
unsafeIndexValName = 'unsafeIndex

unsafeRangeSizeValName :: Name
unsafeRangeSizeValName = 'unsafeRangeSize

unwrapMonadValName :: Name
unwrapMonadValName = 'App.unwrapMonad

boolTypeName :: Name
boolTypeName = ''Bool

falseDataName :: Name
falseDataName = 'False

trueDataName :: Name
trueDataName = 'True

eqDataName :: Name
eqDataName = 'EQ

gtDataName :: Name
gtDataName = 'GT

ltDataName :: Name
ltDataName = 'LT

eqTypeName :: Name
eqTypeName = ''Eq

ordTypeName :: Name
ordTypeName = ''Ord

andValName :: Name
andValName = '(&&)

compareValName :: Name
compareValName = 'compare

eqValName :: Name
eqValName = '(==)

geValName :: Name
geValName = '(>=)

gtValName :: Name
gtValName = '(>)

leValName :: Name
leValName = '(<=)

ltValName :: Name
ltValName = '(<)

notValName :: Name
notValName = 'not

wHashDataName :: Name
wHashDataName = 'W#

#if !(MIN_VERSION_base(4,7,0))
expectP :: Lexeme -> ReadPrec ()
expectP lexeme = do
  thing <- lexP
  if thing == lexeme then return () else pfail
#endif

expectPValName :: Name
expectPValName = 'expectP

allValName :: Name
allValName = 'all

apValName :: Name
apValName = '(<*>)

pureValName :: Name
pureValName = 'pure

liftA2ValName :: Name
liftA2ValName = 'App.liftA2

mappendValName :: Name
mappendValName = 'mappend

memptyValName :: Name
memptyValName = 'mempty

nullValName :: Name
nullValName = 'null

eq1TypeName :: Name
eq1TypeName = ''Eq1

ord1TypeName :: Name
ord1TypeName = ''Ord1

read1TypeName :: Name
read1TypeName = ''Read1

show1TypeName :: Name
show1TypeName = ''Show1

#if !(MIN_VERSION_transformers(0,4,0)) || MIN_VERSION_transformers(0,5,0)
eq2TypeName :: Name
eq2TypeName = ''Eq2

ord2TypeName :: Name
ord2TypeName = ''Ord2

read2TypeName :: Name
read2TypeName = ''Read2

show2TypeName :: Name
show2TypeName = ''Show2

liftEqValName :: Name
liftEqValName = 'liftEq

liftEq2ValName :: Name
liftEq2ValName = 'liftEq2

liftCompareValName :: Name
liftCompareValName = 'liftCompare

liftCompare2ValName :: Name
liftCompare2ValName = 'liftCompare2

liftReadsPrecValName :: Name
liftReadsPrecValName = 'liftReadsPrec

liftReadListValName :: Name
liftReadListValName = 'liftReadList

liftReadsPrec2ValName :: Name
liftReadsPrec2ValName = 'liftReadsPrec2

liftReadList2ValName :: Name
liftReadList2ValName = 'liftReadList2

liftShowListValName :: Name
liftShowListValName = 'liftShowList

liftShowsPrecValName :: Name
liftShowsPrecValName = 'liftShowsPrec

liftShowList2ValName :: Name
liftShowList2ValName = 'liftShowList2

liftShowsPrec2ValName :: Name
liftShowsPrec2ValName = 'liftShowsPrec2
#else
eq1ValName :: Name
eq1ValName = 'eq1

compare1ValName :: Name
compare1ValName = 'compare1

readsPrec1ValName :: Name
readsPrec1ValName = 'readsPrec1

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
applyDataName = 'Apply

unApplyValName :: Name
unApplyValName = 'unApply
#endif

#if MIN_VERSION_base(4,7,0)
coerceValName :: Name
coerceValName = 'coerce
#endif

#if MIN_VERSION_base(4,10,0)
liftReadListPrecDefaultValName :: Name
liftReadListPrecDefaultValName = 'liftReadListPrecDefault

liftReadListPrec2DefaultValName :: Name
liftReadListPrec2DefaultValName = 'liftReadListPrec2Default

liftReadListPrecValName :: Name
liftReadListPrecValName = 'liftReadListPrec

liftReadListPrec2ValName :: Name
liftReadListPrec2ValName = 'liftReadListPrec2

liftReadPrecValName :: Name
liftReadPrecValName = 'liftReadPrec

liftReadPrec2ValName :: Name
liftReadPrec2ValName = 'liftReadPrec2
#else
-- This is a gross hack to avoid needing to guard some uses of these two Names
-- in Text.Read.Deriving.Internal with even grosser CPP.

liftReadListPrecDefaultValName :: Name
liftReadListPrecDefaultValName =
  error "using liftReadListPrecDefault before base-4.10.*"

liftReadListPrec2DefaultValName :: Name
liftReadListPrec2DefaultValName =
  error "using liftReadListPrec2Default before base-4.10.*"

liftReadListPrecValName :: Name
liftReadListPrecValName =
  error "using liftReadListPrec before base-4.10.*"

liftReadListPrec2ValName :: Name
liftReadListPrec2ValName =
  error "using liftReadListPrec2 before base-4.10.*"

liftReadPrecValName :: Name
liftReadPrecValName =
  error "using liftReadPrec before base-4.10.*"

liftReadPrec2ValName :: Name
liftReadPrec2ValName =
  error "using liftReadPrec2 before base-4.10.*"
#endif

#if !(MIN_VERSION_base(4,10,0))
showCommaSpace :: ShowS
showCommaSpace = showString ", "
#endif

showCommaSpaceValName :: Name
showCommaSpaceValName = 'showCommaSpace

appEndoValName :: Name
appEndoValName = 'appEndo

dualDataName :: Name
dualDataName = 'Dual

endoDataName :: Name
endoDataName = 'Endo

getDualValName :: Name
getDualValName = 'getDual

#if !(MIN_VERSION_base(4,11,0))
readField :: String -> ReadPrec a -> ReadPrec a
readField fieldName readVal = do
        expectP (L.Ident fieldName)
        expectP (L.Punc "=")
        readVal
{-# NOINLINE readField #-}

readSymField :: String -> ReadPrec a -> ReadPrec a
readSymField fieldName readVal = do
        expectP (L.Punc "(")
        expectP (L.Symbol fieldName)
        expectP (L.Punc ")")
        expectP (L.Punc "=")
        readVal
{-# NOINLINE readSymField #-}
#endif

readFieldValName :: Name
readFieldValName = 'readField

readSymFieldValName :: Name
readSymFieldValName = 'readSymField

#if MIN_VERSION_base(4,13,0)
eqInt8HashValName :: Name
eqInt8HashValName = 'eqInt8#

eqInt16HashValName :: Name
eqInt16HashValName = 'eqInt16#

eqWord8HashValName :: Name
eqWord8HashValName = 'eqWord8#

eqWord16HashValName :: Name
eqWord16HashValName = 'eqWord16#

geInt8HashValName :: Name
geInt8HashValName = 'geInt8#

geInt16HashValName :: Name
geInt16HashValName = 'geInt16#

geWord8HashValName :: Name
geWord8HashValName = 'geWord8#

geWord16HashValName :: Name
geWord16HashValName = 'geWord16#

gtInt8HashValName :: Name
gtInt8HashValName = 'gtInt8#

gtInt16HashValName :: Name
gtInt16HashValName = 'gtInt16#

gtWord8HashValName :: Name
gtWord8HashValName = 'gtWord8#

gtWord16HashValName :: Name
gtWord16HashValName = 'gtWord16#

int8HashTypeName :: Name
int8HashTypeName = ''Int8#

int8ToIntHashValName :: Name
int8ToIntHashValName =
# if MIN_VERSION_base(4,16,0)
  'int8ToInt#
# else
  'extendInt8#
# endif

int16HashTypeName :: Name
int16HashTypeName = ''Int16#

int16ToIntHashValName :: Name
int16ToIntHashValName =
# if MIN_VERSION_base(4,16,0)
  'int16ToInt#
# else
  'extendInt16#
# endif

intToInt8HashValName :: Name
intToInt8HashValName =
# if MIN_VERSION_base(4,16,0)
  'intToInt8#
# else
  'narrowInt8#
# endif

intToInt16HashValName :: Name
intToInt16HashValName =
# if MIN_VERSION_base(4,16,0)
  'intToInt16#
# else
  'narrowInt16#
# endif

leInt8HashValName :: Name
leInt8HashValName = 'leInt8#

leInt16HashValName :: Name
leInt16HashValName = 'leInt16#

leWord8HashValName :: Name
leWord8HashValName = 'leWord8#

leWord16HashValName :: Name
leWord16HashValName = 'leWord16#

ltInt8HashValName :: Name
ltInt8HashValName = 'ltInt8#

ltInt16HashValName :: Name
ltInt16HashValName = 'ltInt16#

ltWord8HashValName :: Name
ltWord8HashValName = 'ltWord8#

ltWord16HashValName :: Name
ltWord16HashValName = 'ltWord16#

word8HashTypeName :: Name
word8HashTypeName = ''Word8#

word8ToWordHashValName :: Name
word8ToWordHashValName =
# if MIN_VERSION_base(4,16,0)
  'word8ToWord#
# else
  'extendWord8#
# endif

word16HashTypeName :: Name
word16HashTypeName = ''Word16#

word16ToWordHashValName :: Name
word16ToWordHashValName =
# if MIN_VERSION_base(4,16,0)
  'word16ToWord#
# else
  'extendWord16#
# endif

wordToWord8HashValName :: Name
wordToWord8HashValName =
# if MIN_VERSION_base(4,16,0)
  'wordToWord8#
# else
  'narrowWord8#
# endif

wordToWord16HashValName :: Name
wordToWord16HashValName =
# if MIN_VERSION_base(4,16,0)
  'wordToWord16#
# else
  'narrowWord16#
# endif
#endif

#if MIN_VERSION_base(4,16,0)
eqInt32HashValName :: Name
eqInt32HashValName = 'eqInt32#

eqWord32HashValName :: Name
eqWord32HashValName = 'eqWord32#

geInt32HashValName :: Name
geInt32HashValName = 'geInt32#

geWord32HashValName :: Name
geWord32HashValName = 'geWord32#

gtInt32HashValName :: Name
gtInt32HashValName = 'gtInt32#

gtWord32HashValName :: Name
gtWord32HashValName = 'gtWord32#

int32HashTypeName :: Name
int32HashTypeName = ''Int32#

int32ToIntHashValName :: Name
int32ToIntHashValName = 'int32ToInt#

intToInt32HashValName :: Name
intToInt32HashValName = 'intToInt32#

leInt32HashValName :: Name
leInt32HashValName = 'leInt32#

leWord32HashValName :: Name
leWord32HashValName = 'leWord32#

ltInt32HashValName :: Name
ltInt32HashValName = 'ltInt32#

ltWord32HashValName :: Name
ltWord32HashValName = 'ltWord32#

word32HashTypeName :: Name
word32HashTypeName = ''Word32#

word32ToWordHashValName :: Name
word32ToWordHashValName = 'word32ToWord#

wordToWord32HashValName :: Name
wordToWord32HashValName = 'wordToWord32#
#endif

#if MIN_VERSION_base(4,19,0)
i8HashDataName :: Name
i8HashDataName = 'I8#

i16HashDataName :: Name
i16HashDataName = 'I16#

i32HashDataName :: Name
i32HashDataName = 'I32#

i64HashDataName :: Name
i64HashDataName = 'I64#

int64HashTypeName :: Name
int64HashTypeName = ''Int64#

w8HashDataName :: Name
w8HashDataName = 'W8#

w16HashDataName :: Name
w16HashDataName = 'W16#

w32HashDataName :: Name
w32HashDataName = 'W32#

w64HashDataName :: Name
w64HashDataName = 'W64#

word64HashTypeName :: Name
word64HashTypeName = ''Word64#
#endif
