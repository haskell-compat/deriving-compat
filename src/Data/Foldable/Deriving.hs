{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

{-|
Module:      Data.Foldable.Deriving
Copyright:   (C) 2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

Exports functions to mechanically derive 'Foldable' instances in a way that mimics
how the @-XDeriveFoldable@ extension works since GHC 7.12. These changes make it
possible to derive @Foldable@ instances for data types with existential constraints,
e.g.,

@
&#123;-&#35; LANGUAGE DeriveFoldable, GADTs, StandaloneDeriving, TemplateHaskell &#35;-&#125;

data WrappedSet a where
    WrapSet :: Ord a => a -> WrappedSet a
deriving instance Foldable WrappedSet -- On GHC 7.12 on later
$(deriveFoldable ''WrappedSet)        -- On GHC 7.10 and earlier
@

For more info on these changes, see
<https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor this GHC wiki page>.
-}
module Data.Foldable.Deriving (
    -- * 'deriveFoldable'
    -- $derive
      deriveFoldable
    -- * @make@- functions
    -- $make
    , makeFoldMap
    , makeFoldr
  ) where

import Control.Monad (guard)

import Data.Deriving.Internal
#if MIN_VERSION_template_haskell(2,7,0)
import Data.List (find)
#endif
import Data.Maybe
#if __GLASGOW_HASKELL__ < 710 && MIN_VERSION_template_haskell(2,8,0)
import qualified Data.Set as Set
#endif

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- User-facing API
-------------------------------------------------------------------------------

{- $derive

'deriveFoldable' automatically generates a @Foldable@ instances for a given data
type, newtype, or data family instance that has at least one type variable. Examples:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import Data.Foldable.Deriving

data Pair a = Pair a a
$('deriveFoldable' ''Pair) -- instance Foldable Pair where ...

data Product f g a = Product (f a) (g a)
$('deriveFoldable' ''Product)
-- instance (Foldable f, Foldable g) => Foldable (Pair f g) where ...
@

If you are using @template-haskell-2.7.0.0@ or later (i.e., GHC 7.4 or later),
then @deriveFoldable@ can be used with data family instances (which requires the
@-XTypeFamilies@ extension). To do so, pass the name of a data or newtype instance
constructor (NOT a data family name!) to @deriveFoldable@.  Note that the
generated code may require the @-XFlexibleInstances@ extension. Example:

@
&#123;-&#35; LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies &#35;-&#125;
import Data.Foldable.Deriving

class AssocClass a b where
    data AssocData a b
instance AssocClass Int b where
    data AssocData Int b = AssocDataInt1 Int | AssocDataInt2 b
$('deriveFoldable' 'AssocDataInt1) -- instance Foldable (AssocData Int) where ...
-- Alternatively, one could use $(deriveFoldable 'AssocDataInt2)
@

Note that there are some limitations:

* The 'Name' argument must not be a type synonym.

* The last type variable must be of kind @*@. Other type variables of kind @* -> *@
  are assumed to require a 'Foldable' constraint. If your data type doesn't meet
  this assumption, use a @make@ function.

* If using the @-XDatatypeContexts@ extension, a constraint cannot mention the last
  type variable. For example, @data Illegal a where I :: Ord a => a -> Illegal a@
  cannot have a derived 'Foldable' instance.

* If the last type variable is used within a constructor argument's type, it must
  only be used in the last type argument. For example,
  @data Legal a b = Legal (Int, Int, a, b)@ can have a derived 'Foldable' instance,
  but @data Illegal a b = Illegal (a, b, a, b)@ cannot.

* Data family instances must be able to eta-reduce the last type variable. In other
  words, if you have a instance of the form:

  @
  data family Family a1 ... an t
  data instance Family e1 ... e2 v = ...
  @

  Then the following conditions must hold:

  1. @v@ must be a type variable.
  2. @v@ must not be mentioned in any of @e1@, ..., @e2@.

* In GHC 7.8, a bug exists that can cause problems when a data family declaration and
  one of its data instances use different type variables, e.g.,

  @
  data family Foo a b
  data instance Foo Int z = Foo Int z
  $(deriveFoldable 'Foo)
  @

  To avoid this issue, it is recommened that you use the same type variables in the
  same positions in which they appeared in the data family declaration:

  @
  data family Foo a b
  data instance Foo Int b = Foo Int b
  $(deriveFoldable 'Foo)
  @

-}

{- $make

There may be scenarios in which you want to, say, fold over an arbitrary data type
or data family instance without having to make the type an instance of 'Foldable'. For
these cases, this module provides several functions (all prefixed with @make@-) that
splice the appropriate lambda expression into your source code.

This is particularly useful for creating instances for sophisticated data types. For
example, 'deriveFoldable' cannot infer the correct type context for
@newtype HigherKinded f a b = HigherKinded (f a b)@, since @f@ is of kind
@* -> * -> *@. However, it is still possible to create a 'Foldable' instance for
@HigherKinded@ without too much trouble using 'makeFoldr':

@
&#123;-&#35; LANGUAGE FlexibleContexts, TemplateHaskell &#35;-&#125;
import Data.Foldable.Deriving

newtype HigherKinded f a b = HigherKinded (f a b)

instance Foldable (f a) => Foldable (HigherKinded f a) where
    foldr = $(makeFoldr ''HigherKinded)
@

-}

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Generates a 'Foldable' instance declaration for the given data type or data
-- family instance. This mimics how the @-XDeriveFoldable@ extension works since
-- GHC 7.12.
deriveFoldable :: Name -> Q [Dec]
deriveFoldable tyConName = do
  info <- reify tyConName
  case info of
    TyConI{} -> deriveFoldablePlainTy tyConName
#if MIN_VERSION_template_haskell(2,7,0)
    DataConI{} -> deriveFoldableDataFamInst tyConName
    FamilyI (FamilyD DataFam _ _ _) _ ->
      error $ ns ++ "Cannot use a data family name. Use a data family instance constructor instead."
    FamilyI (FamilyD TypeFam _ _ _) _ ->
      error $ ns ++ "Cannot use a type family name."
    _ -> error $ ns ++ "The name must be of a plain type constructor or data family instance constructor."
#else
    DataConI{} -> dataConIError
    _          -> error $ ns ++ "The name must be of a plain type constructor."
#endif
  where
    ns :: String
    ns = "Data.Foldable.Deriving.deriveFoldable: "

-- | Generates a Foldable instance declaration for a plain type constructor.
deriveFoldablePlainTy :: Name -> Q [Dec]
deriveFoldablePlainTy tyConName = withTyCon tyConName fromCons where
  fromCons :: Cxt -> [TyVarBndr] -> [Con] -> Q [Dec]
  fromCons ctxt tvbs cons = (:[]) `fmap`
    instanceD (return instanceCxt)
              (return $ AppT (ConT foldableTypeName) instanceType)
              (foldFunDecs droppedNb cons)
    where
      (instanceCxt, instanceType, droppedNb:_) =
        cxtAndTypePlainTy tyConName ctxt tvbs

#if MIN_VERSION_template_haskell(2,7,0)
-- | Generates a Foldable instance declaration for a data family instance constructor.
deriveFoldableDataFamInst :: Name -> Q [Dec]
deriveFoldableDataFamInst dataFamInstName = withDataFamInstCon dataFamInstName fromDec where
  fromDec :: [TyVarBndr] -> Cxt -> Name -> [Type] -> [Con] -> Q [Dec]
  fromDec famTvbs ctxt parentName instTys cons = (:[]) `fmap`
    instanceD (return instanceCxt)
              (return $ AppT (ConT foldableTypeName) instanceType)
              (foldFunDecs droppedNb cons)
    where
      (instanceCxt, instanceType, droppedNb:_) =
          cxtAndTypeDataFamInstCon parentName ctxt famTvbs instTys
#endif

-- | Generates a the function declarations for foldr and foldMap.
--
-- For why both foldr and foldMap are derived for Foldable, see Trac #7436.
foldFunDecs :: NameBase -> [Con] -> [Q Dec]
foldFunDecs nb cons = map makeFunD [Foldr, FoldMap] where
  makeFunD :: FoldFun -> Q Dec
  makeFunD fun =
    funD (foldFunName fun)
         [ clause []
                  (normalB $ makeFoldFunForCons fun nb cons)
                  []
         ]

-- | Generates a lambda expression which behaves like 'foldMap' (without requiring a
-- 'Foldable' instance). This mimics how the @-XDeriveFoldable@ extension works since
-- GHC 7.12.
makeFoldMap :: Name -> Q Exp
makeFoldMap = makeFoldFun FoldMap

-- | Generates a lambda expression which behaves like 'foldr' (without requiring a
-- 'Foldable' instance). This mimics how the @-XDeriveFoldable@ extension works since
-- GHC 7.12.
makeFoldr :: Name -> Q Exp
makeFoldr = makeFoldFun Foldr

-- | Generates a lambda expression which behaves like the FoldFun argument.
makeFoldFun :: FoldFun -> Name -> Q Exp
makeFoldFun fun tyConName = do
  info <- reify tyConName
  case info of
    TyConI{} -> withTyCon tyConName $ \ctxt tvbs decs ->
      let !nbs = thd3 $ cxtAndTypePlainTy tyConName ctxt tvbs
      in makeFoldFunForCons fun (head nbs) decs
#if MIN_VERSION_template_haskell(2,7,0)
    DataConI{} -> withDataFamInstCon tyConName $ \famTvbs ctxt parentName instTys cons ->
      let !nbs = thd3 $ cxtAndTypeDataFamInstCon parentName ctxt famTvbs instTys
      in makeFoldFunForCons fun (head nbs) cons
    FamilyI (FamilyD DataFam _ _ _) _ ->
      error $ ns ++ "Cannot use a data family name. Use a data family instance constructor instead."
    FamilyI (FamilyD TypeFam _ _ _) _ ->
      error $ ns ++ "Cannot use a type family name."
    _ -> error $ ns ++ "The name must be of a plain type constructor or data family instance constructor."
#else
    DataConI{} -> dataConIError
    _          -> error $ ns ++ "The name must be of a plain type constructor."
#endif
  where
    ns :: String
    ns = "Data.Foldable.Deriving.makeFoldFun: "

-- | Generates a lambda expression for the given constructors.
-- All constructors must be from the same type.
makeFoldFunForCons :: FoldFun -> NameBase -> [Con] -> Q Exp
makeFoldFunForCons fun nb cons = do
  argNames <- mapM newName $ catMaybes [ Just "f"
                                       , guard (fun == Foldr) >> Just "z"
                                       , Just "value"
                                       ]
  let f:others = argNames
      z        = head others -- If we're deriving foldr, this will be well defined
                             -- and useful. Otherwise, it'll be ignored.
      value    = last others
      mbTvi    = Just (nb, f)
  lamE (map varP argNames)
      . appsE
      $ [ varE $ foldFunConstName fun
        , if null cons
             then appE (varE errorValName)
                       (stringE $ "Void " ++ nameBase (foldFunName fun))
             else caseE (varE value)
                        (map (makeFoldFunForCon fun z mbTvi) cons)
        ] ++ map varE argNames

-- | Generates a lambda expression for a single constructor.
makeFoldFunForCon :: FoldFun -> Name -> Maybe TyVarInfo -> Con -> Q Match
makeFoldFunForCon fun z mbTvi (NormalC conName tys) = do
  args <- newNameList "arg" $ length tys
  let argTys = map snd tys
  makeFoldFunForArgs fun z mbTvi conName argTys args
makeFoldFunForCon fun z mbTvi (RecC conName tys) = do
  args <- newNameList "arg" $ length tys
  let argTys = map thd3 tys
  makeFoldFunForArgs fun z mbTvi conName argTys args
makeFoldFunForCon fun z mbTvi (InfixC (_, argTyL) conName (_, argTyR)) = do
  argL <- newName "argL"
  argR <- newName "argR"
  makeFoldFunForArgs fun z mbTvi conName [argTyL, argTyR] [argL, argR]
makeFoldFunForCon fun z mbTvi (ForallC tvbs _ con)
  = makeFoldFunForCon fun z (removeForalled tvbs mbTvi) con

-- | Generates a lambda expression for a single constructor's arguments.
makeFoldFunForArgs :: FoldFun
                   -> Name
                   -> Maybe TyVarInfo
                   -> Name
                   -> [Type]
                   -> [Name]
                   ->  Q Match
makeFoldFunForArgs fun z mbTvi conName tys args =
  match (conP conName $ map varP args)
        (normalB $ foldFunCombine fun z mappedArgs)
        []
  where
    mappedArgs :: [Q Exp]
    mappedArgs = zipWith (makeFoldFunForArg fun mbTvi conName) tys args

-- | Generates a lambda expression for a single argument of a constructor.
makeFoldFunForArg :: FoldFun
                  -> Maybe TyVarInfo
                  -> Name
                  -> Type
                  -> Name
                  -> Q Exp
makeFoldFunForArg fun mbTvi conName ty tyExpName = do
  ty' <- expandSyn ty
  makeFoldFunForType fun mbTvi conName ty' `appE` varE tyExpName

-- | Generates a lambda expression for a specific type.
makeFoldFunForType :: FoldFun
                   -> Maybe TyVarInfo
                   -> Name
                   -> Type
                   -> Q Exp
makeFoldFunForType fun mbTvi _ (VarT tyName) =
    maybe (foldFunTriv fun) (\(nb, mapName) ->
      if NameBase tyName == nb
         then varE mapName
         else foldFunTriv fun) mbTvi
makeFoldFunForType fun mbTvi conName (SigT ty _) =
  makeFoldFunForType fun mbTvi conName ty
makeFoldFunForType fun mbTvi conName (ForallT tvbs _ ty) =
  makeFoldFunForType fun (removeForalled tvbs mbTvi) conName ty
makeFoldFunForType fun mbTvi conName ty =
  let tyCon  :: Type
      tyArgs :: [Type]
      tyCon:tyArgs = unapplyTy ty

      numLastArgs :: Int
      numLastArgs = min 1 $ length tyArgs

      lhsArgs, rhsArgs :: [Type]
      (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

      tyVarNameBase :: [NameBase]
      tyVarNameBase = maybeToList $ fmap fst mbTvi

      mentionsTyArgs :: Bool
      mentionsTyArgs = any (`mentionsNameBase` tyVarNameBase) tyArgs

      makeFoldFunTuple :: Type -> Name -> Q Exp
      makeFoldFunTuple fieldTy fieldName =
        makeFoldFunForType fun mbTvi conName fieldTy `appE` varE fieldName

   in case tyCon of
     ArrowT -> noFunctionsError conName
     TupleT n
       | n > 0 && mentionsTyArgs -> do
         args <- mapM newName $ catMaybes [ Just "x"
                                          , guard (fun == Foldr) >> Just "z"
                                          ]
         xs <- newNameList "tup" n

         let x = head args
             z = last args
         lamE (map varP args) $ caseE (varE x)
              [ match (tupP $ map varP xs)
                      (normalB $ foldFunCombine fun
                                                z
                                                (zipWith makeFoldFunTuple tyArgs xs)
                      )
                      []
              ]
     _ -> do
         itf <- isTyFamily tyCon
         if any (`mentionsNameBase` tyVarNameBase) lhsArgs || (itf && mentionsTyArgs)
           then outOfPlaceTyVarError conName (head tyVarNameBase)
           else if any (`mentionsNameBase` tyVarNameBase) rhsArgs
                  then foldFunApp fun . appsE $
                         ( varE (foldFunName fun)
                         : map (makeFoldFunForType fun mbTvi conName) rhsArgs
                         )
                  else foldFunTriv fun

-------------------------------------------------------------------------------
-- Template Haskell reifying and AST manipulation
-------------------------------------------------------------------------------

-- | Extracts a plain type constructor's information.
withTyCon :: Name
          -> (Cxt -> [TyVarBndr] -> [Con] -> Q a)
          -> Q a
withTyCon name f = do
  info <- reify name
  case info of
    TyConI dec ->
      case dec of
        DataD    ctxt _ tvbs cons _ -> f ctxt tvbs cons
        NewtypeD ctxt _ tvbs con  _ -> f ctxt tvbs [con]
        _ -> error $ ns ++ "Unsupported type " ++ show dec ++ ". Must be a data type or newtype."
    _ -> error $ ns ++ "The name must be of a plain type constructor."
  where
    ns :: String
    ns = "Data.Foldable.Deriving.withTyCon: "

#if MIN_VERSION_template_haskell(2,7,0)
-- | Extracts a data family name's information.
withDataFam :: Name
            -> ([TyVarBndr] -> [Dec] -> Q a)
            -> Q a
withDataFam name f = do
  info <- reify name
  case info of
    FamilyI (FamilyD DataFam _ tvbs _) decs -> f tvbs decs
    FamilyI (FamilyD TypeFam _ _    _) _    -> error $ ns ++ "Cannot use a type family name."
    _ -> error $ ns ++ "Unsupported type " ++ show info ++ ". Must be a data family name."
  where
    ns :: String
    ns = "Data.Foldable.Deriving.withDataFam: "

-- | Extracts a data family instance constructor's information.
withDataFamInstCon :: Name
                   -> ([TyVarBndr] -> Cxt -> Name -> [Type] -> [Con] -> Q a)
                   -> Q a
withDataFamInstCon dficName f = do
  dficInfo <- reify dficName
  case dficInfo of
    DataConI _ _ parentName _ -> do
      parentInfo <- reify parentName
      case parentInfo of
        FamilyI (FamilyD DataFam _ _ _) _ -> withDataFam parentName $ \famTvbs decs ->
          let sameDefDec = flip find decs $ \dec ->
                case dec of
                  DataInstD    _ _ _ cons' _ -> any ((dficName ==) . constructorName) cons'
                  NewtypeInstD _ _ _ con   _ -> dficName == constructorName con
                  _ -> error $ ns ++ "Must be a data or newtype instance."

              (ctxt, instTys, cons) = case sameDefDec of
                Just (DataInstD    ctxt' _ instTys' cons' _) -> (ctxt', instTys', cons')
                Just (NewtypeInstD ctxt' _ instTys' con   _) -> (ctxt', instTys', [con])
                _ -> error $ ns ++ "Could not find data or newtype instance constructor."

          in f famTvbs ctxt parentName instTys cons
        _ -> error $ ns ++ "Data constructor " ++ show dficName ++ " is not from a data family instance."
    _ -> error $ ns ++ "Unsupported type " ++ show dficInfo ++ ". Must be a data family instance constructor."
  where
    ns :: String
    ns = "Data.Foldable.Deriving.withDataFamInstCon: "
#endif

-- | Deduces the instance context, instance head, and eta-reduced type variables
-- for a plain data type constructor.
cxtAndTypePlainTy :: Name        -- The datatype's name
                  -> Cxt         -- The datatype context
                  -> [TyVarBndr] -- The type variables
                  -> (Cxt, Type, [NameBase])
cxtAndTypePlainTy tyConName dataCxt tvbs
  | remainingLength < 0 || not (wellKinded droppedKinds) -- If we have a well-kinded type variable
  = derivingKindError tyConName
  | any (`predMentionsNameBase` droppedNbs) dataCxt -- If the last type variable is mentioned in a datatype context
  = datatypeContextError tyConName instanceType
  | otherwise = (instanceCxt, instanceType, droppedNbs)
  where
    instanceCxt :: Cxt
    instanceCxt = mapMaybe applyConstraint remaining

    instanceType :: Type
    instanceType = applyTyCon tyConName $ map (VarT . tvbName) remaining

    remainingLength :: Int
    remainingLength = length tvbs - 1

    remaining, dropped :: [TyVarBndr]
    (remaining, dropped) = splitAt remainingLength tvbs

    droppedKinds :: [Kind]
    droppedKinds = map tvbKind dropped

    droppedNbs :: [NameBase]
    droppedNbs = map (NameBase . tvbName) dropped

#if MIN_VERSION_template_haskell(2,7,0)
-- | Deduces the instance context, instance head, and eta-reduced type variable
-- for a data family instance constructor.
cxtAndTypeDataFamInstCon :: Name        -- The data family name
                         -> Cxt         -- The datatype context
                         -> [TyVarBndr] -- The data family declaration's type variables
                         -> [Type]      -- The data family instance types
                         -> (Cxt, Type, [NameBase])
cxtAndTypeDataFamInstCon parentName dataCxt famTvbs instTysAndKinds
  | remainingLength < 0 || not (wellKinded droppedKinds) -- If we have a well-kinded type variable
  = derivingKindError parentName
  | any (`predMentionsNameBase` droppedNbs) dataCxt -- If the last type variable is mentioned in a datatype context
  = datatypeContextError parentName instanceType
  | canEtaReduce remaining dropped -- If it is safe to drop the type variable
  = (instanceCxt, instanceType, droppedNbs)
  | otherwise = etaReductionError instanceType
  where
    instanceCxt :: Cxt
    instanceCxt = mapMaybe applyConstraint lhsTvbs

    -- We need to make sure that type variables in the instance head which have
    -- constraints aren't poly-kinded, e.g.,
    --
    -- @
    -- instance Foldable f => Foldable (Foo (f :: k)) where
    -- @
    --
    -- To do this, we remove every kind ascription (i.e., strip off every 'SigT').
    instanceType :: Type
    instanceType = applyTyCon parentName
                 $ map unSigT remaining

    remainingLength :: Int
    remainingLength = length famTvbs - 1

    remaining, dropped :: [Type]
    (remaining, dropped) = splitAt remainingLength rhsTypes

    droppedKinds :: [Kind]
    droppedKinds = map tvbKind . snd $ splitAt remainingLength famTvbs

    droppedNbs :: [NameBase]
    droppedNbs = map varTToNameBase dropped

    -- We need to be mindful of an old GHC bug which causes kind variables to appear in
    -- @instTysAndKinds@ (as the name suggests) if
    --
    --   (1) @PolyKinds@ is enabled
    --   (2) either GHC 7.6 or 7.8 is being used (for more info, see Trac #9692).
    --
    -- Since Template Haskell doesn't seem to have a mechanism for detecting which
    -- language extensions are enabled, we do the next-best thing by counting
    -- the number of distinct kind variables in the data family declaration, and
    -- then dropping that number of entries from @instTysAndKinds@.
    instTypes :: [Type]
    instTypes =
# if __GLASGOW_HASKELL__ >= 710 || !(MIN_VERSION_template_haskell(2,8,0))
      instTysAndKinds
# else
      drop (Set.size . Set.unions $ map (distinctKindVars . tvbKind) famTvbs)
        instTysAndKinds
# endif

    lhsTvbs :: [TyVarBndr]
    lhsTvbs = map (uncurry replaceTyVarName)
            . filter (isTyVar . snd)
            . take remainingLength
            $ zip famTvbs rhsTypes

    -- In GHC 7.8, only the @Type@s up to the rightmost non-eta-reduced type variable
    -- in @instTypes@ are provided (as a result of a bug reported in Trac #9692). This
    -- is pretty inconvenient, as it makes it impossible to come up with the correct
    -- instance types in some cases. For example, consider the following code:
    --
    -- @
    -- data family Foo a b
    -- data instance Foo Int z = Foo Int z
    -- $(deriveFoldable 'Foo)
    -- @
    --
    -- Due to the aformentioned bug, Template Haskell doesn't tell us the names of
    -- the type variable in the data instance (@z@). As a result, we won't know to which
    -- fields of the 'Foo' constructor to apply the map functions, which will result
    -- in an incorrect instance. Urgh.
    --
    -- A workaround is to ensure that you use the exact same type variables, in the
    -- exact same order, in the data family declaration and any data or newtype
    -- instances:
    --
    -- @
    -- data family Foo a b
    -- data instance Foo Int b = Foo Int b
    -- $(deriveFoldable 'Foo)
    -- @
    --
    -- Thankfully, other versions of GHC don't seem to have this bug.
    rhsTypes :: [Type]
    rhsTypes =
# if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
      instTypes ++ map tvbToType (drop (length instTypes) famTvbs)
# else
      instTypes
# endif
#endif

-- | Given a TyVarBndr, apply a Foldable constraint to it if it has the right kind.
applyConstraint :: TyVarBndr -> Maybe Pred
applyConstraint (PlainTV  _)         = Nothing
applyConstraint (KindedTV name kind) = do
  guard $ numKindArrows kind == 1 && canRealizeKindStarChain kind
  Just $ applyClass foldableTypeName name

-------------------------------------------------------------------------------
-- Error messages
-------------------------------------------------------------------------------

-- | Either the given data type doesn't have a type variable, or the type variable
-- to be eta-reduced cannot realize kind *.
derivingKindError :: Name -> a
derivingKindError tyConName = error
  . showString "Cannot derive well-kinded instance of form ‘Foldable "
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass Foldable expects an argument of kind * -> *"
  $ ""

-- | A constructor has a function argument.
noFunctionsError :: Name -> a
noFunctionsError conName = error
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must not contain function types"
  $ ""

-- | The data type has a DatatypeContext which mentions the eta-reduced type variable.
datatypeContextError :: Name -> Type -> a
datatypeContextError dataName instanceType = error
  . showString "Can't make a derived instance of ‘"
  . showString (pprint instanceType)
  . showString "‘:\n\tData type ‘"
  . showString (nameBase dataName)
  . showString "‘ must not have a class context involving the last type argument"
  $ ""

-- | The data type mentions the eta-reduced type variable in a place other
-- than the last position of a data type in a constructor's field.
outOfPlaceTyVarError :: Name -> NameBase -> a
outOfPlaceTyVarError conName tyVarName = error
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must use the type variable "
  . shows tyVarName
  . showString " only in the last argument of a data type"
  $ ""

#if MIN_VERSION_template_haskell(2,7,0)
-- | The last type variable cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> a
etaReductionError instanceType = error $
  "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
  ++ pprint instanceType
#else
-- | Template Haskell didn't list all of a data family's instances upon reification
-- until template-haskell-2.7.0.0, which is necessary for a derived instance to work.
dataConIError :: a
dataConIError = error
  . showString "Cannot use a data constructor."
  . showString "\n\t(Note: if you are trying to derive for a data family instance,"
  . showString "\n\tuse GHC >= 7.4 instead.)"
  $ ""
#endif

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which function is being generated.
data FoldFun = Foldr | FoldMap
  deriving Eq

foldFunConstName :: FoldFun -> Name
foldFunConstName Foldr   = foldrConstValName
foldFunConstName FoldMap = foldMapConstValName

foldFunName :: FoldFun -> Name
foldFunName Foldr   = foldrValName
foldFunName FoldMap = foldMapValName

-- See Trac #7436 for why explicit lambdas are used
foldFunTriv :: FoldFun -> Q Exp
foldFunTriv Foldr = do
  z <- newName "z"
  lamE [wildP, varP z] $ varE z
foldFunTriv FoldMap = lamE [wildP] $ varE memptyValName

foldFunApp :: FoldFun -> Q Exp -> Q Exp
foldFunApp Foldr e = do
  x <- newName "x"
  z <- newName "z"
  lamE [varP x, varP z] $ appsE [e, varE z, varE x]
foldFunApp FoldMap e = e

foldFunCombine :: FoldFun -> Name -> [Q Exp] -> Q Exp
foldFunCombine Foldr    = foldrCombine
foldFunCombine FoldMap  = foldMapCombine

foldrCombine :: Name -> [Q Exp] -> Q Exp
foldrCombine zName = foldr appE (varE zName)

foldMapCombine :: Name -> [Q Exp] -> Q Exp
foldMapCombine _ [] = varE memptyValName
foldMapCombine _ es = foldr1 (appE . appE (varE mappendValName)) es
