{-# LANGUAGE CPP #-}

{-|
Module:      Data.Deriving.Via.Internal
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Portability: Template Haskell

On @template-haskell-2.12@ or later (i.e., GHC 8.2 or later), this module
exports functionality which emulates the @GeneralizedNewtypeDeriving@ and
@DerivingVia@ GHC extensions.

On older versions of @template-haskell@/GHC, this module does not export
anything.
-}
module Data.Deriving.Via.Internal where

#if MIN_VERSION_template_haskell(2,12,0)
import           Control.Monad ((<=<))

import           Data.Deriving.Internal
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe (catMaybes)

import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Generates an instance for a type class at a newtype by emulating the
-- behavior of the @GeneralizedNewtypeDeriving@ extension. For example:
--
-- @
-- newtype Foo a = MkFoo a
-- $('deriveGND' [t| forall a. 'Eq' a => 'Eq' (Foo a) |])
-- @
deriveGND :: Q Type -> Q [Dec]
deriveGND qty = do
  ty <- qty
  let (instanceCxt, instanceTy) = decomposeType ty
  instanceTy' <- (resolveTypeSynonyms <=< resolveInfixT) instanceTy
  decs <- deriveViaDecs instanceTy' Nothing
  (:[]) `fmap` instanceD (return instanceCxt)
                         (return instanceTy)
                         (map return decs)

-- | Generates an instance for a type class by emulating the behavior of the
-- @DerivingVia@ extension. For example:
--
-- @
-- newtype Foo a = MkFoo a
-- $('deriveVia' [t| forall a. 'Ord' a => 'Ord' (Foo a) \`'Via'\` Down a |])
-- @
--
-- As shown in the example above, the syntax is a tad strange. One must specify
-- the type by which to derive the instance using the 'Via' type. This
-- requirement is in place to ensure that the type variables are scoped
-- correctly across all the types being used (e.g., to make sure that the same
-- @a@ is used in @'Ord' a@, @'Ord' (Foo a)@, and @Down a@).
deriveVia :: Q Type -> Q [Dec]
deriveVia qty = do
  ty <- qty
  let (instanceCxt, viaApp) = decomposeType ty
  viaApp' <- (resolveTypeSynonyms <=< resolveInfixT) viaApp
  (instanceTy, viaTy)
    <- case unapplyTy viaApp' of
         [via,instanceTy,viaTy]
           | via == ConT viaTypeName
          -> return (instanceTy, viaTy)
         _ -> fail $ unlines
                [ "Failure to meet ‘deriveVia‘ specification"
                , "\tThe ‘Via‘ type must be used, e.g."
                , "\t[t| forall a. C (T a) `Via` V a |]"
                ]
  decs <- deriveViaDecs instanceTy (Just viaTy)
  (:[]) `fmap` instanceD (return instanceCxt)
                         (return instanceTy)
                         (map return decs)

deriveViaDecs :: Type       -- ^ The instance head (e.g., @Eq (Foo a)@)
              -> Maybe Type -- ^ If using 'deriveGND', this is 'Nothing.
                            --   If using 'deriveVia', this is 'Just' the @via@ type.
              -> Q [Dec]
deriveViaDecs instanceTy mbViaTy = do
  let (clsTy:clsArgs) = unapplyTy instanceTy
  case clsTy of
    ConT clsName -> do
      clsInfo <- reify clsName
      case clsInfo of
        ClassI (ClassD _ _ clsTvbs _ clsDecs) _ ->
          case (unsnoc clsArgs, unsnoc clsTvbs) of
            (Just (_, dataApp), Just (_, clsLastTvb)) -> do
              let (dataTy:dataArgs)  = unapplyTy dataApp
                  clsLastTvbKind     = tvbKind clsLastTvb
                  (_, kindList)      = uncurryTy clsLastTvbKind
                  numArgsToEtaReduce = length kindList - 1
              repTy <-
                case mbViaTy of
                  Just viaTy -> return viaTy
                  Nothing ->
                    case dataTy of
                      ConT dataName -> do
                        DatatypeInfo { datatypeVars    = dataVars
                                     , datatypeVariant = dv
                                     , datatypeCons    = cons
                                     } <- reifyDatatype dataName
                        case newtypeRepType dv cons of
                          Just newtypeRepTy ->
                            case etaReduce numArgsToEtaReduce newtypeRepTy of
                              Just etaRepTy ->
                                let repTySubst =
                                      M.fromList $
                                      zipWith (\var arg -> (varTToName var, arg))
                                              dataVars dataArgs
                                in return $ applySubstitution repTySubst etaRepTy
                              Nothing -> etaReductionError instanceTy
                          Nothing -> fail $ "Not a newtype: " ++ nameBase dataName
                      _ -> fail $ "Not a data type: " ++ pprint dataTy
              catMaybes `fmap` traverse (deriveViaDec clsTvbs clsArgs repTy) clsDecs
            (_, _) -> fail $ "Cannot derive instance for nullary class " ++ pprint clsTy
        _ -> fail $ "Not a type class: " ++ pprint clsTy
    _ -> fail $ "Malformed instance: " ++ pprint instanceTy

deriveViaDec :: [TyVarBndr] -> [Type] -> Type -> Dec -> Q (Maybe Dec)
deriveViaDec clsTvbs clsArgs repTy = go
  where
    go :: Dec -> Q (Maybe Dec)

    go (OpenTypeFamilyD (TypeFamilyHead tfName tfTvbs _ _)) =
      let lhsSubst = zipTvbSubst clsTvbs clsArgs
          rhsSubst = zipTvbSubst clsTvbs $ changeLast clsArgs repTy
          tfTvbTys = map tvbToType tfTvbs
          tfLHSTys = map (applySubstitution lhsSubst) tfTvbTys
          tfRHSTys = map (applySubstitution rhsSubst) tfTvbTys
          tfRHSTy  = applyTy (ConT tfName) tfRHSTys
          tfInst   = TySynInstD tfName (TySynEqn tfLHSTys tfRHSTy)
      in return (Just tfInst)

    go (SigD methName methTy) =
      let (fromTy, toTy) = mkCoerceClassMethEqn clsTvbs clsArgs repTy $
                           stripOuterForallT methTy
          rhsExpr = VarE coerceValName `AppTypeE` fromTy
                                       `AppTypeE` toTy
                                       `AppE`     VarE methName
          meth = ValD (VarP methName)
                      (NormalB rhsExpr)
                      []
      in return (Just meth)

    go _ = return Nothing

mkCoerceClassMethEqn :: [TyVarBndr] -> [Type] -> Type -> Type -> (Type, Type)
mkCoerceClassMethEqn clsTvbs clsArgs repTy methTy
  = ( applySubstitution rhsSubst methTy
    , applySubstitution lhsSubst methTy
    )
  where
    lhsSubst = zipTvbSubst clsTvbs clsArgs
    rhsSubst = zipTvbSubst clsTvbs $ changeLast clsArgs repTy

zipTvbSubst :: [TyVarBndr] -> [Type] -> Map Name Type
zipTvbSubst tvbs = M.fromList . zipWith (\tvb ty -> (tvName tvb, ty)) tvbs

-- | Replace the last element of a list with another element.
changeLast :: [a] -> a -> [a]
changeLast []     _  = error "changeLast"
changeLast [_]    x  = [x]
changeLast (x:xs) x' = x : changeLast xs x'

stripOuterForallT :: Type -> Type
stripOuterForallT (ForallT _ _ ty) = ty
stripOuterForallT ty               = ty

decomposeType :: Type -> (Cxt, Type)
decomposeType (ForallT _ ctxt ty) = (ctxt, ty)
decomposeType ty                  = ([],   ty)

newtypeRepType :: DatatypeVariant -> [ConstructorInfo] -> Maybe Type
newtypeRepType dv cons = do
    checkIfNewtype
    case cons of
      [ConstructorInfo { constructorVars    = []
                       , constructorContext = []
                       , constructorFields  = [repTy]
                       }] -> Just repTy
      _ -> Nothing
  where
    checkIfNewtype :: Maybe ()
    checkIfNewtype
      | Newtype         <- dv = Just ()
      | NewtypeInstance <- dv = Just ()
      | otherwise             = Nothing

etaReduce :: Int -> Type -> Maybe Type
etaReduce num ty =
  let (tyHead:tyArgs) = unapplyTy ty
      (tyArgsRemaining, tyArgsDropped) = splitAt (length tyArgs - num) tyArgs
  in if canEtaReduce tyArgsRemaining tyArgsDropped
        then Just $ applyTy tyHead tyArgsRemaining
        else Nothing
#endif
