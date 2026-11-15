module Language.WeakTerm.Subst
  ( Handle,
    new,
    TypeEntry (..),
    TypeSubst,
    makeTypeSubst,
    substType,
    substTermBinders,
  )
where

import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Gensym.Handle qualified as Gensym
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.WeakTerm.WeakTerm qualified as WT

data TypeEntry
  = TypeRenaming Ident
  | TypeValue WT.WeakType

type TypeSubst =
  IntMap.IntMap TypeEntry

makeTypeSubst :: [Ident] -> [WT.WeakType] -> TypeSubst
makeTypeSubst xs ts =
  IntMap.fromList $ zip (map Ident.toInt xs) (map TypeValue ts)

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

substType :: Handle -> TypeSubst -> WT.WeakType -> IO WT.WeakType
substType h sub ty =
  case ty of
    _ :< WT.Tau ->
      return ty
    m :< WT.TVar x ->
      case IntMap.lookup (Ident.toInt x) sub of
        Just (TypeRenaming x') ->
          return $ m :< WT.TVar x'
        Just (TypeValue t) ->
          return t
        Nothing ->
          return ty
    _ :< WT.TVarGlobal {} ->
      return ty
    m :< WT.TyApp t args -> do
      t' <- substType h sub t
      args' <- mapM (substType h sub) args
      return $ m :< WT.TyApp t' args'
    m :< WT.Pi piKind impParams expParams defaultParams t -> do
      (impParams', sub') <- substTypeBinders h sub impParams
      expParams' <- substTermBinders h sub' expParams
      defaultParams' <- substTermBinders h sub' defaultParams
      t' <- substType h sub' t
      return $ m :< WT.Pi piKind impParams' expParams' defaultParams' t'
    m :< WT.Data attr name es -> do
      es' <- mapM (substType h sub) es
      return $ m :< WT.Data attr name es'
    m :< WT.Box t -> do
      t' <- substType h sub t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- substType h sub t
      return $ m :< WT.BoxNoema t'
    m :< WT.Embed t -> do
      t' <- substType h sub t
      return $ m :< WT.Embed t'
    m :< WT.Code t -> do
      t' <- substType h sub t
      return $ m :< WT.Code t'
    _ :< WT.PrimType {} ->
      return ty
    _ :< WT.Void ->
      return ty
    m :< WT.Resource dd resourceID -> do
      return $ m :< WT.Resource dd resourceID
    m :< WT.TypeHole holeID es -> do
      es' <- mapM (substType h sub) es
      return $ m :< WT.TypeHole holeID es'

substTypeBinders ::
  Handle ->
  TypeSubst ->
  [BinderF WT.WeakType] ->
  IO ([BinderF WT.WeakType], TypeSubst)
substTypeBinders h sub binders =
  case binders of
    [] -> do
      return ([], sub)
    (m, k, x, t) : rest -> do
      t' <- substType h sub t
      x' <- Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (TypeRenaming x') sub
      (rest', sub'') <- substTypeBinders h sub' rest
      return ((m, k, x', t') : rest', sub'')

substTermBinders ::
  Handle ->
  TypeSubst ->
  [BinderF WT.WeakType] ->
  IO [BinderF WT.WeakType]
substTermBinders h sub =
  mapM $ \(m, k, x, t) -> do
    t' <- substType h sub t
    x' <- Gensym.newIdentFromIdent (gensymHandle h) x
    return (m, k, x', t')
