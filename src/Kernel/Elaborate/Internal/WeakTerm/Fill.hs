module Kernel.Elaborate.Internal.WeakTerm.Fill
  ( Handle,
    new,
    fillType,
  )
where

import App.App (App)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Kernel.Elaborate.TypeHoleSubst qualified as THS
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Binder
import Language.Common.Ident.Reify qualified as Ident
import Language.WeakTerm.Reduce qualified as Reduce
import Language.WeakTerm.Subst (SubstEntry (..))
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.ToText qualified as WT
import Language.WeakTerm.WeakTerm qualified as WT
import Prelude hiding (lookup)

data Handle = Handle
  { substHandle :: Subst.Handle,
    reduceHandle :: Reduce.Handle
  }

new :: Subst.Handle -> Reduce.Handle -> Handle
new substHandle reduceHandle = do
  Handle {..}

fillType :: Handle -> THS.TypeHoleSubst -> WT.WeakType -> App WT.WeakType
fillType h holeSubst ty =
  case ty of
    _ :< WT.Tau ->
      return ty
    _ :< WT.TVar {} ->
      return ty
    _ :< WT.TVarGlobal {} ->
      return ty
    m :< WT.TyApp t args -> do
      t' <- fillType h holeSubst t
      args' <- mapM (fillType h holeSubst) args
      return $ m :< WT.TyApp t' args'
    m :< WT.Pi piKind impArgs expArgs defaultArgs t -> do
      impArgs' <- fillTypeBinder h holeSubst impArgs
      expArgs' <- fillTypeBinder h holeSubst expArgs
      defaultArgs' <- fillTypeBinder h holeSubst defaultArgs
      t' <- fillType h holeSubst t
      return $ m :< WT.Pi piKind impArgs' expArgs' defaultArgs' t'
    m :< WT.Data attr name es -> do
      es' <- mapM (fillType h holeSubst) es
      attr' <- fillAttrData h holeSubst attr
      return $ m :< WT.Data attr' name es'
    m :< WT.Box t -> do
      t' <- fillType h holeSubst t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- fillType h holeSubst t
      return $ m :< WT.BoxNoema t'
    m :< WT.Code t -> do
      t' <- fillType h holeSubst t
      return $ m :< WT.Code t'
    _ :< WT.PrimType {} ->
      return ty
    _ :< WT.Void ->
      return ty
    m :< WT.Resource dd resourceID -> do
      return $ m :< WT.Resource dd resourceID
    m :< WT.TypeHole i es -> do
      es' <- mapM (fillType h holeSubst) es
      case THS.lookup i holeSubst of
        Just (xs, body)
          | length xs == length es' -> do
              let varList = map Ident.toInt xs
              let sub = IntMap.fromList $ zip varList (map Type es')
              liftIO (Subst.substType (substHandle h) sub body) >>= Reduce.reduceType (reduceHandle h)
          | otherwise -> do
              let tyText = WT.toTextType ty
              error $ "Rule.WeakTerm.Fill (assertion failure; arity mismatch)\nhole = ?M" ++ show i ++ "(" ++ show xs ++ ")\nhole id = " ++ show i ++ "\ninput: " <> T.unpack tyText
        Nothing ->
          return $ m :< WT.TypeHole i es'

fillTypeBinder ::
  Handle ->
  THS.TypeHoleSubst ->
  [BinderF WT.WeakType] ->
  App [BinderF WT.WeakType]
fillTypeBinder h holeSubst binder =
  case binder of
    [] -> do
      return []
    (m, x, t) : xts -> do
      t' <- fillType h holeSubst t
      xts' <- fillTypeBinder h holeSubst xts
      return $ (m, x, t') : xts'

fillAttrData :: Handle -> THS.TypeHoleSubst -> AttrD.Attr name (BinderF WT.WeakType) -> App (AttrD.Attr name (BinderF WT.WeakType))
fillAttrData h holeSubst attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- forM consNameList $ \(cn, binders, cl) -> do
    binders' <- forM binders $ \(mx, x, t) -> do
      t' <- fillType h holeSubst t
      return (mx, x, t')
    return (cn, binders', cl)
  return $ attr {AttrD.consNameList = consNameList'}
