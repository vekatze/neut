module Language.WeakTerm.Reduce
  ( Handle,
    new,
    reduceType,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable (bimapM)
import Data.IORef
import Data.Text qualified as T
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Binder
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint qualified as H

type InlineLimit =
  Int

type CurrentStep =
  Int

data Handle = Handle
  { substHandle :: Subst.Handle,
    inlineLimit :: InlineLimit,
    currentStepRef :: IORef CurrentStep,
    location :: H.Hint
  }

new :: Subst.Handle -> H.Hint -> InlineLimit -> IO Handle
new substHandle location inlineLimit = do
  currentStepRef <- liftIO $ newIORef 0
  return $ Handle {..}

reduceType :: Handle -> WT.WeakType -> App WT.WeakType
reduceType h ty = do
  detectPossibleInfiniteLoop h
  liftIO $ incrementStep h
  case ty of
    _ :< WT.Tau ->
      return ty
    _ :< WT.TVar {} ->
      return ty
    _ :< WT.TVarGlobal {} ->
      return ty
    m :< WT.TyApp t args -> do
      t' <- reduceType h t
      args' <- mapM (reduceType h) args
      return $ m :< WT.TyApp t' args'
    m :< WT.Pi piKind impArgs expArgs defaultArgs cod -> do
      impArgs' <- mapM (reduceBinder h) impArgs
      expArgs' <- mapM (reduceBinder h) expArgs
      defaultArgs' <- mapM (bimapM (reduceBinder h) return) defaultArgs
      cod' <- reduceType h cod
      return $ m :< WT.Pi piKind impArgs' expArgs' defaultArgs' cod'
    m :< WT.Data attr name es -> do
      es' <- mapM (reduceType h) es
      attr' <- reduceAttrData h attr
      return $ m :< WT.Data attr' name es'
    m :< WT.Box t -> do
      t' <- reduceType h t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- reduceType h t
      return $ m :< WT.BoxNoema t'
    m :< WT.Code t -> do
      t' <- reduceType h t
      return $ m :< WT.Code t'
    _ :< WT.PrimType {} ->
      return ty
    _ :< WT.Void ->
      return ty
    m :< WT.Resource dd resourceID unitType discarder copier -> do
      unitType' <- reduceType h unitType
      return $ m :< WT.Resource dd resourceID unitType' discarder copier
    m :< WT.TypeHole hole es -> do
      es' <- mapM (reduceType h) es
      return $ m :< WT.TypeHole hole es'

reduceBinder :: Handle -> BinderF WT.WeakType -> App (BinderF WT.WeakType)
reduceBinder h (m, x, t) = do
  t' <- reduceType h t
  return (m, x, t')

detectPossibleInfiniteLoop :: Handle -> App ()
detectPossibleInfiniteLoop h = do
  let Handle {location, currentStepRef, inlineLimit} = h
  currentStep <- liftIO $ readIORef currentStepRef
  when (inlineLimit < currentStep) $ do
    raiseError location $ "Exceeded max recursion depth of " <> T.pack (show inlineLimit)

incrementStep :: Handle -> IO ()
incrementStep h = do
  let Handle {currentStepRef} = h
  modifyIORef' currentStepRef (+ 1)

reduceAttrData :: Handle -> AttrD.Attr name (BinderF WT.WeakType) -> App (AttrD.Attr name (BinderF WT.WeakType))
reduceAttrData h attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- forM consNameList $ \(cn, binders, cl) -> do
    binders' <- forM binders $ \(mx, x, t) -> do
      t' <- reduceType h t
      return (mx, x, t')
    return (cn, binders', cl)
  return $ attr {AttrD.consNameList = consNameList'}
