module Kernel.Elaborate.Internal.TypeUtil
  ( inlineType,
    lookupDataInfoFull,
    getConsParamTypes,
  )
where

import App.App (App)
import App.Run (raiseCritical, raiseError)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Elaborate.Internal.Handle.Elaborate
import Language.Common.Binder
import Language.Common.DataInfo qualified as DI
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.Inline qualified as Inline
import Language.Term.Subst qualified as TmSubst
import Language.Term.Term qualified as TM
import Logger.Hint

inlineType :: Handle -> Hint -> TM.Type -> App TM.Type
inlineType h m t = do
  env <- liftIO $ inlineEnv h
  inlineHandle <- liftIO $ Inline.new env m False
  Inline.inlineType inlineHandle t

lookupDataInfoFull :: Handle -> Hint -> DD.DefiniteDescription -> App (DI.DataInfo (BinderF TM.Type))
lookupDataInfoFull h m dataName = do
  dataInfoOrNone <- liftIO $ Data.lookup (dataHandle h) dataName
  case dataInfoOrNone of
    Just dataInfo ->
      return dataInfo
    Nothing ->
      raiseError m $
        "could not find the layout of the type `"
          <> ModulePath.renderDD (modulePathMap h) dataName
          <> "`"

getConsParamTypes ::
  Handle ->
  Hint ->
  DD.DefiniteDescription ->
  [TM.Type] ->
  App [DI.ConsInfo (BinderF TM.Type)]
getConsParamTypes h m dataName dataArgs = do
  dataInfo <- lookupDataInfoFull h m dataName
  let dataParams = DI.dataParams dataInfo
  if length dataParams == length dataArgs
    then do
      let sub = TmSubst.makeTypeSubst (map getBinderIdent dataParams) dataArgs
      forM (DI.consInfoList dataInfo) $ \consInfo -> do
        let consImpParams = DI.consImpParams consInfo
        (consImpParams', sub') <- liftIO $ TmSubst.refreshTypeBinders (termSubstHandle h) sub consImpParams
        consParams' <- liftIO $ substConsParams h sub' (DI.consParams consInfo)
        return consInfo {DI.consImpParams = consImpParams', DI.consParams = consParams'}
    else
      raiseCritical m $
        "Could not specialize constructor metadata for `" <> DD.reify dataName <> "` due to arity mismatch"

substConsParams :: Handle -> TmSubst.TypeSubst -> [BinderF TM.Type] -> IO [BinderF TM.Type]
substConsParams h sub consParams =
  forM consParams $ \(m, k, x, t) -> do
    t' <- TmSubst.substType (termSubstHandle h) sub t
    return (m, k, x, t')
