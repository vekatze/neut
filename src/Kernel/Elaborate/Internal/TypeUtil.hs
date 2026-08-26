module Kernel.Elaborate.Internal.TypeUtil
  ( inlineType,
    lookupDataInfoFull,
    getConsArgTypes,
  )
where

import App.App (App)
import App.Run (raiseCritical, raiseError)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.IntMap qualified as IntMap
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Elaborate.Internal.Handle.Elaborate
import Language.Common.Binder
import Language.Common.DataInfo qualified as DI
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident.Reify qualified as Ident
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

getConsArgTypes ::
  Handle ->
  Hint ->
  DD.DefiniteDescription ->
  [TM.Type] ->
  App [[BinderF TM.Type]]
getConsArgTypes h m dataName dataArgs = do
  dataInfo <- lookupDataInfoFull h m dataName
  let dataBinders = DI.dataArgs dataInfo
  if length dataBinders == length dataArgs
    then do
      let binderIds = map (\(_, _, x, _) -> x) dataBinders
      let sub = IntMap.fromList $ zip (map Ident.toInt binderIds) (map TmSubst.Type dataArgs)
      forM (DI.consInfoList dataInfo) $ \consInfo -> do
        liftIO $ substConsArgs h sub (DI.consArgs consInfo)
    else
      raiseCritical m $
        "Could not specialize constructor metadata for `" <> DD.reify dataName <> "` due to arity mismatch"

substConsArgs :: Handle -> TmSubst.Subst -> [BinderF TM.Type] -> IO [BinderF TM.Type]
substConsArgs h sub consArgs =
  case consArgs of
    [] ->
      return []
    (m, k, x, t) : rest -> do
      let substHandle' = TmSubst.new (gensymHandle h)
      t' <- TmSubst.substType substHandle' sub t
      let opaque = m :< TM.Tau
      let sub' = IntMap.insert (Ident.toInt x) (TmSubst.Type opaque) sub
      rest' <- substConsArgs h sub' rest
      return $ (m, k, x, t') : rest'
