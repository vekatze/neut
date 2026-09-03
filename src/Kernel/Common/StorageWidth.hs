module Kernel.Common.StorageWidth (storageWidthOf) where

import App.App (App)
import App.Run (raiseCritical)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.OptimizableData qualified as OD
import Language.Common.Binder
import Language.Common.CellLayout qualified as CL
import Language.Common.DataInfo qualified as DI
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident.Reify qualified as Ident
import Language.Term.Subst qualified as Subst
import Language.Term.Term qualified as TM
import Logger.Hint

data Handle = Handle
  { dataHandle :: Data.Handle,
    optDataHandle :: OptimizableData.Handle,
    substHandle :: Subst.Handle,
    hint :: Hint,
    visited :: S.Set DD.DefiniteDescription
  }

storageWidthOf ::
  Data.Handle ->
  OptimizableData.Handle ->
  Subst.Handle ->
  Hint ->
  TM.Type ->
  App CL.FieldWidth
storageWidthOf dataHandle optDataHandle substHandle hint = do
  let visited = S.empty
  resolveWidth Handle {..}

resolveWidth :: Handle -> TM.Type -> App CL.FieldWidth
resolveWidth h ty =
  case ty of
    _ :< TM.Tau ->
      return CL.WidthPointer
    _ :< TM.TVar _ ->
      return defaultWidth
    _ :< TM.TVarGlobal {} ->
      return defaultWidth
    _ :< TM.TyApp inner _ ->
      resolveWidth h inner
    _ :< TM.Pi {} ->
      return CL.WidthPointer
    _ :< TM.Data _ dataName dataArgs -> do
      optDataOrNone <- liftIO $ OptimizableData.lookup (optDataHandle h) dataName
      case optDataOrNone of
        Just OD.Enum -> do
          dataInfo <- lookupDataInfo h dataName
          return $ DI.discriminantWidth (DI.consInfoList dataInfo)
        Just OD.Unary ->
          if S.member dataName (visited h)
            then return defaultWidth
            else do
              innerType <- specializeUnary h dataName dataArgs
              resolveWidth (visit h dataName) innerType
        Nothing ->
          return CL.WidthPointer
    _ :< TM.Box inner ->
      resolveWidth h inner
    _ :< TM.BoxNoema inner ->
      resolveWidth h inner
    _ :< TM.Embed inner ->
      resolveWidth h inner
    _ :< TM.Code inner ->
      resolveWidth h inner
    _ :< TM.PrimType primType ->
      return $ CL.widthOfPrimType primType
    _ :< TM.Void ->
      return defaultWidth
    _ :< TM.Resource {} ->
      return defaultWidth

visit :: Handle -> DD.DefiniteDescription -> Handle
visit h dataName =
  h {visited = S.insert dataName (visited h)}

defaultWidth :: CL.FieldWidth
defaultWidth =
  CL.Width64

lookupDataInfo :: Handle -> DD.DefiniteDescription -> App (DI.DataInfo (BinderF TM.Type))
lookupDataInfo h dataName = do
  dataInfoOrNone <- liftIO $ Data.lookup (dataHandle h) dataName
  case dataInfoOrNone of
    Just dataInfo ->
      return dataInfo
    Nothing ->
      raiseCritical (hint h) $ "Could not find constructor metadata for `" <> DD.reify dataName <> "`"

specializeUnary :: Handle -> DD.DefiniteDescription -> [TM.Type] -> App TM.Type
specializeUnary h dataName dataArgs = do
  dataInfo <- lookupDataInfo h dataName
  let dataBinders = DI.dataArgs dataInfo
  when (length dataBinders /= length dataArgs) $
    raiseCritical (hint h) $
      "Arity mismatch while resolving the width of the unary type `" <> DD.reify dataName <> "`"
  let binderIds = map (\(_, _, x, _) -> x) dataBinders
  let sub = IntMap.fromList $ zip (map Ident.toInt binderIds) (map Subst.Type dataArgs)
  case DI.consInfoList dataInfo of
    [DI.ConsInfo {DI.consArgs = [(_, _, _, t)]}] ->
      liftIO $ Subst.substType (substHandle h) sub t
    _ ->
      raiseCritical (hint h) $ "Found a broken unary data metadata for `" <> DD.reify dataName <> "`"
