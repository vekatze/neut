module Kernel.Common.StorageWidth (storageWidthOf) where

import App.App (App)
import Control.Comonad.Cofree
import Control.Monad.IO.Class (liftIO)
import Data.Set qualified as S
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.OptimizableData qualified as OD
import Language.Common.CellLayout qualified as CL
import Language.Common.DataInfo qualified as DI
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.DataInfo qualified as DataInfo
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
          dataInfo <- Data.lookupOrCritical (dataHandle h) (hint h) dataName
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

specializeUnary :: Handle -> DD.DefiniteDescription -> [TM.Type] -> App TM.Type
specializeUnary h dataName dataArgs = do
  dataInfo <- Data.lookupOrCritical (dataHandle h) (hint h) dataName
  DataInfo.specializeUnary (substHandle h) (hint h) dataName dataInfo dataArgs
