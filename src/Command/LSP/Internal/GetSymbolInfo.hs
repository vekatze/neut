module Command.LSP.Internal.GetSymbolInfo
  ( getSymbolInfo,
  )
where

import Command.Common.Check qualified as Check
import Command.LSP.Internal.FindDefinition qualified as FindDefinition
import Command.LSP.Internal.GetSource qualified as GetSource
import CommandParser.Config.Remark (lspConfig)
import Control.Monad.Trans
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Error.EIO (EIO)
import Error.Run (liftMaybe)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Common.LocationTree qualified as LT
import Kernel.Common.ManageCache (invalidate)
import Kernel.Common.Source (Source (sourceFilePath, sourceModule))
import Kernel.Common.Target (Target (Peripheral))
import Kernel.Elaborate.Elaborate qualified as Elaborate
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Language.Term.Weaken (weaken)
import Language.WeakTerm.ToText

getSymbolInfo ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  EIO T.Text
getSymbolInfo params = do
  h <- liftIO $ Global.new lspConfig Nothing
  let getSourceHandle = GetSource.new h
  source <- GetSource.getSource getSourceHandle params
  invalidate (Global.pathHandle h) Peripheral source
  let checkHandle = Check.new h
  handleOrNone <- Check.checkSingle checkHandle (sourceModule source) (sourceFilePath source)
  case handleOrNone of
    Nothing ->
      liftMaybe Nothing
    Just handle -> do
      let findDefHandle = FindDefinition.new h
      ((locType, _), _) <- FindDefinition.findDefinition findDefHandle params
      symbolName <- liftMaybe $ getSymbolLoc locType
      case symbolName of
        LT.Local varID _ -> do
          weakTypeEnv <- liftIO $ Elaborate.getWeakTypeEnv handle
          t <- liftMaybe $ IntMap.lookup varID weakTypeEnv
          t' <- Elaborate.elaborate' handle t
          return $ toText $ weaken t'
        LT.Global dd _ -> do
          let typeHandle = Global.typeHandle h
          t <- lift (liftIO $ Type.lookupMaybe' typeHandle dd) >>= liftMaybe
          return $ toText t
        LT.Foreign {} -> do
          liftMaybe Nothing

getSymbolLoc :: LT.LocType -> Maybe LT.SymbolName
getSymbolLoc locType =
  case locType of
    LT.FileLoc ->
      Nothing
    LT.SymbolLoc symbolName ->
      return symbolName
