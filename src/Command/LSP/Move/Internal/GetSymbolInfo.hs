module Command.LSP.Move.Internal.GetSymbolInfo
  ( getSymbolInfo,
  )
where

import Command.Common.Move.Check qualified as Check
import Command.LSP.Move.Internal.FindDefinition qualified as FindDefinition
import Command.LSP.Move.Internal.GetSource qualified as GetSource
import CommandParser.Rule.Config.Remark (lspConfig)
import Control.Comonad.Cofree
import Control.Monad.Trans
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Error.Move.Run (liftMaybe)
import Error.Rule.EIO (EIO)
import Kernel.Common.Rule.LocationTree qualified as LT
import Kernel.Common.Rule.Source (Source (sourceFilePath, sourceModule))
import Kernel.Common.Rule.Target (Target (Peripheral))
import Kernel.Elaborate.Move.Elaborate qualified as Elaborate
import Kernel.Move.Context.Global.Type qualified as Type
import Kernel.Move.Scene.Init.Global qualified as Global
import Kernel.Move.Scene.ManageCache (invalidate)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Language.Term.Rule.Term.Weaken (weaken)
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Language.WeakTerm.Rule.WeakTerm.ToText

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
        LT.Global dd isConstLike -> do
          let typeHandle = Global.typeHandle h
          t <- lift (liftIO $ Type.lookupMaybe' typeHandle dd) >>= liftMaybe
          case (t, isConstLike) of
            (_ :< WT.Pi _ _ cod, True) ->
              return $ toText cod
            _ ->
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
