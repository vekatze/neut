module Main.Move.Scene.LSP.GetSymbolInfo
  ( getSymbolInfo,
  )
where

import Control.Comonad.Cofree
import Control.Monad.Trans
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Error.Move.Run (liftMaybe)
import Error.Rule.EIO (EIO)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Language.Term.Rule.Term.Weaken (weaken)
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Language.WeakTerm.Rule.WeakTerm.ToText
import Main.Move.Context.Cache (invalidate)
import Main.Move.Context.Type qualified as Type
import Main.Move.Scene.Check qualified as Check
import Main.Move.Scene.Elaborate qualified as Elaborate
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.LSP.FindDefinition qualified as FindDefinition
import Main.Move.Scene.LSP.GetSource qualified as GetSource
import Main.Rule.Config.Remark (lspConfig)
import Main.Rule.LocationTree qualified as LT
import Main.Rule.Source (Source (sourceFilePath, sourceModule))
import Main.Rule.Target (Target (Peripheral))

getSymbolInfo ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  EIO T.Text
getSymbolInfo params = do
  h <- liftIO $ Base.new lspConfig Nothing
  let getSourceHandle = GetSource.new h
  source <- GetSource.getSource getSourceHandle params
  invalidate (Base.pathHandle h) Peripheral source
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
          let typeHandle = Base.typeHandle h
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
