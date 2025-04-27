module Move.Scene.LSP.GetSymbolInfo (getSymbolInfo) where

import Control.Comonad.Cofree
import Control.Monad.Trans
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Move.Context.AppM
import Move.Context.Cache (invalidate)
import Move.Context.EIO (toApp)
import Move.Context.Path qualified as Path
import Move.Context.Throw qualified as Throw
import Move.Context.Type
import Move.Scene.Check qualified as Check
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.LSP.FindDefinition qualified as LSP
import Move.Scene.LSP.GetSource qualified as LSP
import Rule.LocationTree qualified as LT
import Rule.Source (Source (sourceFilePath, sourceModule))
import Rule.Target (Target (Peripheral))
import Rule.Term.Weaken (weaken)
import Rule.WeakTerm qualified as WT
import Rule.WeakTerm.ToText

getSymbolInfo ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  AppM T.Text
getSymbolInfo params = do
  source <- LSP.getSource params
  h <- lift Path.new
  lift $ toApp $ invalidate h Peripheral source
  weakTypeEnv <- lift $ Check.checkSingle (sourceModule source) (sourceFilePath source)
  ((locType, _), _) <- LSP.findDefinition params
  _getSymbolInfo weakTypeEnv locType

_getSymbolInfo :: IntMap.IntMap WT.WeakTerm -> LT.LocType -> AppM T.Text
_getSymbolInfo weakTypeEnv locType = do
  symbolName <- liftMaybe $ getSymbolLoc locType
  case symbolName of
    LT.Local varID _ -> do
      t <- liftMaybe $ IntMap.lookup varID weakTypeEnv
      h <- lift Elaborate.new
      t' <- lift (Throw.runMaybe $ toApp $ Elaborate.elaborate' h t) >>= liftMaybe
      return $ toText $ weaken t'
    LT.Global dd isConstLike -> do
      t <- lift (lookupMaybe dd) >>= liftMaybe
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
