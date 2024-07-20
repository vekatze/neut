module Scene.LSP.GetSymbolInfo (getSymbolInfo) where

import Context.AppM
import Context.Cache (invalidate)
import Context.Elaborate
import Context.Type
import Control.Comonad.Cofree
import Control.Monad.Trans
import Data.Text qualified as T
import Entity.LocationTree qualified as LT
import Entity.Source (Source (sourceFilePath))
import Entity.Term.Weaken (weaken)
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.ToText
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Scene.Check qualified as Check
import Scene.Elaborate
import Scene.LSP.FindDefinition qualified as LSP
import Scene.LSP.GetSource qualified as LSP

getSymbolInfo ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  AppM T.Text
getSymbolInfo params = do
  source <- LSP.getSource params
  lift $ invalidate source
  lift $ Check.checkSingle (sourceFilePath source)
  ((locType, _), _) <- LSP.findDefinition params
  _getSymbolInfo locType

_getSymbolInfo :: LT.LocType -> AppM T.Text
_getSymbolInfo locType = do
  symbolName <- liftMaybe $ getSymbolLoc locType
  case symbolName of
    LT.Local varID _ -> do
      t <- lift (lookupWeakTypeEnvMaybe varID) >>= liftMaybe
      t' <- lift $ elaborate' t
      return $ toText $ weaken t'
    LT.Global dd isConstLike -> do
      t <- lift (lookupMaybe dd) >>= liftMaybe
      case (t, isConstLike) of
        (_ :< WT.Pi _ _ cod, True) ->
          return $ toText cod
        _ ->
          return $ toText t

getSymbolLoc :: LT.LocType -> Maybe LT.SymbolName
getSymbolLoc locType =
  case locType of
    LT.FileLoc ->
      Nothing
    LT.SymbolLoc symbolName ->
      return symbolName
