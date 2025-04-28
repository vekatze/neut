module Move.Scene.LSP.GetSymbolInfo
  ( Handle,
    new,
    getSymbolInfo,
  )
where

import Control.Comonad.Cofree
import Control.Monad.Trans
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App (App)
import Move.Context.AppM
import Move.Context.Cache (invalidate)
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (toApp)
import Move.Context.Elaborate qualified as Elaborate
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Move.Context.Path qualified as Path
import Move.Context.Tag qualified as Tag
import Move.Context.Throw qualified as Throw
import Move.Context.Type qualified as Type
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Check qualified as Check
import Move.Scene.Elaborate (overrideHandleEnv)
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Elaborate.Handle.WeakType qualified as WeakType
import Move.Scene.LSP.FindDefinition qualified as FindDefinition
import Move.Scene.LSP.GetSource qualified as GetSource
import Rule.LocationTree qualified as LT
import Rule.Source (Source (sourceFilePath, sourceModule))
import Rule.Target (Target (Peripheral))
import Rule.Term.Weaken (weaken)
import Rule.WeakTerm qualified as WT
import Rule.WeakTerm.ToText

data Handle
  = Handle
  { getSourceHandle :: GetSource.Handle,
    pathHandle :: Path.Handle,
    findDefHandle :: FindDefinition.Handle,
    envHandle :: Env.Handle,
    gensymHandle :: Gensym.Handle,
    checkHandle :: Check.Handle,
    locatorHandle :: Locator.Handle,
    tagHandle :: Tag.Handle,
    antecedentHandle :: Antecedent.Handle,
    colorHandle :: Color.Handle,
    debugHandle :: Debug.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Color.Handle -> Debug.Handle -> Locator.Handle -> Tag.Handle -> Antecedent.Handle -> App Handle
new envHandle gensymHandle colorHandle debugHandle locatorHandle tagHandle antecedentHandle = do
  getSourceHandle <- GetSource.new envHandle gensymHandle
  pathHandle <- Path.new envHandle colorHandle debugHandle
  findDefHandle <- FindDefinition.new envHandle gensymHandle colorHandle debugHandle
  checkHandle <- Check.new envHandle gensymHandle colorHandle debugHandle locatorHandle tagHandle antecedentHandle
  return $ Handle {..}

getSymbolInfo ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  Handle ->
  p ->
  AppM T.Text
getSymbolInfo h params = do
  source <- lift $ toApp $ GetSource.getSource (getSourceHandle h) params
  lift $ toApp $ invalidate (pathHandle h) Peripheral source
  handleEnv <- lift $ Check.checkSingle (checkHandle h) (sourceModule source) (sourceFilePath source)
  ((locType, _), _) <- lift $ toApp $ FindDefinition.findDefinition (findDefHandle h) params
  symbolName <- liftMaybe $ getSymbolLoc locType
  case symbolName of
    LT.Local varID _ -> do
      weakTypeEnv <- liftIO $ WeakType.get $ Elaborate.weakTypeHandle handleEnv
      t <- liftMaybe $ IntMap.lookup varID weakTypeEnv
      elaborateHandle <- lift $ Elaborate.new (envHandle h) (gensymHandle h) (colorHandle h) (debugHandle h) (locatorHandle h) (tagHandle h) (antecedentHandle h)
      let elaborateHandle' = overrideHandleEnv elaborateHandle handleEnv
      t' <- lift (Throw.runMaybe $ toApp $ Elaborate.elaborate' elaborateHandle' t) >>= liftMaybe
      return $ toText $ weaken t'
    LT.Global dd isConstLike -> do
      t <- lift (Type.lookupMaybe dd) >>= liftMaybe
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
