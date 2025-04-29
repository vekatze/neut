module Move.Scene.Elaborate.Handle.WeakDecl
  ( Handle,
    initialize,
    new,
    insert,
    lookup,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.EIO (EIO, raiseError)
import Move.Context.Env qualified as Env
import Rule.DeclarationName qualified as DN
import Rule.Foreign qualified as F
import Rule.ForeignCodType qualified as F
import Rule.Hint
import Rule.WeakTerm qualified as WT
import Prelude hiding (lookup)

newtype Handle
  = Handle
  { weakDeclEnvRef :: IORef (Map.HashMap DN.DeclarationName ([WT.WeakTerm], F.ForeignCodType WT.WeakTerm))
  }

new :: IO Handle
new = do
  weakDeclEnvRef <- newIORef Map.empty
  return $ Handle {..}

initialize :: Handle -> EIO ()
initialize h = do
  liftIO $ writeIORef (weakDeclEnvRef h) Map.empty
  arch <- Env.getArch Nothing
  forM_ (F.defaultWeakForeignList arch) $ \(F.Foreign _ name domList cod) -> do
    liftIO $ modifyIORef' (weakDeclEnvRef h) $ Map.insert (DN.Ext name) (domList, cod)

insert :: Handle -> DN.DeclarationName -> [WT.WeakTerm] -> F.ForeignCodType WT.WeakTerm -> IO ()
insert h k domList cod =
  modifyIORef' (weakDeclEnvRef h) $ Map.insert k (domList, cod)

lookup :: Handle -> Hint -> DN.DeclarationName -> EIO ([WT.WeakTerm], F.ForeignCodType WT.WeakTerm)
lookup h m name = do
  denv <- liftIO $ readIORef (weakDeclEnvRef h)
  case Map.lookup name denv of
    Just typeInfo ->
      return typeInfo
    Nothing -> do
      raiseError m $ "Undeclared function: " <> DN.reify name
