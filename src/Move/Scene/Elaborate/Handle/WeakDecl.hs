module Move.Scene.Elaborate.Handle.WeakDecl
  ( Handle,
    new,
    insert,
    lookup,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, raiseError)
import Rule.DeclarationName qualified as DN
import Rule.ForeignCodType qualified as F
import Rule.Hint
import Rule.WeakTerm qualified as WT
import Prelude hiding (lookup)

newtype Handle
  = Handle
  { weakDeclEnvRef :: IORef (Map.HashMap DN.DeclarationName ([WT.WeakTerm], F.ForeignCodType WT.WeakTerm))
  }

new :: App Handle
new = do
  weakDeclEnvRef <- asks App.weakDeclEnv
  return $ Handle {..}

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
