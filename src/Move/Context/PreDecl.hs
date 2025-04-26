module Move.Context.PreDecl
  ( Handle,
    new,
    initialize,
    insert,
    lookup,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, raiseError)
import Rule.ExternalName qualified as EN
import Rule.Hint
import Prelude hiding (lookup, read)

newtype Handle
  = Handle
  { preDeclEnvRef :: IORef (Map.HashMap EN.ExternalName Hint)
  }

initialize :: App ()
initialize = do
  writeRef' App.preDeclEnv Map.empty

new :: App Handle
new = do
  preDeclEnvRef <- asks App.preDeclEnv
  return $ Handle {..}

insert :: Handle -> EN.ExternalName -> Hint -> IO ()
insert h k m =
  modifyIORef' (preDeclEnvRef h) $ Map.insert k m

lookup :: Handle -> Hint -> EN.ExternalName -> EIO Hint
lookup h m name = do
  preDeclEnv <- liftIO $ readIORef (preDeclEnvRef h)
  case Map.lookup name preDeclEnv of
    Just typeInfo ->
      return typeInfo
    Nothing -> do
      raiseError m $ "Undeclared function: " <> EN.reify name
