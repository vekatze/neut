module Main.Move.Scene.Parse.Handle.PreDecl
  ( Handle,
    new,
    insert,
    lookup,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.Hint
import Main.Move.Context.EIO (EIO, raiseError)
import Prelude hiding (lookup, read)

newtype Handle = Handle
  { preDeclEnvRef :: IORef (Map.HashMap EN.ExternalName Hint)
  }

new :: IO Handle
new = do
  preDeclEnvRef <- newIORef Map.empty
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
