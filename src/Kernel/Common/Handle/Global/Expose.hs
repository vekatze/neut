module Kernel.Common.Handle.Global.Expose
  ( Handle,
    new,
    insert,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Logger.Hint

newtype Handle = Handle
  { exposedNameMapRef :: IORef (Map.HashMap EN.ExternalName (Hint, DD.DefiniteDescription))
  }

new :: IO Handle
new = do
  exposedNameMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> Hint -> DD.DefiniteDescription -> EN.ExternalName -> App ()
insert h m dd extName = do
  exposedNameMap <- liftIO $ readIORef (exposedNameMapRef h)
  case Map.lookup extName exposedNameMap of
    Just entry
      | entry /= (m, dd) ->
          raiseError m $ "`" <> EN.reify extName <> "` is already exposed"
    _ ->
      liftIO $ modifyIORef' (exposedNameMapRef h) $ Map.insert extName (m, dd)
