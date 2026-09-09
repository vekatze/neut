module Kernel.Elaborate.Internal.Handle.WeakDecl
  ( Handle,
    new,
    insert,
    lookup,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.Foreign qualified as F
import Language.LowComp.DeclarationName qualified as DN
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint
import Prelude hiding (lookup)

newtype Handle = Handle
  { weakDeclEnvRef :: IORef (Map.HashMap DN.DeclarationName (F.ForeignSignature WT.WeakType))
  }

new :: IO Handle
new = do
  weakDeclEnvRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> DN.DeclarationName -> F.ForeignSignature WT.WeakType -> IO ()
insert h k sig =
  modifyIORef' (weakDeclEnvRef h) $ Map.insert k sig

lookup :: Handle -> Hint -> DN.DeclarationName -> App (F.ForeignSignature WT.WeakType)
lookup h m name = do
  denv <- liftIO $ readIORef (weakDeclEnvRef h)
  case Map.lookup name denv of
    Just sig ->
      return sig
    Nothing -> do
      raiseError m $ "Undeclared foreign name: " <> DN.reify name
