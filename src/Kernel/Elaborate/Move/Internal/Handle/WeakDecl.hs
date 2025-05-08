module Kernel.Elaborate.Move.Internal.Handle.WeakDecl
  ( Handle,
    new,
    insert,
    lookup,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.Move.Raise (raiseError)
import Language.Common.Rule.ForeignCodType qualified as F
import Language.LowComp.Rule.DeclarationName qualified as DN
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Library.Error.Rule.EIO (EIO)
import Library.Logger.Rule.Hint
import Prelude hiding (lookup)

newtype Handle = Handle
  { weakDeclEnvRef :: IORef (Map.HashMap DN.DeclarationName ([WT.WeakTerm], F.ForeignCodType WT.WeakTerm))
  }

new :: IO Handle
new = do
  weakDeclEnvRef <- newIORef Map.empty
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
