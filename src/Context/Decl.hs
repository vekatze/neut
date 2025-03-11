module Context.Decl
  ( initialize,
    insPreDeclEnv,
    lookupPreDeclEnv,
    insWeakDeclEnv,
    lookupWeakDeclEnv,
  )
where

import Context.App
import Context.App.Internal
import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.DeclarationName qualified as DN
import Entity.ExternalName qualified as EN
import Entity.Foreign qualified as F
import Entity.ForeignCodType qualified as FCT
import Entity.Hint
import Entity.WeakTerm qualified as WT
import Prelude hiding (lookup, read)

initialize :: App ()
initialize = do
  writeRef' preDeclEnv Map.empty
  writeRef' weakDeclEnv Map.empty
  arch <- Env.getArch Nothing
  forM_ (F.defaultWeakForeignList arch) $ \(F.Foreign _ name domList cod) -> do
    insWeakDeclEnv (DN.Ext name) domList cod

insPreDeclEnv :: EN.ExternalName -> Hint -> App ()
insPreDeclEnv k m =
  modifyRef' preDeclEnv $ Map.insert k m

lookupPreDeclEnv :: Hint -> EN.ExternalName -> App Hint
lookupPreDeclEnv m name = do
  denv <- readRef' preDeclEnv
  case Map.lookup name denv of
    Just typeInfo ->
      return typeInfo
    Nothing -> do
      Throw.raiseError m $ "Undeclared function: " <> EN.reify name

insWeakDeclEnv :: DN.DeclarationName -> [WT.WeakTerm] -> FCT.ForeignCodType WT.WeakTerm -> App ()
insWeakDeclEnv k domList cod =
  modifyRef' weakDeclEnv $ Map.insert k (domList, cod)

lookupWeakDeclEnv :: Hint -> DN.DeclarationName -> App ([WT.WeakTerm], FCT.ForeignCodType WT.WeakTerm)
lookupWeakDeclEnv m name = do
  denv <- readRef' weakDeclEnv
  case Map.lookup name denv of
    Just typeInfo ->
      return typeInfo
    Nothing -> do
      Throw.raiseError m $ "Undeclared function: " <> DN.reify name
