module Context.Decl
  ( initialize,
    insDeclEnv,
    insDeclEnv',
    lookupDeclEnv,
    getDeclEnv,
    member,
  )
where

import Context.App
import Context.App.Internal
import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.ArgNum qualified as AN
import Entity.BaseLowType qualified as BLT
import Entity.DeclarationName qualified as DN
import Entity.Foreign qualified as F
import Entity.ForeignCodType qualified as FCT
import Entity.Hint
import Prelude hiding (lookup, read)

initialize :: App ()
initialize = do
  writeRef' declEnv Map.empty
  arch <- Env.getArch Nothing
  forM_ (F.defaultForeignList arch) $ \(F.Foreign name domList cod) -> do
    insDeclEnv' (DN.Ext name) domList cod

getDeclEnv :: App DN.DeclEnv
getDeclEnv =
  readRef' declEnv

insDeclEnv :: DN.DeclarationName -> AN.ArgNum -> App ()
insDeclEnv k argNum =
  modifyRef' declEnv $ Map.insert k (BLT.toVoidPtrSeq argNum, FCT.Void)

insDeclEnv' :: DN.DeclarationName -> [BLT.BaseLowType] -> FCT.ForeignCodType BLT.BaseLowType -> App ()
insDeclEnv' k domList cod =
  modifyRef' declEnv $ Map.insert k (domList, cod)

lookupDeclEnv :: Hint -> DN.DeclarationName -> App ([BLT.BaseLowType], FCT.ForeignCodType BLT.BaseLowType)
lookupDeclEnv m name = do
  denv <- readRef' declEnv
  case Map.lookup name denv of
    Just typeInfo ->
      return typeInfo
    Nothing -> do
      Throw.raiseError m $ "Undeclared function: " <> DN.reify name

member :: DN.DeclarationName -> App Bool
member name = do
  denv <- readRef' declEnv
  return $ Map.member name denv
