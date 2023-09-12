module Context.Decl
  ( initialize,
    insDeclEnv,
    insDeclEnv',
    lookupDeclEnv,
    lookupDeclEnv',
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
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Decl qualified as DE
import Entity.DeclarationName qualified as DN
import Entity.Error
import Entity.Hint
import Entity.LowType qualified as LT
import Prelude hiding (lookup, read)

initialize :: App ()
initialize = do
  writeRef' declEnv Map.empty
  intBaseSize <- Env.getBaseSize'
  forM_ (DE.defaultDeclList intBaseSize) $ \(DE.Decl name domList cod) -> do
    insDeclEnv' (DN.Ext name) domList cod

getDeclEnv :: App DN.DeclEnv
getDeclEnv =
  readRef' declEnv

insDeclEnv :: DN.DeclarationName -> AN.ArgNum -> App ()
insDeclEnv k argNum =
  modifyRef' declEnv $ Map.insert k (LT.toVoidPtrSeq argNum, LT.Pointer)

insDeclEnv' :: DN.DeclarationName -> [LT.LowType] -> LT.LowType -> App ()
insDeclEnv' k domList cod =
  modifyRef' declEnv $ Map.insert k (domList, cod)

lookupDeclEnv :: Hint -> DN.DeclarationName -> App ([LT.LowType], LT.LowType)
lookupDeclEnv m name = do
  denv <- readRef' declEnv
  Throw.liftEither $ lookupDeclEnv' m name denv

lookupDeclEnv' :: Hint -> DN.DeclarationName -> DN.DeclEnv -> EE ([LT.LowType], LT.LowType)
lookupDeclEnv' m name denv = do
  case Map.lookup name denv of
    Just typeInfo ->
      return typeInfo
    Nothing -> do
      Left $ newError m $ "undeclared function: " <> T.pack (show name)

member :: DN.DeclarationName -> App Bool
member name = do
  denv <- readRef' declEnv
  return $ Map.member name denv
