module Context.Decl
  ( initialize,
    lookupExtEnv,
    insExtEnv,
    insDeclEnv,
    insDeclEnv',
    lookupDeclEnv,
    getDeclEnv,
  )
where

import Context.App
import Context.App.Internal
import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.Arity qualified as A
import Entity.Decl qualified as DE
import Entity.DeclarationName qualified as DN
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.LowType qualified as LT
import Prelude hiding (lookup, read)

initialize :: App ()
initialize = do
  writeRef' extEnv Map.empty
  writeRef' declEnv Map.empty
  intBaseSize <- Env.getBaseSize'
  forM_ (DE.defaultDeclList intBaseSize) $ \(DE.Decl name domList cod) -> do
    insDeclEnv' (DN.Ext name) domList cod
    insExtEnv name (A.fromInt $ length domList)

lookupExtEnv :: Hint -> EN.ExternalName -> App A.Arity
lookupExtEnv m name = do
  eenv <- readRef' extEnv
  case Map.lookup name eenv of
    Just typeInfo ->
      return typeInfo
    Nothing -> do
      Throw.raiseError m $ "no such external name is defined: " <> EN.reify name

insExtEnv :: EN.ExternalName -> A.Arity -> App ()
insExtEnv name arity =
  modifyRef' extEnv $ Map.insert name arity

getDeclEnv :: App DN.DeclEnv
getDeclEnv =
  readRef' declEnv

insDeclEnv :: DN.DeclarationName -> A.Arity -> App ()
insDeclEnv k arity =
  modifyRef' declEnv $ Map.insert k (LT.toVoidPtrSeq arity, LT.Pointer)

insDeclEnv' :: DN.DeclarationName -> [LT.LowType] -> LT.LowType -> App ()
insDeclEnv' k domList cod =
  modifyRef' declEnv $ Map.insert k (domList, cod)

lookupDeclEnv :: DN.DeclarationName -> App ([LT.LowType], LT.LowType)
lookupDeclEnv name = do
  denv <- readRef' declEnv
  case Map.lookup name denv of
    Just typeInfo ->
      return typeInfo
    Nothing -> do
      Throw.raiseError' $ "lookupDeclEnv: " <> T.pack (show name)
