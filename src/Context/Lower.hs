module Context.Lower
  ( initialize,
    getDeclEnv,
    insDeclEnv,
    insDeclEnv',
    lookupDeclEnv,
    getExtEnv,
    insExtEnv,
    getDefinedNameSet,
  )
where

import Context.App
import Context.App.Internal
import Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Arity qualified as A
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.LowType qualified as LT

initialize :: [DD.DefiniteDescription] -> App ()
initialize nameList = do
  writeRef' staticTextList []
  writeRef' definedNameSet $ S.fromList nameList
  writeRef' extEnv S.empty
  writeRef' declEnv $
    Map.fromList
      [ (DN.malloc, ([LT.Pointer], LT.Pointer)),
        (DN.free, ([LT.Pointer], LT.Pointer))
      ]

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

getExtEnv :: App (S.Set DD.DefiniteDescription)
getExtEnv =
  readRef' extEnv

insExtEnv :: DD.DefiniteDescription -> App ()
insExtEnv k =
  modifyRef' extEnv $ S.insert k

getDefinedNameSet :: App (S.Set DD.DefiniteDescription)
getDefinedNameSet =
  readRef' definedNameSet
