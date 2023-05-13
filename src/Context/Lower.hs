module Context.Lower
  ( initialize,
    getDeclEnv,
    insDeclEnv,
    getExtEnv,
    insExtEnv,
    getDefinedNameSet,
  )
where

import Context.App
import Context.App.Internal
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
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
      [ (DN.malloc, ([LT.voidPtr], LT.voidPtr)),
        (DN.free, ([LT.voidPtr], LT.voidPtr))
      ]

getDeclEnv :: App DN.DeclEnv
getDeclEnv =
  readRef' declEnv

insDeclEnv :: DN.DeclarationName -> A.Arity -> App ()
insDeclEnv k arity =
  modifyRef' declEnv $ Map.insert k (LT.toVoidPtrSeq arity, LT.voidPtr)

getExtEnv :: App (S.Set DD.DefiniteDescription)
getExtEnv =
  readRef' extEnv

insExtEnv :: DD.DefiniteDescription -> App ()
insExtEnv k =
  modifyRef' extEnv $ S.insert k

getDefinedNameSet :: App (S.Set DD.DefiniteDescription)
getDefinedNameSet =
  readRef' definedNameSet
