module Kernel.Elaborate.Internal.Handle.TypeDef
  ( Handle,
    TypeDefInfo (..),
    TypeDefMap,
    new,
    insertMany,
    makeEntry,
    get',
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.HashMapRef (insertEntries)
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Opacity qualified as O
import Language.Term.Term qualified as TM
import Prelude hiding (lookup, read)

data TypeDefInfo = TypeDefInfo
  { typeDefBinders :: [BinderF TM.Type],
    typeDefBody :: TM.Type
  }

type TypeDefMap = Map.HashMap DD.DefiniteDescription TypeDefInfo

newtype Handle = Handle
  { typeDefMapRef :: IORef TypeDefMap
  }

new :: IO Handle
new = do
  typeDefMapRef <- newIORef Map.empty
  return $ Handle {..}

makeEntry :: O.Opacity -> DD.DefiniteDescription -> [BinderF TM.Type] -> TM.Type -> [(DD.DefiniteDescription, TypeDefInfo)]
makeEntry opacity name binders body =
  [(name, TypeDefInfo {typeDefBinders = binders, typeDefBody = body}) | opacity == O.Clear]

insertMany :: Handle -> [(DD.DefiniteDescription, TypeDefInfo)] -> IO ()
insertMany h =
  insertEntries (typeDefMapRef h)

get' :: Handle -> IO TypeDefMap
get' h =
  readIORef (typeDefMapRef h)
