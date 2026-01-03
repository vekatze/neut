module Kernel.Elaborate.Internal.Handle.TypeDef
  ( Handle,
    TypeDefInfo (..),
    TypeDefMap,
    new,
    insert',
    get',
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
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

insert' :: Handle -> DD.DefiniteDescription -> [BinderF TM.Type] -> TM.Type -> IO ()
insert' h name binders body = do
  let typeDefInfo = TypeDefInfo {typeDefBinders = binders, typeDefBody = body}
  modifyIORef' (typeDefMapRef h) $
    Map.insert name typeDefInfo

get' :: Handle -> IO TypeDefMap
get' h =
  readIORef (typeDefMapRef h)
