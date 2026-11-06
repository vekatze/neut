module Kernel.Elaborate.Internal.Handle.WeakTypeDef
  ( Handle,
    TypeDef (..),
    new,
    insertMany,
    makeEntry,
    lookup',
    read',
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.HashMapRef (insertEntries)
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Opacity qualified as O
import Language.WeakTerm.WeakTerm qualified as WT
import Prelude hiding (lookup, read)

data TypeDef = TypeDef
  { typeDefBinders :: [BinderF WT.WeakType],
    typeDefBody :: WT.WeakType
  }

newtype Handle = Handle
  { typeDefMapRef :: IORef (Map.HashMap DD.DefiniteDescription TypeDef)
  }

new :: IO Handle
new = do
  typeDefMapRef <- newIORef Map.empty
  return $ Handle {..}

makeEntry :: O.Opacity -> DD.DefiniteDescription -> [BinderF WT.WeakType] -> WT.WeakType -> [(DD.DefiniteDescription, TypeDef)]
makeEntry opacity name binders body =
  [(name, TypeDef {typeDefBinders = binders, typeDefBody = body}) | opacity == O.Clear]

insertMany :: Handle -> [(DD.DefiniteDescription, TypeDef)] -> IO ()
insertMany h =
  insertEntries (typeDefMapRef h)

lookup' :: Handle -> DD.DefiniteDescription -> IO (Maybe TypeDef)
lookup' h name = do
  typeDefMap <- readIORef (typeDefMapRef h)
  return $ Map.lookup name typeDefMap

read' :: Handle -> IO (Map.HashMap DD.DefiniteDescription TypeDef)
read' h =
  readIORef (typeDefMapRef h)
