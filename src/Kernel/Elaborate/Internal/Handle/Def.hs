module Kernel.Elaborate.Internal.Handle.Def
  ( Handle,
    new,
    insert',
    get',
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.Inline qualified as Inline
import Language.Term.Term qualified as TM
import Prelude hiding (lookup, read)

newtype Handle = Handle
  { defMapRef :: IORef (Map.HashMap DD.DefiniteDescription Inline.DefInfo)
  }

new :: IO Handle
new = do
  defMapRef <- newIORef Map.empty
  return $ Handle {..}

insert' ::
  Handle ->
  DD.DefiniteDescription ->
  [BinderF TM.Type] ->
  [BinderF TM.Type] ->
  [(BinderF TM.Type, TM.Term)] ->
  TM.Term ->
  TM.Type ->
  Maybe Inline.DefKind ->
  IO ()
insert' h name impArgs expArgs defaultArgs e typ mDefKind =
  case mDefKind of
    Just defKind -> do
      let defInfo =
            Inline.DefInfo
              { Inline.defImpBinders = impArgs,
                Inline.defExpBinders = expArgs,
                Inline.defDefaultArgs = defaultArgs,
                Inline.defBody = e,
                Inline.codType = typ,
                Inline.defKind = defKind
              }
      modifyIORef' (defMapRef h) $ Map.insert name defInfo
    Nothing ->
      return ()

get' :: Handle -> IO (Map.HashMap DD.DefiniteDescription Inline.DefInfo)
get' h =
  readIORef (defMapRef h)
