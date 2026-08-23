module Kernel.Elaborate.Internal.Handle.Def
  ( Handle,
    new,
    insert',
    get',
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntSet qualified as IntSet
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.Inline.Handle qualified as InlineHandle
import Language.Term.Term qualified as TM
import Prelude hiding (lookup, read)

newtype Handle = Handle
  { defMapRef :: IORef (Map.HashMap DD.DefiniteDescription InlineHandle.DefInfo)
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
  Maybe InlineHandle.DefKind ->
  IntSet.IntSet ->
  IO ()
insert' h name impArgs expArgs defaultArgs e typ mDefKind traceSiteIDs =
  case mDefKind of
    Just defKind -> do
      let defInfo =
            InlineHandle.DefInfo
              { InlineHandle.defImpBinders = impArgs,
                InlineHandle.defExpBinders = expArgs,
                InlineHandle.defDefaultArgs = defaultArgs,
                InlineHandle.defBody = e,
                InlineHandle.codType = typ,
                InlineHandle.defKind = defKind,
                InlineHandle.traceSiteIDs = traceSiteIDs
              }
      atomicModifyIORef' (defMapRef h) $ \mp ->
        (Map.insert name defInfo mp, ())
    Nothing ->
      return ()

get' :: Handle -> IO (Map.HashMap DD.DefiniteDescription InlineHandle.DefInfo)
get' h =
  readIORef (defMapRef h)
