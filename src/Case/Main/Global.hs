module Case.Main.Global
  ( registerTopLevelFunc,
    registerData,
    lookup,
    initialize,
    registerResource,
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Throw as Throw
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import qualified Entity.Arity as A
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import qualified Entity.GlobalName as GN
import qualified Entity.Hint as Hint
import qualified Entity.PrimOp.FromText as PrimOp
import qualified Entity.PrimType.FromText as PT
import Prelude hiding (lookup)

type NameMap = Map.HashMap DD.DefiniteDescription GN.GlobalName

class
  ( Throw.Context m,
    Env.Context m,
    MonadIO m
  ) =>
  Context m

initialize :: Context m => m ()
initialize = do
  Env.setNameMap Map.empty

registerTopLevelFunc ::
  Context m =>
  Hint.Hint ->
  DD.DefiniteDescription ->
  A.Arity ->
  m ()
registerTopLevelFunc m topLevelName arity = do
  topNameMap <- Env.getNameMap
  ensureFreshness m topNameMap topLevelName
  Env.insertToNameMap topLevelName $ GN.TopLevelFunc arity

registerData ::
  Context m =>
  Hint.Hint ->
  DD.DefiniteDescription ->
  [BinderF a] ->
  [(DD.DefiniteDescription, [BinderF a], D.Discriminant)] ->
  m ()
registerData m dataName dataArgs consInfoList = do
  topNameMap <- Env.getNameMap
  ensureFreshness m topNameMap dataName
  let consList = map (\(consName, _, _) -> consName) consInfoList
  let dataArity = A.fromInt $ length dataArgs
  Env.insertToNameMap dataName $ GN.Data dataArity consList
  forM_ consInfoList $ \(consName, consArgs, discriminant) -> do
    topNameMap' <- Env.getNameMap
    ensureFreshness m topNameMap' consName
    let consArity = A.fromInt $ length consArgs
    Env.insertToNameMap consName $ GN.DataIntro dataArity consArity discriminant

registerResource ::
  Context m =>
  Hint.Hint ->
  DD.DefiniteDescription ->
  m ()
registerResource m resourceName = do
  topNameMap <- Env.getNameMap
  ensureFreshness m topNameMap resourceName
  Env.insertToNameMap resourceName GN.Resource

ensureFreshness :: Context m => Hint.Hint -> NameMap -> DD.DefiniteDescription -> m ()
ensureFreshness m topNameMap name = do
  when (Map.member name topNameMap) $
    Throw.raiseError m $
      "`" <> DD.reify name <> "` is already defined"

lookup :: Context m => DD.DefiniteDescription -> m (Maybe GN.GlobalName)
lookup name = do
  nameMap <- Env.getNameMap
  case Map.lookup name nameMap of
    Just kind ->
      return $ Just kind
    Nothing
      | Just primType <- PT.fromDefiniteDescription name ->
          return $ Just $ GN.PrimType primType
      | Just primOp <- PrimOp.fromDefiniteDescription name ->
          return $ Just $ GN.PrimOp primOp
      | otherwise -> do
          return Nothing
