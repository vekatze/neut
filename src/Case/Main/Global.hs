module Case.Main.Global
  ( registerTopLevelFunc,
    registerEnum,
    registerData,
    registerResource,
    lookup,
    initialize,
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Throw as Throw
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Entity.Arity
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.EnumInfo hiding (new)
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import qualified Entity.GlobalName as GN
import qualified Entity.Hint as Hint
import qualified Entity.PrimNum.FromText as PrimNum
import qualified Entity.PrimOp.FromText as PrimOp
import Prelude hiding (lookup)

type NameMap = Map.HashMap DD.DefiniteDescription GN.GlobalName

class
  ( Throw.Context m,
    Env.Context m,
    MonadIO m
  ) =>
  Context m

-- new :: Context m => m Global.Context
-- new = do
--   nameMapRef <- newIORef Map.empty
--   forM_ defaultEnumEnv $ \(typeName, enumItemList) ->
--     modifyIORef' nameMapRef $ Map.union $ createEnumMap typeName enumItemList
--   return
--     Global.Context
--       { Global.registerTopLevelFunc =
--           registerTopLevelFunc (Global.throwCtx cfg) nameMapRef,
--         Global.registerEnum =
--           registerEnum (Global.throwCtx cfg) nameMapRef,
--         Global.registerData =
--           registerData (Global.throwCtx cfg) nameMapRef,
--         Global.registerResource =
--           registerResource (Global.throwCtx cfg) nameMapRef,
--         Global.lookup =
--           lookup nameMapRef
--       }

initialize :: Context m => m ()
initialize = do
  Env.setNameMap Map.empty
  forM_ defaultEnumEnv $ \(typeName, enumItemList) ->
    forM_ (createEnumMap typeName enumItemList) $
      uncurry Env.insertToNameMap

registerTopLevelFunc ::
  Context m =>
  Hint.Hint ->
  DD.DefiniteDescription ->
  Arity ->
  m ()
registerTopLevelFunc m topLevelName arity = do
  topNameMap <- Env.getNameMap
  ensureFreshness m topNameMap topLevelName
  Env.insertToNameMap topLevelName $ GN.TopLevelFunc arity

registerEnum ::
  Context m =>
  Hint.Hint ->
  ET.EnumTypeName ->
  [EnumValue] ->
  m ()
registerEnum hint typeName@(ET.EnumTypeName typeNameString) enumItemList = do
  topNameMap <- Env.getNameMap
  ensureFreshness hint topNameMap typeNameString
  forM_ (createEnumMap typeName enumItemList) $ uncurry Env.insertToNameMap

registerData ::
  Context m =>
  Hint.Hint ->
  DD.DefiniteDescription ->
  Arity ->
  [DD.DefiniteDescription] ->
  m ()
registerData m dataName arity consList = do
  topNameMap <- Env.getNameMap
  ensureFreshness m topNameMap dataName
  Env.insertToNameMap dataName $ GN.Data arity consList

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
    Throw.raiseError m $ "`" <> DD.reify name <> "` is already defined"

lookup :: Context m => DD.DefiniteDescription -> m (Maybe GN.GlobalName)
lookup name = do
  nameMap <- Env.getNameMap
  case Map.lookup name nameMap of
    Just kind ->
      return $ Just kind
    Nothing
      | Just primType <- PrimNum.fromDefiniteDescription name ->
        return $ Just $ GN.PrimType primType
      | Just primOp <- PrimOp.fromDefiniteDescription name ->
        return $ Just $ GN.PrimOp primOp
      | otherwise -> do
        return Nothing

createEnumMap :: ET.EnumTypeName -> [EnumValue] -> [(DD.DefiniteDescription, GN.GlobalName)]
createEnumMap typeName@(ET.EnumTypeName typeNameInner) enumItemList = do
  let (labels, discriminants) = unzip enumItemList
  let labels' = map (\(EV.EnumValueName v) -> v) labels
  (typeNameInner, GN.EnumType enumItemList) : zip labels' (map (GN.EnumIntro typeName) discriminants)

defaultEnumEnv :: [(ET.EnumTypeName, [EnumValue])]
defaultEnumEnv =
  [ (constBottom, []),
    (constTop, [(constTopUnit, D.zero)]),
    (constBool, [(constBoolFalse, D.zero), (constBoolTrue, D.increment D.zero)])
  ]
