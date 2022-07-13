module Context.Global.Main
  ( new,
  )
where

import qualified Context.Global as Global
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.Maybe
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

new :: Global.Config -> IO Global.Context
new cfg = do
  nameMapRef <- newIORef Map.empty
  forM_ defaultEnumEnv $ \(typeName, enumItemList) ->
    modifyIORef' nameMapRef $ Map.union $ createEnumMap typeName enumItemList
  return
    Global.Context
      { Global.registerTopLevelFunc =
          registerTopLevelFunc (Global.throwCtx cfg) nameMapRef,
        Global.registerEnum =
          registerEnum (Global.throwCtx cfg) nameMapRef,
        Global.registerResource =
          registerResource (Global.throwCtx cfg) nameMapRef,
        Global.lookup =
          lookup nameMapRef
      }

registerTopLevelFunc ::
  Throw.Context ->
  IORef NameMap ->
  Hint.Hint ->
  DD.DefiniteDescription ->
  IO ()
registerTopLevelFunc ctx nameMapRef m topLevelName = do
  topNameMap <- readIORef nameMapRef
  ensureFreshness ctx m topNameMap topLevelName
  modifyIORef' nameMapRef $ Map.insert topLevelName GN.TopLevelFunc

registerEnum ::
  Throw.Context ->
  IORef NameMap ->
  Hint.Hint ->
  ET.EnumTypeName ->
  [EnumValue] ->
  IO ()
registerEnum ctx nameMapRef hint typeName@(ET.EnumTypeName typeNameString) enumItemList = do
  nameMap <- readIORef nameMapRef
  ensureFreshness ctx hint nameMap typeNameString
  modifyIORef' nameMapRef $ Map.union $ createEnumMap typeName enumItemList

registerResource ::
  Throw.Context ->
  IORef NameMap ->
  Hint.Hint ->
  DD.DefiniteDescription ->
  IO ()
registerResource ctx nameMapRef m resourceName = do
  topNameMap <- readIORef nameMapRef
  ensureFreshness ctx m topNameMap resourceName
  modifyIORef' nameMapRef $ Map.insert resourceName GN.Resource

ensureFreshness :: Throw.Context -> Hint.Hint -> NameMap -> DD.DefiniteDescription -> IO ()
ensureFreshness ctx m topNameMap name = do
  when (Map.member name topNameMap) $
    Throw.raiseError ctx m $ "`" <> DD.reify name <> "` is already defined"

lookup :: IORef NameMap -> DD.DefiniteDescription -> IO (Maybe GN.GlobalName)
lookup nameMapRef name = do
  nameMap <- readIORef nameMapRef
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

createEnumMap :: ET.EnumTypeName -> [EnumValue] -> NameMap
createEnumMap typeName@(ET.EnumTypeName typeNameInner) enumItemList = do
  let (labels, discriminants) = unzip enumItemList
  let labels' = map (\(EV.EnumValueName v) -> v) labels
  let rev = Map.fromList $ zip labels' (map (GN.EnumIntro typeName) discriminants)
  Map.insert typeNameInner (GN.EnumType enumItemList) rev

defaultEnumEnv :: [(ET.EnumTypeName, [EnumValue])]
defaultEnumEnv =
  [ (constBottom, []),
    (constTop, [(constTopUnit, D.zero)]),
    (constBool, [(constBoolFalse, D.zero), (constBoolTrue, D.increment D.zero)])
  ]
