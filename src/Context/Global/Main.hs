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
import qualified Data.Text as T
import qualified Entity.Discriminant as D
import Entity.EnumInfo hiding (new)
import Entity.Global
import qualified Entity.GlobalName as GN
import qualified Entity.Hint as Hint
import qualified Entity.PrimNum.FromText as PrimNum
import qualified Entity.PrimOp.FromText as PrimOp
import Prelude hiding (lookup)

type NameMap = Map.HashMap T.Text GN.GlobalName

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
        Global.lookup =
          lookup nameMapRef
      }

registerTopLevelFunc ::
  Throw.Context ->
  IORef NameMap ->
  Hint.Hint ->
  T.Text ->
  IO ()
registerTopLevelFunc ctx nameMapRef m topLevelName = do
  topNameMap <- readIORef nameMapRef
  ensureFreshness ctx m topNameMap topLevelName
  when (Map.member topLevelName topNameMap) $
    Throw.raiseError ctx m $ "`" <> topLevelName <> "` is already defined at the top level"
  modifyIORef' nameMapRef $ Map.insert topLevelName GN.TopLevelFunc

ensureFreshness :: Throw.Context -> Hint.Hint -> NameMap -> T.Text -> IO ()
ensureFreshness ctx m topNameMap name = do
  when (Map.member name topNameMap) $
    Throw.raiseError ctx m $ "`" <> name <> "` is already defined"

registerEnum ::
  Throw.Context ->
  IORef NameMap ->
  Hint.Hint ->
  EnumTypeName ->
  [EnumItem] ->
  IO ()
registerEnum ctx nameMapRef hint typeName enumItemList = do
  nameMap <- readIORef nameMapRef
  ensureFreshness ctx hint nameMap typeName
  modifyIORef' nameMapRef $ Map.union $ createEnumMap typeName enumItemList

lookup :: IORef NameMap -> T.Text -> IO (Maybe GN.GlobalName)
lookup nameMapRef name = do
  nameMap <- readIORef nameMapRef
  case Map.lookup name nameMap of
    Just kind ->
      return $ Just kind
    Nothing
      | Just _ <- PrimNum.fromText name ->
        return $ Just GN.Constant
      | Just _ <- PrimOp.fromText name ->
        return $ Just GN.Constant
      | otherwise ->
        return Nothing

createEnumMap :: EnumTypeName -> [EnumItem] -> NameMap
createEnumMap typeName enumItemList = do
  let (labels, discriminants) = unzip enumItemList
  let rev = Map.fromList $ zip labels (map (GN.EnumIntro typeName) discriminants)
  Map.insert typeName (GN.Enum enumItemList) rev

defaultEnumEnv :: [(EnumTypeName, [EnumItem])]
defaultEnumEnv =
  [ (constBottom, []),
    (constTop, [(constTopUnit, D.zero)]),
    (constBool, [(constBoolFalse, D.zero), (constBoolTrue, D.increment D.zero)])
  ]

-- {-# INLINE asWeakConstant #-}
-- asWeakConstant :: Hint -> T.Text -> Maybe WeakTerm
-- asWeakConstant m name
--   | Just (PrimNumInt _) <- PrimNum.fromText name =
--     Just (m :< WeakTermConst name)
--   | Just (PrimNumFloat _) <- PrimNum.fromText name =
--     Just (m :< WeakTermConst name)
--   | Just _ <- PrimOp.fromText name =
--     Just (m :< WeakTermConst name)
--   | otherwise = do
--     Nothing
