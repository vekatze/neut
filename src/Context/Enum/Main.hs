module Context.Enum.Main where

-- import qualified Context.Enum as Enum
-- import qualified Context.Throw as Throw
-- import Control.Monad
-- import qualified Data.HashMap.Lazy as Map
-- import Data.IORef
-- import qualified Data.Text as T
-- import Entity.EnumInfo hiding (new)
-- import Entity.Global
-- import qualified Entity.Hint as Hint

-- new :: Enum.Config -> IO Enum.Axis
-- new cfg = do
--   enumEnvRef <- newIORef $ Map.fromList defaultEnumEnv
--   revEnumEnvRef <- newIORef $ Map.fromList $ transpose defaultEnumEnv
--   return
--     Enum.Axis
--       { Enum.register = _register (Enum.throwCtx cfg) enumEnvRef revEnumEnvRef,
--         Enum.lookupType = _lookupType enumEnvRef,
--         Enum.lookupValue = _lookupValue revEnumEnvRef
--       }

-- type EnumEnv = Map.HashMap T.Text [EnumItem]

-- type RevEnumEnv = Map.HashMap T.Text (T.Text, Integer)

-- _register ::
--   Throw.Context ->
--   IORef EnumEnv ->
--   IORef RevEnumEnv ->
--   Hint.Hint ->
--   Enum.EnumTypeName ->
--   [EnumItem] ->
--   IO ()
-- _register axis enumEnvRef revEnumEnvRef hint typeName enumItemList = do
--   enumEnv <- readIORef enumEnvRef
--   when (Map.member typeName enumEnv) $ do
--     Throw.raiseError axis hint $ "the enum `" <> typeName <> "` is already defined"
--   modifyIORef' enumEnvRef $ Map.insert typeName enumItemList
--   let (labels, discriminants) = unzip enumItemList
--   let rev = Map.fromList $ zip labels (zip (repeat typeName) discriminants)
--   modifyIORef' revEnumEnvRef $ Map.union rev

-- _lookupType :: IORef EnumEnv -> Enum.EnumTypeName -> IO (Maybe [EnumItem])
-- _lookupType enumEnvRef typeName = do
--   enumEnv <- readIORef enumEnvRef
--   return $ Map.lookup typeName enumEnv

-- _lookupValue :: IORef RevEnumEnv -> Enum.EnumValueName -> IO (Maybe (Enum.EnumTypeName, Enum.Discriminant))
-- _lookupValue revEnumEnvRef valueName = do
--   revEnumEnv <- readIORef revEnumEnvRef
--   return $ Map.lookup valueName revEnumEnv

-- transpose :: [(T.Text, [(T.Text, Integer)])] -> [(T.Text, (T.Text, Integer))]
-- transpose =
--   concatMap transpose'

-- transpose' :: (T.Text, [(T.Text, Integer)]) -> [(T.Text, (T.Text, Integer))]
-- transpose' (typeName, enumItemList) = do
--   let (labels, discriminants) = unzip enumItemList
--   zip labels (zip (repeat typeName) discriminants)

-- defaultEnumEnv :: [(Enum.EnumTypeName, [EnumItem])]
-- defaultEnumEnv =
--   [ (constBottom, []),
--     (constTop, [(constTopUnit, 0)]),
--     (constBool, [(constBoolFalse, 0), (constBoolTrue, 1)])
--   ]
