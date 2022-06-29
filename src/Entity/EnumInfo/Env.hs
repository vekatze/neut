module Entity.EnumInfo.Env
  ( register,
    registerIfNew,
    initializeEnumEnv,
  )
where

import Context.App
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import Data.List
import Entity.EnumInfo
import Entity.Global
import Entity.Hint

register :: EnumInfo -> IO ()
register enumInfo = do
  let (name, xis) = fromEnumInfo enumInfo
  let (xs, is) = unzip xis
  let rev = Map.fromList $ zip xs (zip (repeat name) is)
  modifyIORef' enumEnvRef $ Map.insert name xis
  modifyIORef' revEnumEnvRef $ Map.union rev

registerIfNew :: Axis -> Hint -> EnumInfo -> IO ()
registerIfNew axis m enumInfo = do
  let (name, xis) = fromEnumInfo enumInfo
  enumEnv <- readIORef enumEnvRef
  let definedEnums = Map.keys enumEnv ++ map fst (concat (Map.elems enumEnv))
  case find (`elem` definedEnums) $ name : map fst xis of
    Just x ->
      (axis & throw & Throw.raiseError) m $ "the constant `" <> x <> "` is already defined [ENUM]"
    _ ->
      register enumInfo

initializeEnumEnv :: IO ()
initializeEnumEnv = do
  forM_ initialEnumEnv register
