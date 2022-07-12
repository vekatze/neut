module Entity.Module.Locator
  (
  )
where

-- import qualified Context.Module as Module
-- import qualified Context.Throw as Throw
-- import Control.Monad
-- import qualified Data.HashMap.Strict as Map
-- import Data.IORef
-- import qualified Data.Text as T
-- import Entity.Hint
-- import Entity.Module
-- import qualified Entity.Module.Reflect as Module
-- import qualified Entity.ModuleID as MID
-- import Path
-- import Path.IO
-- import System.IO.Unsafe

-- data Context = Context
--   { throw :: Throw.Context,
--     getModuleCtx :: Module.Context
--   }

-- getNextModule :: Context -> Hint -> MID.ModuleID -> IO Module
-- getNextModule ctx m moduleID = do
--   nextModuleFilePath <- Module.getModuleFilePath (getModuleCtx ctx) (Just m) moduleID
--   moduleCacheMap <- readIORef moduleCacheMapRef
--   case Map.lookup nextModuleFilePath moduleCacheMap of
--     Just nextModule ->
--       return nextModule
--     Nothing -> do
--       moduleFileExists <- doesFileExist nextModuleFilePath
--       unless moduleFileExists $ do
--         Throw.raiseError (throw ctx) m $
--           T.pack "could not find the module file for `"
--             <> MID.reify moduleID -- fixme
--             <> "`"
--       nextModule <- Module.fromFilePath (throw ctx) nextModuleFilePath
--       modifyIORef' moduleCacheMapRef $ Map.insert nextModuleFilePath nextModule
--       return nextModule

-- {-# NOINLINE moduleCacheMapRef #-}
-- moduleCacheMapRef :: IORef (Map.HashMap (Path Abs File) Module)
-- moduleCacheMapRef =
--   unsafePerformIO (newIORef Map.empty)
--
