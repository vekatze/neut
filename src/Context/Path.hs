module Context.Path where

import Entity.ModuleChecksum
import Path

data Context = Context
  { getLibraryDirPath :: IO (Path Abs Dir),
    getModuleDirPath :: ModuleChecksum -> IO (Path Abs Dir),
    getLibraryModuleFilePath :: ModuleChecksum -> IO (Path Abs File)
  }

-- getLibraryModuleFilePath :: ModuleChecksum -> IO (Path Abs File)
-- getLibraryModuleFilePath checksum = do
--   moduleDir <- getModuleDir checksum
--   return $ moduleDir </> moduleFile

data Config = Config
  {
  }

-- mainModule :: Module,
-- currentSource :: Source,
-- throwCtx :: Throw.Context

-- getModuleDir :: ModuleChecksum -> IO (Path Abs Dir)
-- getModuleDir (ModuleChecksum checksum) = do
--   libDir <- getLibraryDirPath
--   resolveDir libDir $ T.unpack checksum
