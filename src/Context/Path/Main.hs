module Context.Path.Main () where

-- import qualified Context.Path as Path
-- import qualified Context.Throw as Throw
-- import Control.Monad
-- import Path
-- import Path.IO

-- new :: Path.Config -> IO Path.Context
-- new cfg =
--   return $
--     Path.Context
--       { Path.getLibraryDirPath =
--           getLibraryDirPath,
--         Path.ensureNotInLibDir =
--           ensureNotInLibDir (Path.throwCtx cfg)
--       }

-- getLibraryDirPath :: IO (Path Abs Dir)
-- getLibraryDirPath = do
--   basePath <- getCacheDirPath
--   returnDirectory $ basePath </> $(mkRelDir "library")

-- getCacheDirPath :: IO (Path Abs Dir)
-- getCacheDirPath = do
--   getXdgDir XdgCache (Just $(mkRelDir "neut")) >>= returnDirectory

-- returnDirectory :: Path Abs Dir -> IO (Path Abs Dir)
-- returnDirectory path =
--   ensureDir path >> return path

-- ensureNotInLibDir :: Throw.Context -> IO ()
-- ensureNotInLibDir throwCtx = do
--   currentDir <- getCurrentDir
--   libDir <- getLibraryDirPath
--   when (isProperPrefixOf libDir currentDir) $
--     Throw.raiseError'
--       throwCtx
--       "this command cannot be used under the library directory"
