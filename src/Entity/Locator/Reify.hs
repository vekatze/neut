module Entity.Locator.Reify () where

-- import Control.Exception.Safe
-- import Entity.Locator
-- import Entity.Module
-- import Path

-- toAbsPath :: MonadThrow m => Locator -> m (Path Abs File)
-- toAbsPath (Locator baseModule dirNameList fileName) = do
--   relPathToSource <- getRelPathToSource dirNameList fileName
--   return $ getSourceDir baseModule </> relPathToSource

-- getRelPathToSource :: MonadThrow m => [DirName] -> FileName -> m (Path Rel File)
-- getRelPathToSource dirNameList f@(FileName fileName) =
--   case dirNameList of
--     [] ->
--       parseRelFile fileName
--     DirName dirName : rest -> do
--       relPath <- getRelPathToSource rest f
--       dirPath <- parseRelDir dirName
--       return $ dirPath </> relPath
