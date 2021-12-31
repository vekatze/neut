{-# LANGUAGE TemplateHaskell #-}

module Command.Init (initialize) where

import Control.Monad (forM_)
import Data.Basic ()
import Data.Global (sourceFileExtension)
import qualified Data.HashMap.Lazy as Map
import Data.Log (raiseError')
import Data.Module
  ( Module (..),
    getReleaseDir,
    getSourceDir,
    getTargetDir,
    moduleFile,
    ppModule,
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Path (parent, parseRelFile, toFilePath, (</>))
import Path.IO (doesDirExist, ensureDir, getCurrentDir, resolveDir)

initialize :: T.Text -> IO ()
initialize moduleName = do
  newModule <- constructDefaultModule moduleName
  moduleDirExists <- doesDirExist $ parent $ moduleLocation newModule
  if moduleDirExists
    then raiseError' $ "the directory `" <> moduleName <> "` already exists"
    else do
      ensureDir $ parent $ moduleLocation newModule
      ensureDir $ getReleaseDir newModule
      ensureDir $ getSourceDir newModule
      ensureDir $ getTargetDir newModule
      createModuleFile newModule
      createMainFile newModule

createModuleFile :: Module -> IO ()
createModuleFile newModule = do
  ensureDir $ parent $ moduleLocation newModule
  TIO.writeFile (toFilePath $ moduleLocation newModule) $ ppModule newModule

createMainFile :: Module -> IO ()
createMainFile newModule = do
  let sourceDir = getSourceDir newModule
  let target = Map.toList $ moduleTarget newModule
  forM_ target $ \(_, relPath) -> do
    let mainFilePath = sourceDir </> relPath
    TIO.writeFile (toFilePath mainFilePath) "define main : i64 =\n  0\n"

constructDefaultModule :: T.Text -> IO Module
constructDefaultModule name = do
  currentDir <- getCurrentDir
  moduleRootDir <- resolveDir currentDir $ T.unpack name
  mainFile <- parseRelFile $ T.unpack $ name <> "." <> sourceFileExtension
  return $
    Module
      { moduleTarget = Map.fromList [(name, mainFile)],
        moduleDependency = Map.empty,
        moduleExtraContents = [],
        moduleLocation = moduleRootDir </> moduleFile
      }
