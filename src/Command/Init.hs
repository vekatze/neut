{-# LANGUAGE TemplateHaskell #-}

module Command.Init (initialize) where

import Control.Monad (forM_)
import Data.Basic ()
import Data.Global (moduleFileName, sourceFileExtension)
import qualified Data.HashMap.Lazy as Map
import Data.Log (raiseError')
import Data.Spec
  ( Spec (..),
    ppSpec,
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Path (Dir, Rel, mkRelDir, parent, parseRelFile, toFilePath, (</>))
import Path.IO (doesDirExist, ensureDir, getCurrentDir, resolveFile)

initialize :: T.Text -> IO ()
initialize moduleName = do
  spec <- constructDefaultSpec moduleName
  moduleDirExists <- doesDirExist $ parent $ specLocation spec
  if moduleDirExists
    then raiseError' $ "the directory `" <> moduleName <> "` already exists"
    else do
      createModuleFile spec
      createReleaseDir spec
      createSourceDir spec
      createTargetDir spec
      createMainFile spec

createModuleFile :: Spec -> IO ()
createModuleFile spec = do
  ensureDir $ parent $ specLocation spec
  TIO.writeFile (toFilePath $ specLocation spec) $ ppSpec spec

createReleaseDir :: Spec -> IO ()
createReleaseDir spec = do
  let baseDir = parent $ specLocation spec
  ensureDir $ baseDir </> $(mkRelDir "release/")

createSourceDir :: Spec -> IO ()
createSourceDir spec = do
  let baseDir = parent $ specLocation spec
  ensureDir $ baseDir </> specSourceDir spec

createTargetDir :: Spec -> IO ()
createTargetDir spec = do
  let baseDir = parent $ specLocation spec
  ensureDir $ baseDir </> specTargetDir spec

createMainFile :: Spec -> IO ()
createMainFile spec = do
  let baseDir = parent $ specLocation spec
  let sourceDir = baseDir </> specSourceDir spec
  let entryPointList = Map.toList $ specEntryPoint spec
  forM_ entryPointList $ \(_, relPath) -> do
    let mainFilePath = sourceDir </> relPath
    TIO.writeFile (toFilePath mainFilePath) "define main : i64 =\n  0\n"

constructDefaultSpec :: T.Text -> IO Spec
constructDefaultSpec name = do
  currentDir <- getCurrentDir
  mainFile <- parseRelFile $ T.unpack $ name <> "." <> sourceFileExtension
  specFilePath <- resolveFile currentDir $ T.unpack name <> "/" <> moduleFileName
  return $
    Spec
      { specSourceDir = $(mkRelDir "source/"),
        specTargetDir = $(mkRelDir "target/"),
        specEntryPoint = Map.fromList [(name, mainFile)],
        specDependency = Map.empty,
        specLocation = specFilePath
      }
