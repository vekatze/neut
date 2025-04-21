module Move.Scene.Link
  ( link,
  )
where

import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe
import Data.Text qualified as T
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.Debug (report)
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.LLVM qualified as LLVM
import Move.Context.Path qualified as Path
import Move.Scene.ShowProgress qualified as ProgressBar
import Path
import Path.IO
import Rule.Artifact qualified as A
import Rule.Module
import Rule.OutputKind qualified as OK
import Rule.Source qualified as Source
import Rule.Target
import System.Console.ANSI

link :: MainTarget -> Bool -> Bool -> A.ArtifactTime -> [Source.Source] -> App ()
link target shouldSkipLink didPerformForeignCompilation artifactTime sourceList = do
  mainModule <- Env.getMainModule
  h <- Path.new
  executablePath <- toApp $ Path.getExecutableOutputPath h target (extractModule mainModule)
  isExecutableAvailable <- doesFileExist executablePath
  let freshExecutableAvailable = isJust (A.objectTime artifactTime) && isExecutableAvailable
  if shouldSkipLink || (not didPerformForeignCompilation && freshExecutableAvailable)
    then toApp $ report "Skipped linking object files"
    else link' target mainModule sourceList

link' :: MainTarget -> MainModule -> [Source.Source] -> App ()
link' target (MainModule mainModule) sourceList = do
  h <- Path.new
  mainObject <- toApp $ snd <$> Path.getOutputPathForEntryPoint h mainModule OK.Object target
  outputPath <- toApp $ Path.getExecutableOutputPath h target mainModule
  objectPathList <- toApp $ mapM (Path.sourceToOutputPath h (Main target) OK.Object) sourceList
  let moduleList = nubOrdOn moduleID $ map Source.sourceModule sourceList
  foreignDirList <- toApp $ mapM (Path.getForeignDir h (Main target)) moduleList
  foreignObjectList <- concat <$> mapM getForeignDirContent foreignDirList
  let clangOptions = getLinkOption (Main target)
  let objects = mainObject : objectPathList ++ foreignObjectList
  let numOfObjects = length objects
  color <- getColor
  let workingTitle = getWorkingTitle numOfObjects
  let completedTitle = getCompletedTitle numOfObjects
  progressBarHandle <- ProgressBar.new Nothing workingTitle completedTitle color
  LLVM.link clangOptions objects outputPath
  ProgressBar.close progressBarHandle

getWorkingTitle :: Int -> T.Text
getWorkingTitle numOfObjects = do
  let suffix = if numOfObjects <= 1 then "" else "s"
  "Linking " <> T.pack (show numOfObjects) <> " object" <> suffix

getCompletedTitle :: Int -> T.Text
getCompletedTitle numOfObjects = do
  let suffix = if numOfObjects <= 1 then "" else "s"
  "Linked " <> T.pack (show numOfObjects) <> " object" <> suffix

getColor :: App [SGR]
getColor = do
  shouldColorize <- Color.getShouldColorizeStdout
  if shouldColorize
    then return [SetColor Foreground Vivid Green]
    else return []

getForeignDirContent :: Path Abs Dir -> App [Path Abs File]
getForeignDirContent foreignDir = do
  b <- doesDirExist foreignDir
  if b
    then snd <$> listDirRecur foreignDir
    else return []
