module Scene.Link
  ( link,
  )
where

import Context.App
import Context.Color qualified as Color
import Context.Debug (report)
import Context.Env qualified as Env
import Context.LLVM qualified as LLVM
import Context.Path qualified as Path
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe
import Data.Text qualified as T
import Rule.Artifact qualified as A
import Rule.Module
import Rule.OutputKind qualified as OK
import Rule.Source qualified as Source
import Rule.Target
import Path
import Path.IO
import Scene.ShowProgress qualified as ProgressBar
import System.Console.ANSI

link :: MainTarget -> Bool -> Bool -> A.ArtifactTime -> [Source.Source] -> App ()
link target shouldSkipLink didPerformForeignCompilation artifactTime sourceList = do
  mainModule <- Env.getMainModule
  isExecutableAvailable <- Path.getExecutableOutputPath target mainModule >>= Path.doesFileExist
  let freshExecutableAvailable = isJust (A.objectTime artifactTime) && isExecutableAvailable
  if shouldSkipLink || (not didPerformForeignCompilation && freshExecutableAvailable)
    then report "Skipped linking object files"
    else link' target mainModule sourceList

link' :: MainTarget -> Module -> [Source.Source] -> App ()
link' target mainModule sourceList = do
  mainObject <- snd <$> Path.getOutputPathForEntryPoint mainModule OK.Object target
  outputPath <- Path.getExecutableOutputPath target mainModule
  objectPathList <- mapM (Path.sourceToOutputPath (Main target) OK.Object) sourceList
  let moduleList = nubOrdOn moduleID $ map Source.sourceModule sourceList
  foreignDirList <- mapM (Path.getForeignDir (Main target)) moduleList
  foreignObjectList <- concat <$> mapM getForeignDirContent foreignDirList
  let clangOptions = getLinkOption (Main target)
  let objects = mainObject : objectPathList ++ foreignObjectList
  let numOfObjects = length objects
  color <- getColor
  let workingTitle = getWorkingTitle numOfObjects
  let completedTitle = getCompletedTitle numOfObjects
  h <- ProgressBar.new Nothing workingTitle completedTitle color
  LLVM.link clangOptions objects outputPath
  ProgressBar.close h

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
