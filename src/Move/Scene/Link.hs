module Move.Scene.Link
  ( Handle,
    new,
    link,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe
import Data.Text qualified as T
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO)
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

data Handle
  = Handle
  { debugHandle :: Debug.Handle,
    envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    colorHandle :: Color.Handle,
    llvmHandle :: LLVM.Handle
  }

new :: Env.Handle -> Color.Handle -> App Handle
new envHandle colorHandle = do
  debugHandle <- Debug.new colorHandle
  pathHandle <- Path.new envHandle debugHandle
  llvmHandle <- LLVM.new envHandle debugHandle
  return $ Handle {..}

link :: Handle -> MainTarget -> Bool -> Bool -> A.ArtifactTime -> [Source.Source] -> EIO ()
link h target shouldSkipLink didPerformForeignCompilation artifactTime sourceList = do
  mainModule <- Env.getMainModule (envHandle h)
  executablePath <- Path.getExecutableOutputPath (pathHandle h) target (extractModule mainModule)
  isExecutableAvailable <- doesFileExist executablePath
  let freshExecutableAvailable = isJust (A.objectTime artifactTime) && isExecutableAvailable
  if shouldSkipLink || (not didPerformForeignCompilation && freshExecutableAvailable)
    then Debug.report (debugHandle h) "Skipped linking object files"
    else link' h target mainModule sourceList

link' :: Handle -> MainTarget -> MainModule -> [Source.Source] -> EIO ()
link' h target (MainModule mainModule) sourceList = do
  mainObject <- snd <$> Path.getOutputPathForEntryPoint (pathHandle h) mainModule OK.Object target
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target mainModule
  objectPathList <- mapM (Path.sourceToOutputPath (pathHandle h) (Main target) OK.Object) sourceList
  let moduleList = nubOrdOn moduleID $ map Source.sourceModule sourceList
  foreignDirList <- mapM (Path.getForeignDir (pathHandle h) (Main target)) moduleList
  foreignObjectList <- concat <$> mapM getForeignDirContent foreignDirList
  let clangOptions = getLinkOption (Main target)
  let objects = mainObject : objectPathList ++ foreignObjectList
  let numOfObjects = length objects
  color <- liftIO $ getColor h
  let workingTitle = getWorkingTitle numOfObjects
  let completedTitle = getCompletedTitle numOfObjects
  progressBarHandle <- liftIO $ ProgressBar.new (envHandle h) (colorHandle h) Nothing workingTitle completedTitle color
  LLVM.link (llvmHandle h) clangOptions objects outputPath
  liftIO $ ProgressBar.close progressBarHandle

getWorkingTitle :: Int -> T.Text
getWorkingTitle numOfObjects = do
  let suffix = if numOfObjects <= 1 then "" else "s"
  "Linking " <> T.pack (show numOfObjects) <> " object" <> suffix

getCompletedTitle :: Int -> T.Text
getCompletedTitle numOfObjects = do
  let suffix = if numOfObjects <= 1 then "" else "s"
  "Linked " <> T.pack (show numOfObjects) <> " object" <> suffix

getColor :: Handle -> IO [SGR]
getColor h = do
  shouldColorize <- Color.getShouldColorizeStdout (colorHandle h)
  if shouldColorize
    then return [SetColor Foreground Vivid Green]
    else return []

getForeignDirContent :: Path Abs Dir -> EIO [Path Abs File]
getForeignDirContent foreignDir = do
  b <- doesDirExist foreignDir
  if b
    then snd <$> listDirRecur foreignDir
    else return []
