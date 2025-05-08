module Command.Common.Move.Build.Link
  ( Handle,
    new,
    link,
  )
where

import Color.Rule.Handle qualified as Color
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.Handle.Global.Env qualified as Env
import Kernel.Common.Move.Handle.Global.Path qualified as Path
import Kernel.Common.Move.Handle.Global.Platform qualified as Platform
import Kernel.Common.Move.RunProcess qualified as RunProcess
import Kernel.Common.Rule.Artifact qualified as A
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Handle.Global.Path qualified as Path
import Kernel.Common.Rule.Module
import Kernel.Common.Rule.OutputKind qualified as OK
import Kernel.Common.Rule.Source qualified as Source
import Kernel.Common.Rule.Target
import Logger.Move.Debug qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Path
import Path.IO
import ProgressIndicator.Move.ShowProgress qualified as Indicator
import System.Console.ANSI

data Handle = Handle
  { loggerHandle :: Logger.Handle,
    envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    colorHandle :: Color.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  Handle {..}

link :: Handle -> MainTarget -> Bool -> Bool -> A.ArtifactTime -> [Source.Source] -> EIO ()
link h target shouldSkipLink didPerformForeignCompilation artifactTime sourceList = do
  let mainModule = Env.getMainModule (envHandle h)
  executablePath <- Path.getExecutableOutputPath (pathHandle h) target
  isExecutableAvailable <- doesFileExist executablePath
  let freshExecutableAvailable = isJust (A.objectTime artifactTime) && isExecutableAvailable
  if shouldSkipLink || (not didPerformForeignCompilation && freshExecutableAvailable)
    then liftIO $ Logger.report (loggerHandle h) "Skipped linking object files"
    else link' h target mainModule sourceList

link' :: Handle -> MainTarget -> MainModule -> [Source.Source] -> EIO ()
link' h target (MainModule mainModule) sourceList = do
  mainObject <- snd <$> Path.getOutputPathForEntryPoint (pathHandle h) mainModule OK.Object target
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target
  objectPathList <- mapM (Path.sourceToOutputPath (pathHandle h) (Main target) OK.Object) sourceList
  let moduleList = nubOrdOn moduleID $ map Source.sourceModule sourceList
  foreignDirList <- mapM (Path.getForeignDir (pathHandle h) (Main target)) moduleList
  foreignObjectList <- concat <$> mapM getForeignDirContent foreignDirList
  let clangOptions = getLinkOption (Main target)
  let objects = mainObject : objectPathList ++ foreignObjectList
  let numOfObjects = length objects
  let workingTitle = getWorkingTitle numOfObjects
  let completedTitle = getCompletedTitle numOfObjects
  let silentMode = Env.getSilentMode (envHandle h)
  progressBarHandle <- liftIO $ Indicator.new (colorHandle h) silentMode Nothing workingTitle completedTitle barColor
  link'' h clangOptions objects outputPath
  liftIO $ Indicator.close progressBarHandle

getWorkingTitle :: Int -> T.Text
getWorkingTitle numOfObjects = do
  let suffix = if numOfObjects <= 1 then "" else "s"
  "Linking " <> T.pack (show numOfObjects) <> " object" <> suffix

getCompletedTitle :: Int -> T.Text
getCompletedTitle numOfObjects = do
  let suffix = if numOfObjects <= 1 then "" else "s"
  "Linked " <> T.pack (show numOfObjects) <> " object" <> suffix

barColor :: [SGR]
barColor = do
  [SetColor Foreground Vivid Green]

getForeignDirContent :: Path Abs Dir -> EIO [Path Abs File]
getForeignDirContent foreignDir = do
  b <- doesDirExist foreignDir
  if b
    then snd <$> listDirRecur foreignDir
    else return []

link'' :: Handle -> [String] -> [Path Abs File] -> Path Abs File -> EIO ()
link'' h clangOptions objectPathList outputPath = do
  clang <- liftIO Platform.getClang
  ensureDir $ parent outputPath
  let runProcessHandle = RunProcess.new (loggerHandle h)
  RunProcess.run runProcessHandle clang $ clangLinkOpt objectPathList outputPath (unwords clangOptions)

clangLinkOpt :: [Path Abs File] -> Path Abs File -> String -> [String]
clangLinkOpt objectPathList outputPath additionalOptionStr = do
  let pathList = map toFilePath objectPathList
  [ "-Wno-override-module",
    "-O2",
    "-flto=thin",
    "-pthread",
    "-lm",
    "-o",
    toFilePath outputPath
    ]
    ++ words additionalOptionStr
    ++ pathList
