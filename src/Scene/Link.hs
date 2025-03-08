module Scene.Link
  ( link,
  )
where

import Context.App
import Context.Debug (report)
import Context.Env qualified as Env
import Context.LLVM qualified as LLVM
import Context.Path qualified as Path
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe
import Entity.Artifact qualified as A
import Entity.Module
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.Target
import Path
import Path.IO

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
  LLVM.link clangOptions (mainObject : objectPathList ++ foreignObjectList) outputPath

getForeignDirContent :: Path Abs Dir -> App [Path Abs File]
getForeignDirContent foreignDir = do
  b <- doesDirExist foreignDir
  if b
    then snd <$> listDirRecur foreignDir
    else return []
