module Scene.Link
  ( link,
  )
where

import Context.App
import Context.LLVM qualified as LLVM
import Context.Module qualified as Module
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

link :: ConcreteTarget -> Bool -> A.ArtifactTime -> [Source.Source] -> App ()
link target shouldSkipLink artifactTime sourceList = do
  mainModule <- Module.getMainModule
  isExecutableAvailable <- Path.getExecutableOutputPath target mainModule >>= Path.doesFileExist
  if shouldSkipLink || (isJust (A.objectTime artifactTime) && isExecutableAvailable)
    then return ()
    else link' target mainModule sourceList

link' :: ConcreteTarget -> Module -> [Source.Source] -> App ()
link' target mainModule sourceList = do
  mainObject <- snd <$> Path.getOutputPathForEntryPoint mainModule OK.Object target
  foreignLibraries <- getForeignLibraries mainModule
  outputPath <- Path.getExecutableOutputPath target mainModule
  objectPathList <- mapM (Path.sourceToOutputPath OK.Object) sourceList
  let moduleList = nubOrdOn moduleID $ map Source.sourceModule sourceList
  foreignDirList <- mapM Path.getForeignDir moduleList
  foreignObjectList <- concat <$> mapM getForeignDirContent foreignDirList
  LLVM.link (mainObject : objectPathList ++ foreignLibraries ++ foreignObjectList) outputPath

getForeignDirContent :: Path Abs Dir -> App [Path Abs File]
getForeignDirContent foreignDir = do
  b <- doesDirExist foreignDir
  if b
    then snd <$> listDir foreignDir
    else return []

getForeignLibraries :: Module -> App [Path Abs File]
getForeignLibraries targetModule = do
  let foreignDirList = moduleForeignDirList targetModule
  let moduleRootDir = getModuleRootDir targetModule
  concat <$> mapM (getForeignLibraries' . (moduleRootDir </>)) foreignDirList

getForeignLibraries' :: Path Abs Dir -> App [Path Abs File]
getForeignLibraries' foreignDir = do
  platformPrefix <- Path.getPlatformPrefix
  let foreignDir' = foreignDir </> platformPrefix
  (_, objectFileList) <- listDir foreignDir'
  return objectFileList
