module Parse.Spec
  ( moduleToSpec,
  )
where

import Control.Monad (unless)
import Data.Basic (Hint)
import Data.Entity (Entity, access, toDictionary, toString)
import Data.Log (raiseError)
import Data.Module (Checksum (..), Module (moduleFilePath), getModuleName, getModuleRootDir)
import Data.Spec (Spec (..), URL (..))
import qualified Data.Text as T
import qualified Parse.Entity as E
import Path (Abs, Dir, File, Path)
import Path.IO (doesFileExist, resolveDir, resolveFile)

moduleToSpec :: Hint -> Module -> IO Spec
moduleToSpec m mo = do
  moduleFileExists <- doesFileExist (moduleFilePath mo)
  unless moduleFileExists $ do
    let moduleName = getModuleName mo
    raiseError m $
      T.pack "could not find the module `"
        <> moduleName
        <> "`"
  entity <- E.parse (moduleFilePath mo)
  sourceDirPath <- access "source-directory" entity >>= toString >>= interpretRelDirString mo
  targetDirPath <- access "target-directory" entity >>= toString >>= interpretRelDirString mo
  entryPointEns <- access "entry-point" entity >>= toDictionary
  dependencyEns <- access "dependency" entity >>= toDictionary
  entryPoint <- mapM (interpretEntryPoint sourceDirPath) entryPointEns
  dependency <- mapM interpretDependency dependencyEns
  return
    Spec
      { specSourceDir = sourceDirPath,
        specTargetDir = targetDirPath,
        specEntryPoint = entryPoint,
        specDependency = dependency,
        specLocation = moduleFilePath mo
      }

interpretEntryPoint :: Path Abs Dir -> Entity -> IO (Path Abs File)
interpretEntryPoint sourceDirPath entryPointFilePathValue = do
  entryPointFilePath <- toString entryPointFilePathValue
  resolveFile sourceDirPath $ T.unpack entryPointFilePath

interpretDependency :: Entity -> IO (URL, Checksum)
interpretDependency dependencyValue = do
  url <- access "URL" dependencyValue >>= toString
  checksum <- access "checksum" dependencyValue >>= toString
  return (URL url, Checksum checksum)

interpretRelDirString :: Module -> T.Text -> IO (Path Abs Dir)
interpretRelDirString mo sourceDirPathString = do
  resolveDir (getModuleRootDir mo) $ T.unpack sourceDirPathString
