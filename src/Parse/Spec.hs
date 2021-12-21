module Parse.Spec
  ( moduleToSpec,
    getMainSpec,
    parse,
  )
where

import Control.Monad (unless, (>=>))
import Data.Basic (Hint)
import Data.Entity (Entity, access, toDictionary, toString)
import Data.Log (raiseError)
import Data.Module (Checksum (..), Module (moduleFilePath), getMainModule, getModuleName)
import Data.Spec (Spec (..), URL (..))
import qualified Data.Text as T
import qualified Parse.Entity as E
import Path (Abs, Dir, File, Path, Rel, parseRelDir, parseRelFile)
import Path.IO (doesFileExist)

parse :: Path Abs File -> IO Spec
parse specFilePath = do
  entity <- E.parse specFilePath
  sourceDirPath <- access "source-directory" entity >>= interpretRelDirPath
  targetDirPath <- access "target-directory" entity >>= interpretRelDirPath
  entryPointEns <- access "target" entity >>= toDictionary
  dependencyEns <- access "dependency" entity >>= toDictionary
  entryPoint <- mapM interpretRelFilePath entryPointEns
  dependency <- mapM interpretDependency dependencyEns
  return
    Spec
      { specSourceDir = sourceDirPath,
        specTargetDir = targetDirPath,
        specEntryPoint = entryPoint,
        specDependency = dependency,
        specLocation = specFilePath
      }

moduleToSpec :: Hint -> Module -> IO Spec
moduleToSpec m mo = do
  moduleFileExists <- doesFileExist (moduleFilePath mo)
  unless moduleFileExists $ do
    let moduleName = getModuleName mo
    raiseError m $
      T.pack "could not find the module file for `"
        <> moduleName
        <> "`"
  parse $ moduleFilePath mo

getMainSpec :: IO Spec
getMainSpec =
  getMainModule >>= parse . moduleFilePath

interpretRelFilePath :: Entity -> IO (Path Rel File)
interpretRelFilePath =
  toString >=> parseRelFile . T.unpack

interpretRelDirPath :: Entity -> IO (Path Rel Dir)
interpretRelDirPath =
  toString >=> parseRelDir . T.unpack

interpretDependency :: Entity -> IO (URL, Checksum)
interpretDependency dependencyValue = do
  url <- access "URL" dependencyValue >>= toString
  checksum <- access "checksum" dependencyValue >>= toString
  return (URL url, Checksum checksum)
