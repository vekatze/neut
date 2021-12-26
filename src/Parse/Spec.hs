module Parse.Spec
  ( initializeMainSpec,
    parse,
  )
where

import Control.Monad ((>=>))
import Data.Entity (Entity, access, toDictionary, toString)
import Data.Module (Checksum (..), Module (moduleFilePath), getMainModule)
import Data.Spec (Spec (..), URL (..), setMainSpec)
import qualified Data.Text as T
import qualified Parse.Entity as E
import Path (Abs, Dir, File, Path, Rel, parseRelDir, parseRelFile)

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

initializeMainSpec :: IO ()
initializeMainSpec = do
  getMainModule >>= parse . moduleFilePath >>= setMainSpec

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
