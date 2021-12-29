module Parse.Module
  ( initializeMainModule,
    parse,
  )
where

import Control.Monad ((>=>))
import Data.Basic (Checksum (..), URL (..))
import Data.Entity (Entity, access, toDictionary, toString)
import Data.Module (Module (..), findModuleFile, setMainModule)
import qualified Data.Text as T
import qualified Parse.Entity as E
import Path (Abs, File, Path, Rel, parseRelFile)
import Path.IO (getCurrentDir)

parse :: Path Abs File -> IO Module
parse moduleFilePath = do
  entity <- E.parse moduleFilePath
  entryPointEns <- access "target" entity >>= toDictionary
  dependencyEns <- access "dependency" entity >>= toDictionary
  target <- mapM interpretRelFilePath entryPointEns
  dependency <- mapM interpretDependency dependencyEns
  return
    Module
      { moduleTarget = target,
        moduleDependency = dependency,
        moduleLocation = moduleFilePath
      }

initializeMainModule :: IO ()
initializeMainModule = do
  getMainModuleFilePath >>= parse >>= setMainModule

interpretRelFilePath :: Entity -> IO (Path Rel File)
interpretRelFilePath =
  toString >=> parseRelFile . T.unpack

interpretDependency :: Entity -> IO (URL, Checksum)
interpretDependency dependencyValue = do
  url <- access "URL" dependencyValue >>= toString
  checksum <- access "checksum" dependencyValue >>= toString
  return (URL url, Checksum checksum)

getMainModuleFilePath :: IO (Path Abs File)
getMainModuleFilePath =
  getCurrentDir >>= findModuleFile
