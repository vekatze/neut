module Scene.Module.UpdateAntecedents (updateAntecedents) where

import Context.App
import Context.Fetch (getHandleContents)
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Containers.ListUtils qualified as ListUtils
import Data.Text qualified as T
import Entity.Const
import Entity.Ens qualified as E
import Entity.Hint
import Entity.Module
import Entity.ModuleDigest (ModuleDigest (..))
import Entity.ModuleDigest qualified as MD
import Entity.PackageVersion qualified as PV
import Path
import Scene.Ens.Reflect qualified as Ens
import Scene.Module.GetExistingVersions
import System.IO
import Prelude hiding (log)

updateAntecedents :: PV.PackageVersion -> Module -> App ()
updateAntecedents newVersion targetModule = do
  existingVersions <- getExistingVersions targetModule
  let antecedents = PV.getAntecedents newVersion existingVersions
  antecedentList <- ListUtils.nubOrd <$> mapM (getDigest targetModule) antecedents
  (c1, (baseEns@(m :< _), c2)) <- Ens.fromFilePath (moduleLocation targetModule)
  let antecedentEns = makeAntecedentEns m antecedentList
  mergedEns <- Throw.liftEither $ E.merge baseEns antecedentEns
  Module.saveEns (moduleLocation targetModule) (c1, (mergedEns, c2))

getPackagePath :: Module -> PV.PackageVersion -> App (Path Abs File)
getPackagePath targetModule ver = do
  let archiveDir = getArchiveDir targetModule
  let archiveName = PV.reify ver
  Path.resolveFile archiveDir $ T.unpack $ archiveName <> packageFileExtension

getDigest :: Module -> PV.PackageVersion -> App ModuleDigest
getDigest targetModule ver = do
  path <- getPackagePath targetModule ver
  handle <- liftIO $ openFile (toFilePath path) ReadMode
  package <- getHandleContents handle
  return $ MD.fromByteString package

makeAntecedentEns :: Hint -> [ModuleDigest] -> E.Ens
makeAntecedentEns m antecedentList = do
  m
    :< E.Dictionary
      []
      [ ( keyAntecedent,
          E.inject $
            m
              :< E.List [] (map (\(ModuleDigest digest) -> (m :< E.String digest, [])) antecedentList)
        )
      ]
