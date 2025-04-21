module Scene.Module.MakeArchiveEns (makeArchiveEns) where

import Context.App
import Context.Fetch (getHandleContents)
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Containers.ListUtils qualified as ListUtils
import Data.Text qualified as T
import Rule.Const
import Rule.Ens qualified as E
import Rule.Hint
import Rule.Module
import Rule.ModuleDigest (ModuleDigest (..))
import Rule.ModuleDigest qualified as MD
import Rule.PackageVersion qualified as PV
import Rule.Syntax.Series (Series (hasOptionalSeparator))
import Rule.Syntax.Series qualified as SE
import Path
import Scene.Ens.Reflect qualified as Ens
import Scene.Module.GetExistingVersions
import System.IO
import Prelude hiding (log)

makeArchiveEns :: PV.PackageVersion -> Module -> App E.FullEns
makeArchiveEns newVersion targetModule = do
  existingVersions <- getExistingVersions targetModule
  let antecedents = PV.getAntecedents newVersion existingVersions
  antecedentList <- ListUtils.nubOrd <$> mapM (getDigest targetModule) antecedents
  (c1, (baseEns@(m :< _), c2)) <- Ens.fromFilePath (moduleLocation targetModule)
  let antecedentEns = makeAntecedentEns m antecedentList
  mergedEns <- Throw.liftEither $ E.merge baseEns antecedentEns
  return (c1, (mergedEns, c2))

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
  let antecedentList' = map (\(ModuleDigest digest) -> m :< E.String digest) antecedentList
  E.dictFromList
    m
    [ ( keyAntecedent,
        m :< E.List ((SE.fromList SE.Bracket SE.Comma antecedentList') {hasOptionalSeparator = True})
      )
    ]
