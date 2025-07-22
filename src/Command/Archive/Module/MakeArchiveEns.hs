module Command.Archive.Module.MakeArchiveEns (makeArchiveEns) where

import App.App (App)
import Command.Archive.Module.GetExistingVersions
import Command.Archive.PackageVersion.PackageVersion qualified as PV
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.Containers.ListUtils qualified as ListUtils
import Data.Text qualified as T
import Ens.Ens qualified as E
import Ens.Parse qualified as Ens
import Kernel.Common.Const
import Kernel.Common.Module
import Language.Common.ModuleDigest (ModuleDigest (..))
import Language.Common.ModuleDigest qualified as MD
import Logger.Hint
import Path
import Path.IO
import SyntaxTree.Series (Series (hasOptionalSeparator))
import SyntaxTree.Series qualified as SE
import System.IO
import Prelude hiding (log)

makeArchiveEns :: PV.PackageVersion -> MainModule -> App E.FullEns
makeArchiveEns newVersion targetModule = do
  existingVersions <- getExistingVersions targetModule
  let antecedents = PV.getAntecedents newVersion existingVersions
  antecedentList <- ListUtils.nubOrd <$> mapM (getDigest $ extractModule targetModule) antecedents
  (c1, (baseEns@(m :< _), c2)) <- Ens.fromFilePath (moduleLocation $ extractModule targetModule)
  let antecedentEns = makeAntecedentEns m antecedentList
  mergedEns <- liftEither $ E.merge baseEns antecedentEns
  return (c1, (mergedEns, c2))

getPackagePath :: Module -> PV.PackageVersion -> App (Path Abs File)
getPackagePath targetModule ver = do
  let archiveDir = getArchiveDir targetModule
  let archiveName = PV.reify ver
  resolveFile archiveDir $ T.unpack $ archiveName <> packageFileExtension

getDigest :: Module -> PV.PackageVersion -> App ModuleDigest
getDigest targetModule ver = do
  path <- getPackagePath targetModule ver
  handle <- liftIO $ openFile (toFilePath path) ReadMode
  package <- liftIO $ B.hGetContents handle
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
