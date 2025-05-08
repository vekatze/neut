module Command.Archive.Move.Internal.Module.MakeArchiveEns (makeArchiveEns) where

import Aux.Ens.Move.Parse qualified as Ens
import Aux.Ens.Rule.Ens qualified as E
import Aux.Error.Rule.EIO (EIO)
import Aux.Logger.Rule.Hint
import Aux.SyntaxTree.Rule.Series (Series (hasOptionalSeparator))
import Aux.SyntaxTree.Rule.Series qualified as SE
import Command.Archive.Move.Internal.Module.GetExistingVersions
import Command.Archive.Rule.PackageVersion qualified as PV
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.Containers.ListUtils qualified as ListUtils
import Data.Text qualified as T
import Kernel.Common.Rule.Const
import Kernel.Common.Rule.Module
import Language.Common.Rule.ModuleDigest (ModuleDigest (..))
import Language.Common.Rule.ModuleDigest qualified as MD
import Path
import Path.IO
import System.IO
import Prelude hiding (log)

makeArchiveEns :: PV.PackageVersion -> MainModule -> EIO E.FullEns
makeArchiveEns newVersion targetModule = do
  existingVersions <- getExistingVersions targetModule
  let antecedents = PV.getAntecedents newVersion existingVersions
  antecedentList <- ListUtils.nubOrd <$> mapM (getDigest $ extractModule targetModule) antecedents
  (c1, (baseEns@(m :< _), c2)) <- Ens.fromFilePath (moduleLocation $ extractModule targetModule)
  let antecedentEns = makeAntecedentEns m antecedentList
  mergedEns <- liftEither $ E.merge baseEns antecedentEns
  return (c1, (mergedEns, c2))

getPackagePath :: Module -> PV.PackageVersion -> EIO (Path Abs File)
getPackagePath targetModule ver = do
  let archiveDir = getArchiveDir targetModule
  let archiveName = PV.reify ver
  resolveFile archiveDir $ T.unpack $ archiveName <> packageFileExtension

getDigest :: Module -> PV.PackageVersion -> EIO ModuleDigest
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
