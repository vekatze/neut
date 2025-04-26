module Move.Scene.Module.MakeArchiveEns (makeArchiveEns) where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class
import Data.Containers.ListUtils qualified as ListUtils
import Data.Text qualified as T
import Move.Context.EIO (EIO)
import Move.Context.Fetch (getHandleContents)
import Move.Scene.Ens.Reflect qualified as Ens
import Move.Scene.Module.GetExistingVersions
import Path
import Path.IO
import Rule.Const
import Rule.Ens qualified as E
import Rule.Hint
import Rule.Module
import Rule.ModuleDigest (ModuleDigest (..))
import Rule.ModuleDigest qualified as MD
import Rule.PackageVersion qualified as PV
import Rule.Syntax.Series (Series (hasOptionalSeparator))
import Rule.Syntax.Series qualified as SE
import System.IO
import Prelude hiding (log)

makeArchiveEns :: Ens.Handle -> PV.PackageVersion -> MainModule -> EIO E.FullEns
makeArchiveEns h newVersion targetModule = do
  existingVersions <- getExistingVersions targetModule
  let antecedents = PV.getAntecedents newVersion existingVersions
  antecedentList <- ListUtils.nubOrd <$> mapM (getDigest $ extractModule targetModule) antecedents
  (c1, (baseEns@(m :< _), c2)) <- Ens.fromFilePath h (moduleLocation $ extractModule targetModule)
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
  package <- liftIO $ getHandleContents handle
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
