module Kernel.Common.Source
  ( Source (..),
    getRelPathFromSourceDir,
    getBaseReadableLocator,
  )
where

import Control.Monad.Catch
import Data.Text qualified as T
import Kernel.Common.Module qualified as M
import Language.Common.Const
import Logger.Hint
import Path

data Source = Source
  { sourceFilePath :: Path Abs File,
    sourceModule :: M.Module,
    sourceHint :: Maybe Hint
  }
  deriving (Show)

getRelPathFromSourceDir :: (MonadThrow m) => Source -> m (Path Rel File)
getRelPathFromSourceDir source = do
  M.getRelPathFromSourceDir (sourceModule source) (sourceFilePath source)

getBaseReadableLocator :: (MonadThrow m) => Source -> m T.Text
getBaseReadableLocator source = do
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  return $ T.replace "/" nsSep $ T.pack $ toFilePath relPathWithoutExtension
