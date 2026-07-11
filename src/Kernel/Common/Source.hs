module Kernel.Common.Source
  ( Source (..),
    ensureSourceExistence,
    getRelPathFromSourceDir,
    getBaseReadableLocator,
  )
where

import App.App (App)
import Control.Monad.Catch
import Data.Text qualified as T
import Kernel.Common.Module qualified as M
import Language.Common.Const
import Logger.Hint
import Path
import Path.EnsureFileExistence

data Source = Source
  { sourceFilePath :: Path Abs File,
    sourceModule :: M.Module,
    sourceHint :: Maybe Hint,
    sourceImportLocator :: Maybe T.Text
  }
  deriving (Show)

ensureSourceExistence :: Source -> App ()
ensureSourceExistence source = do
  let path = sourceFilePath source
  case (sourceHint source, sourceImportLocator source) of
    (Just m, Just locatorText) ->
      ensureSourceFileExistence path m locatorText
    (Just m, Nothing) ->
      ensureFileExistence path m
    (Nothing, _) ->
      ensureFileExistence' path

getRelPathFromSourceDir :: (MonadThrow m) => Source -> m (Path Rel File)
getRelPathFromSourceDir source = do
  M.getRelPathFromSourceDir (sourceModule source) (sourceFilePath source)

getBaseReadableLocator :: (MonadThrow m) => Source -> m T.Text
getBaseReadableLocator source = do
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  return $ T.replace "/" nsSep $ T.pack $ toFilePath relPathWithoutExtension
