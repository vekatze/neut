module Kernel.Common.LocalArchive
  ( LocalArchiveMap,
    empty,
    load,
  )
where

import App.App (App)
import App.Run (forP, raiseError, raiseError')
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.HashMap.Strict qualified as Map
import Data.List (isSuffixOf, sort)
import Data.Text qualified as T
import Ens.Ens qualified as E
import Ens.Parse qualified as EnsParse
import Kernel.Common.Const (packageFileExtension)
import Language.Common.ModuleDigest qualified as MD
import Path
import Path.IO
import SyntaxTree.Series qualified as SE

type LocalArchiveMap =
  Map.HashMap MD.ModuleDigest (Path Abs File)

empty :: LocalArchiveMap
empty =
  Map.empty

load :: FilePath -> App LocalArchiveMap
load filePath = do
  path <- resolveFile' filePath
  fileExists <- doesFileExist path
  unless fileExists $ do
    raiseError' $ "No such file exists: " <> T.pack (toFilePath path)
  archiveList <- readArchiveList path
  entryList <- forP archiveList $ \archive -> do
    content <- liftIO $ B.readFile $ toFilePath archive
    return (MD.fromByteString content, archive)
  return $ Map.fromListWith (\_ earlier -> earlier) entryList

readArchiveList :: Path Abs File -> App [Path Abs File]
readArchiveList path = do
  (_, (ens, _)) <- EnsParse.fromFilePath path
  (_, entrySeries) <- liftEither $ E.toList ens
  concat <$> mapM (resolveEntry (parent path)) (SE.extract entrySeries)

resolveEntry :: Path Abs Dir -> E.Ens -> App [Path Abs File]
resolveEntry baseDir entryEns = do
  (m, entryText) <- liftEither $ E.toString entryEns
  if T.isSuffixOf "/" entryText
    then do
      dir <- resolveDir baseDir $ T.unpack entryText
      dirExists <- doesDirExist dir
      unless dirExists $ do
        raiseError m $ "No such directory exists: " <> T.pack (toFilePath dir)
      listArchive dir
    else do
      file <- resolveFile baseDir $ T.unpack entryText
      fileExists <- doesFileExist file
      unless fileExists $ do
        raiseError m $ "No such file exists: " <> T.pack (toFilePath file)
      return [file]

listArchive :: Path Abs Dir -> App [Path Abs File]
listArchive dir = do
  (_, fileList) <- listDir dir
  return $ sort $ filter isArchive fileList

isArchive :: Path Abs File -> Bool
isArchive path =
  T.unpack packageFileExtension `isSuffixOf` toFilePath (filename path)
