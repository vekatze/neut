module Path.Archive
  ( writeArchive,
    extractArchive,
  )
where

import App.App (App)
import App.Run (raiseError')
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Check qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.Zstd.Lazy qualified as ZstdLazy
import Control.Applicative ((<|>))
import Control.Exception (ErrorCall, IOException, bracket, displayException, handle, try)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as L
import Data.List (sort)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Path
import Path.FileSystemEncoding (decodeFileSystemPath, encodeFileSystemPath)
import Path.IO (ensureDir, renameDir, withTempDir)
import Path.Write (placeAtomically)
import System.Directory qualified as Dir
import System.FilePath qualified as FP
import System.IO (hClose)
import System.IO.Error (ioeGetErrorString, ioeGetFileName, ioeSetFileName, isUserError)
import System.Posix.Files qualified as PF
import System.Posix.IO qualified as PIO

type DecodedEntries =
  Tar.GenEntries FilePath FilePath (Either Tar.FormatError Tar.DecodeLongNamesError)

archiveCompressionLevel :: Int
archiveCompressionLevel =
  19

tarNameFieldWidth :: Int
tarNameFieldWidth =
  100

longNameEntryPath :: FilePath
longNameEntryPath =
  "././@LongLink"

writeArchive :: Path Abs Dir -> [Path Rel File] -> Path Abs File -> App B.ByteString
writeArchive rootDir files outputPath = do
  entryLists <- mapM (packFile rootDir) $ sort $ map toFilePath files
  let archive = L.toStrict $ ZstdLazy.compress archiveCompressionLevel $ Tar.write $ concat entryLists
  timeStamp <- liftIO getCurrentTime
  liftIO $ placeAtomically outputPath timeStamp $ \path -> B.writeFile (toFilePath path) archive
  return archive

packFile :: Path Abs Dir -> FilePath -> App [Tar.Entry]
packFile rootDir path = do
  let sourcePath = toFilePath rootDir FP.</> path
  content <- liftIO $ B.readFile sourcePath
  permissions <- liftIO $ Dir.getPermissions sourcePath
  pathBytes <- liftIO $ encodeFileSystemPath path
  if B.length pathBytes <= tarNameFieldWidth
    then do
      tarPath <- makeTarPath path
      return [makeFileEntry permissions content tarPath]
    else do
      tarPath <- makeTarPath longNameEntryPath
      return
        [ makeLongNameEntry tarPath pathBytes,
          (makeFileEntry permissions content tarPath) {Tar.entryFormat = Tar.GnuFormat}
        ]

makeTarPath :: FilePath -> App Tar.TarPath
makeTarPath path =
  case Tar.toTarPath False path of
    Left err ->
      raiseError' $ T.pack err
    Right tarPath ->
      return tarPath

makeFileEntry :: Dir.Permissions -> B.ByteString -> Tar.TarPath -> Tar.Entry
makeFileEntry permissions content tarPath = do
  let entry = Tar.fileEntry tarPath $ L.fromStrict content
  if Dir.executable permissions
    then entry {Tar.entryPermissions = Tar.executableFilePermissions}
    else entry

makeLongNameEntry :: Tar.TarPath -> B.ByteString -> Tar.Entry
makeLongNameEntry tarPath pathBytes = do
  let payload = B.snoc pathBytes 0
  let content = Tar.OtherEntryType 'L' (L.fromStrict payload) (fromIntegral $ B.length payload)
  (Tar.simpleEntry tarPath content) {Tar.entryFormat = Tar.GnuFormat}

extractArchive :: T.Text -> Path Abs File -> Path Abs Dir -> App ()
extractArchive description archivePath outputDir = do
  ensureDir $ parent outputDir
  withTempDir (parent outputDir) "extracting" $ \stagingDir -> do
    let extractedDir = stagingDir </> $(mkRelDir "extracted")
    result <- liftIO $ try $ do
      unpackArchive archivePath extractedDir
      renameDir extractedDir outputDir
    case result of
      Left err ->
        raiseError' $ "Could not extract " <> description <> ":\n" <> T.pack (describeIOException extractedDir err)
      Right () ->
        return ()

describeIOException :: Path Abs Dir -> IOException -> String
describeIOException extractedDir err =
  if isUserError err
    then ioeGetErrorString err
    else case ioeGetFileName err of
      Just path ->
        displayException $ ioeSetFileName err $ FP.makeRelative (toFilePath extractedDir) path
      Nothing ->
        displayException err

unpackArchive :: Path Abs File -> Path Abs Dir -> IO ()
unpackArchive archivePath outputDir = do
  Dir.createDirectory $ toFilePath outputDir
  compressed <- B.readFile $ toFilePath archivePath
  let entries = Tar.decodeLongNames $ Tar.read $ ZstdLazy.decompress $ L.fromStrict compressed
  handle (\(_ :: ErrorCall) -> failExtraction "The zstd data is malformed") $
    unpackEntries (toFilePath outputDir) Nothing entries

failExtraction :: String -> IO a
failExtraction message =
  ioError $ userError message

unpackEntries :: FilePath -> Maybe FilePath -> DecodedEntries -> IO ()
unpackEntries outputDir paxPath entries =
  case entries of
    Tar.Done ->
      return ()
    Tar.Fail (Left err) ->
      failExtraction $ "The tar data is malformed: " ++ displayException err
    Tar.Fail (Right _) ->
      failExtraction "A GNU long name entry is malformed"
    Tar.Next entry rest ->
      case Tar.entryContent entry of
        Tar.OtherEntryType 'x' header _ -> do
          path <- readPaxPath header
          unpackEntries outputDir (path <|> paxPath) rest
        Tar.OtherEntryType 'g' _ _ ->
          unpackEntries outputDir paxPath rest
        _ -> do
          unpackEntry outputDir $ maybe entry (\path -> entry {Tar.entryTarPath = path}) paxPath
          unpackEntries outputDir Nothing rest

unpackEntry :: FilePath -> Tar.GenEntry FilePath FilePath -> IO ()
unpackEntry outputDir entry = do
  ensureSafePath entry
  let entryPath = Tar.entryTarPath entry
  let path = outputDir FP.</> entryPath
  case Tar.entryContent entry of
    Tar.NormalFile content _ -> do
      Dir.createDirectoryIfMissing True $ FP.takeDirectory path
      let mode = Tar.entryPermissions entry .&. PF.accessModes
      bracket (PIO.createFile path mode >>= PIO.fdToHandle) hClose $ \fileHandle ->
        L.hPut fileHandle content
    Tar.Directory ->
      Dir.createDirectoryIfMissing True path
    _ ->
      failExtraction $ "The entry `" ++ entryPath ++ "` is neither a file nor a directory"

ensureSafePath :: Tar.GenEntry FilePath FilePath -> IO ()
ensureSafePath entry =
  case Tar.checkEntrySecurity entry of
    Just (Tar.InvalidFileName path) ->
      failExtraction $ "The path `" ++ path ++ "` is invalid"
    Just _ ->
      failExtraction $ "The path `" ++ Tar.entryTarPath entry ++ "` is not inside the destination"
    Nothing ->
      return ()

readPaxPath :: L.ByteString -> IO (Maybe FilePath)
readPaxPath header =
  case parsePaxRecords (L.toStrict header) of
    Nothing ->
      failExtraction "A pax header is malformed"
    Just records ->
      case lookup "path" $ reverse records of
        Just path
          | not (B.null path) ->
              Just <$> decodeFileSystemPath path
        _ ->
          return Nothing

parsePaxRecords :: B.ByteString -> Maybe [(B.ByteString, B.ByteString)]
parsePaxRecords header =
  if B.null header
    then Just []
    else do
      (recordLength, afterLength) <- BC.readInt header
      let lengthWidth = B.length header - B.length afterLength
      let record = B.take recordLength header
      if recordLength <= lengthWidth + 1
        || recordLength > B.length header
        || BC.index record lengthWidth /= ' '
        || BC.last record /= '\n'
        then Nothing
        else do
          let (key, value) = BC.break (== '=') $ B.drop (lengthWidth + 1) $ B.init record
          rest <- parsePaxRecords $ B.drop recordLength header
          return $ (key, B.drop 1 value) : rest
