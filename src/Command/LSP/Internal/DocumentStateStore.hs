module Command.LSP.Internal.DocumentStateStore
  ( DocumentStateStore,
    new,
    lookupDocumentState,
    rebuildDocumentState,
    updateDocumentState,
    clearDocumentState,
    readSavedText,
  )
where

import App.App (App)
import App.Run (runApp)
import Command.LSP.Internal.DocumentState qualified as DocumentState
import Command.LSP.Internal.Source.Reflect qualified as SourceReflect
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.Foldable (foldlM)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Path qualified as KPath
import Kernel.Common.Source qualified as Source
import Kernel.Common.Target (Target (Peripheral))
import Language.LSP.Protocol.Types
import Path.IO qualified as PathIO
import System.Directory (doesFileExist)

type DocumentStateStore =
  IORef (M.Map NormalizedUri DocumentState.DocumentState)

new :: IO DocumentStateStore
new =
  newIORef M.empty

lookupDocumentState :: DocumentStateStore -> Uri -> IO (Maybe DocumentState.DocumentState)
lookupDocumentState documentStateStore uri =
  M.lookup (toNormalizedUri uri) <$> readIORef documentStateStore

rebuildDocumentState :: Global.Handle -> DocumentStateStore -> Uri -> T.Text -> IO ()
rebuildDocumentState gh documentStateStore uri bufferText = do
  preferredBaseTextOrNone <- readBaseTextIfCacheFresh gh uri
  storeResolvedDocumentState documentStateStore uri (Just bufferText) preferredBaseTextOrNone

updateDocumentState :: DocumentStateStore -> Uri -> [TextDocumentContentChangeEvent] -> Maybe T.Text -> IO ()
updateDocumentState documentStateStore uri changes bufferTextOrNone = do
  documentStateMap <- readIORef documentStateStore
  let nuri = toNormalizedUri uri
  documentStateOrNone <- case M.lookup nuri documentStateMap of
    Just documentState ->
      case foldlM DocumentState.applyContentChange documentState changes of
        Just updated ->
          return $ Just updated
        Nothing ->
          resolveDocumentState documentStateStore uri bufferTextOrNone Nothing
    Nothing ->
      resolveDocumentState documentStateStore uri bufferTextOrNone Nothing
  writeDocumentState documentStateStore uri documentStateOrNone

clearDocumentState :: DocumentStateStore -> Uri -> IO ()
clearDocumentState documentStateStore uri =
  atomicModifyIORef' documentStateStore $ \documentStateMap ->
    (M.delete (toNormalizedUri uri) documentStateMap, ())

writeDocumentState :: DocumentStateStore -> Uri -> Maybe DocumentState.DocumentState -> IO ()
writeDocumentState documentStateStore uri documentStateOrNone =
  atomicModifyIORef' documentStateStore $ \documentStateMap -> do
    let nuri = toNormalizedUri uri
    case documentStateOrNone of
      Just documentState ->
        (M.insert nuri documentState documentStateMap, ())
      Nothing ->
        (M.delete nuri documentStateMap, ())

storeResolvedDocumentState :: DocumentStateStore -> Uri -> Maybe T.Text -> Maybe T.Text -> IO ()
storeResolvedDocumentState documentStateStore uri bufferTextOrNone preferredBaseTextOrNone = do
  documentStateOrNone <- resolveDocumentState documentStateStore uri bufferTextOrNone preferredBaseTextOrNone
  writeDocumentState documentStateStore uri documentStateOrNone

resolveDocumentState :: DocumentStateStore -> Uri -> Maybe T.Text -> Maybe T.Text -> IO (Maybe DocumentState.DocumentState)
resolveDocumentState documentStateStore uri bufferTextOrNone preferredBaseTextOrNone =
  case bufferTextOrNone of
    Nothing ->
      return Nothing
    Just bufferText -> do
      baseTextOrNone <- resolveBaseText documentStateStore uri preferredBaseTextOrNone
      return $ (`DocumentState.fromTexts` bufferText) <$> baseTextOrNone

resolveBaseText :: DocumentStateStore -> Uri -> Maybe T.Text -> IO (Maybe T.Text)
resolveBaseText documentStateStore uri preferredBaseTextOrNone = do
  m <- readIORef documentStateStore
  let nuri = toNormalizedUri uri
  let existingBaseTextOrNone = DocumentState.baseText <$> M.lookup nuri m
  case preferredBaseTextOrNone <|> existingBaseTextOrNone of
    Just baseText ->
      return $ Just baseText
    Nothing ->
      readSavedText uri

readSavedText :: Uri -> IO (Maybe T.Text)
readSavedText uri =
  case uriToFilePath uri of
    Nothing ->
      return Nothing
    Just path -> do
      exists <- doesFileExist path
      if exists
        then Just . decodeUtf8 <$> B.readFile path
        else return Nothing

readBaseTextIfCacheFresh :: Global.Handle -> Uri -> IO (Maybe T.Text)
readBaseTextIfCacheFresh gh uri = do
  result <- runApp $ readSavedTextIfCacheFresh gh uri
  case result of
    Right textOrNone ->
      return textOrNone
    _ ->
      return Nothing

readSavedTextIfCacheFresh :: Global.Handle -> Uri -> App (Maybe T.Text)
readSavedTextIfCacheFresh gh uri =
  case uriToFilePath uri of
    Nothing ->
      return Nothing
    Just fp -> do
      srcOrNone <- SourceReflect.reflect (SourceReflect.new gh) fp
      case srcOrNone of
        Nothing ->
          return Nothing
        Just src -> do
          cachePath <- KPath.getSourceLocationCachePath (Global.pathHandle gh) Peripheral src
          cacheExists <- PathIO.doesFileExist cachePath
          let srcAbs = Source.sourceFilePath src
          srcExists <- PathIO.doesFileExist srcAbs
          if not (cacheExists && srcExists)
            then return Nothing
            else do
              cacheMtime <- PathIO.getModificationTime cachePath
              sourceMtime <- PathIO.getModificationTime srcAbs
              if cacheMtime >= sourceMtime
                then liftIO $ Just . decodeUtf8 <$> B.readFile fp
                else return Nothing
