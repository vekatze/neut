module Command.LSP.Internal.DiagnosticStore
  ( DiagnosticStore,
    Entry,
    new,
    save,
    applyContentChanges,
    lookupEntry,
    clear,
  )
where

import Command.LSP.Internal.DocumentState (DocumentState)
import Command.LSP.Internal.DocumentState qualified as DocumentState
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Language.LSP.Protocol.Types

type Entry =
  (DocumentState, [Diagnostic])

type DiagnosticStore =
  IORef (M.Map NormalizedUri Entry)

new :: IO DiagnosticStore
new =
  newIORef M.empty

save :: DiagnosticStore -> NormalizedUri -> T.Text -> [Diagnostic] -> IO ()
save diagnosticStore uri baseText diags =
  atomicModifyIORef' diagnosticStore $ \m ->
    (M.insert uri (DocumentState.fromTexts baseText baseText, diags) m, ())

applyContentChanges :: DiagnosticStore -> NormalizedUri -> [TextDocumentContentChangeEvent] -> Maybe T.Text -> IO ()
applyContentChanges diagnosticStore uri changes bufferTextOrNone =
  atomicModifyIORef' diagnosticStore $ \m ->
    case M.lookup uri m of
      Nothing ->
        (m, ())
      Just (documentState, diags) -> do
        let documentState' = fromMaybe documentState (DocumentState.reapply documentState changes bufferTextOrNone)
        (M.insert uri (documentState', diags) m, ())

lookupEntry :: DiagnosticStore -> NormalizedUri -> IO (Maybe Entry)
lookupEntry diagnosticStore uri =
  M.lookup uri <$> readIORef diagnosticStore

clear :: DiagnosticStore -> IO ()
clear diagnosticStore =
  writeIORef diagnosticStore M.empty
