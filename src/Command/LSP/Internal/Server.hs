module Command.LSP.Internal.Server
  ( lsp,
  )
where

import App.Error qualified as E
import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Colog.Core qualified as L
import Command.Common.Check qualified as Check
import Command.Common.Fetch qualified as Fetch
import Command.LSP.CodeAction qualified as CA
import Command.LSP.Internal.Complete qualified as Complete
import Command.LSP.Internal.DiagnosticStore qualified as DiagnosticStore
import Command.LSP.Internal.DocumentState qualified as DocumentState
import Command.LSP.Internal.DocumentStateStore qualified as DocumentStateStore
import Command.LSP.Internal.FindDefinition qualified as FindDefinition
import Command.LSP.Internal.Format qualified as Format
import Command.LSP.Internal.GetSymbolInfo qualified as GetSymbolInfo
import Command.LSP.Internal.Highlight qualified as Highlight
import Command.LSP.Internal.Lint qualified as Lint
import Command.LSP.Internal.References qualified as References
import Command.LSP.Internal.Util (Lsp, LspState, getUriParam, report, republishDiagnostics, run)
import Command.LSP.Internal.Util qualified as Util
import CommandParser.Config.Remark (lspConfig)
import Control.Lens hiding (Iso)
import Control.Monad (forM)
import Control.Monad.IO.Class
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Language.LSP.Logging
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)
import Prettyprinter
import System.IO (stdin, stdout)

lsp :: IO Int
lsp = do
  documentStateStore <- DocumentStateStore.new
  diagnosticStore <- DiagnosticStore.new
  let lspState = Util.LspState {Util.documentStateStore = documentStateStore, Util.diagnosticStore = diagnosticStore}
  runQuietServer $
    ServerDefinition
      { defaultConfig = (),
        parseConfig = const $ const $ Right (),
        configSection = "Neut",
        onConfigChange = const $ return (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = const $ handlers lspState,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = lspOptions
      }

runQuietServer :: ServerDefinition config -> IO Int
runQuietServer def = do
  runServerWithHandles
    ioLogger
    lspLogger
    stdin
    stdout
    def

ioLogger :: LogAction IO (WithSeverity LspServerLog)
ioLogger =
  L.filterBySeverity Info getSeverity $ L.cmap (show . prettyMsg) L.logStringStderr

lspLogger :: LogAction (LspM config) (WithSeverity LspServerLog)
lspLogger = do
  let clientLogger = L.cmap (fmap (T.pack . show . pretty)) defaultClientLogger
  clientLogger <> L.hoistLogAction liftIO ioLogger

prettyMsg :: (Pretty a) => WithSeverity a -> Doc ann
prettyMsg l =
  "[" <> viaShow (L.getSeverity l) <> "] " <> pretty (L.getMsg l)

withGlobalHandle :: LspState -> Lsp () () -> (Global.Handle -> Lsp () ()) -> Lsp () ()
withGlobalHandle lspState defaultAction cont = do
  vOrErr <- liftIO $ Global.newOrError lspConfig Nothing
  case vOrErr of
    Left (_, E.MakeError errors) -> do
      report lspState errors
      defaultAction
    Right v ->
      cont v

mapDefinitionLinkFromStore :: DocumentStateStore.DocumentStateStore -> DefinitionLink -> Lsp () (Maybe DefinitionLink)
mapDefinitionLinkFromStore documentStateStore loc@(DefinitionLink (LocationLink {_targetUri})) = do
  documentState <- liftIO $ DocumentStateStore.lookupDocumentState documentStateStore _targetUri
  return $ DocumentState.mapDefinitionLink documentState loc

mapLocationFromStore :: DocumentStateStore.DocumentStateStore -> Location -> Lsp () (Maybe Location)
mapLocationFromStore documentStateStore loc@Location {_uri} = do
  documentState <- liftIO $ DocumentStateStore.lookupDocumentState documentStateStore _uri
  return $ DocumentState.mapLocation documentState loc

handlers :: LspState -> Handlers (Lsp ())
handlers lspState = do
  let documentStateStore = Util.documentStateStore lspState
  let diagnosticStore = Util.diagnosticStore lspState
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        withGlobalHandle lspState (return ()) $ \h -> do
          let fetchHandle = Fetch.new h
          _ <- run lspState h $ Fetch.fetch fetchHandle (Env.getMainModule (Global.envHandle h))
          return (),
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidOpen $ \notif -> do
        let uri = notif ^. J.params . J.textDocument . J.uri
        let bufferText = notif ^. J.params . J.textDocument . J.text
        withGlobalHandle lspState (return ()) $ \h -> do
          Lint.lint (Lint.new h lspState)
          liftIO $ DocumentStateStore.rebuildDocumentState h documentStateStore uri bufferText,
      notificationHandler SMethod_TextDocumentDidChange $ \notif -> do
        let uri = notif ^. J.params . J.textDocument . J.uri
        let changes = notif ^. J.params . J.contentChanges
        bufferTextOrNone <- fmap virtualFileText <$> getVirtualFile (toNormalizedUri uri)
        liftIO $ do
          DocumentStateStore.updateDocumentState documentStateStore uri changes bufferTextOrNone
          DiagnosticStore.applyContentChanges diagnosticStore (toNormalizedUri uri) changes bufferTextOrNone
        republishDiagnostics lspState uri,
      notificationHandler SMethod_TextDocumentDidSave $ \notif -> do
        let uri = notif ^. J.params . J.textDocument . J.uri
        withGlobalHandle lspState (return ()) $ \h -> do
          Lint.lint (Lint.new h lspState)
          liftIO $ do
            diskTextOrNone <- DocumentStateStore.readSavedText uri
            case diskTextOrNone of
              Just diskText ->
                DocumentStateStore.rebuildDocumentState h documentStateStore uri diskText
              Nothing ->
                DocumentStateStore.clearDocumentState documentStateStore uri,
      notificationHandler SMethod_TextDocumentDidClose $ \notif -> do
        let uri = notif ^. J.params . J.textDocument . J.uri
        liftIO $ DocumentStateStore.clearDocumentState documentStateStore uri,
      notificationHandler SMethod_CancelRequest $ \_ -> do
        return (),
      notificationHandler SMethod_SetTrace $ \_ -> do
        return (),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        let pos = req ^. (J.params . J.position)
        fileOrNone <- getVirtualFile (toNormalizedUri uri)
        withGlobalHandle lspState (responder $ Right $ InL $ List []) $ \h -> do
          completeHandle <- liftIO $ Complete.new h
          itemListOrNone <- run lspState h $ Complete.complete completeHandle uri pos fileOrNone
          case itemListOrNone of
            Nothing ->
              responder $ Right $ InL $ List []
            Just itemList ->
              responder $ Right $ InL $ List itemList,
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        let uri = req ^. J.params . J.textDocument . J.uri
        documentState <- liftIO $ DocumentStateStore.lookupDocumentState documentStateStore uri
        case DocumentState.baseParams documentState (req ^. J.params) of
          Nothing ->
            responder $ Right $ InR $ InR Null
          Just baseReqParams ->
            withGlobalHandle lspState (responder $ Right $ InR $ InR Null) $ \h -> do
              let findDefHandle = FindDefinition.new h
              mLoc <- run lspState h $ FindDefinition.findDefinition findDefHandle baseReqParams
              mappedLocOrNone <- case mLoc of
                Nothing ->
                  return Nothing
                Just ((_, loc), _) ->
                  mapDefinitionLinkFromStore documentStateStore loc
              case mappedLocOrNone of
                Nothing ->
                  responder $ Right $ InR $ InR Null
                Just loc ->
                  responder $ Right $ InR $ InL $ List [loc],
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
        let uri = req ^. J.params . J.textDocument . J.uri
        documentState <- liftIO $ DocumentStateStore.lookupDocumentState documentStateStore uri
        case DocumentState.baseParams documentState (req ^. J.params) of
          Nothing ->
            responder $ Right $ InR Null
          Just baseReqParams ->
            withGlobalHandle lspState (responder $ Right $ InR Null) $ \h -> do
              let highlightHandle = Highlight.new h
              highlightsOrNone <- run lspState h $ Highlight.highlight highlightHandle baseReqParams
              case highlightsOrNone of
                Nothing ->
                  responder $ Right $ InR Null
                Just highlights ->
                  responder $ Right $ InL $ List $ DocumentState.mapHighlights documentState (req ^. J.params . J.position) highlights,
      requestHandler SMethod_TextDocumentReferences $ \req responder -> do
        let uri = req ^. J.params . J.textDocument . J.uri
        documentState <- liftIO $ DocumentStateStore.lookupDocumentState documentStateStore uri
        case DocumentState.baseParams documentState (req ^. J.params) of
          Nothing ->
            responder $ Right $ InR Null
          Just baseReqParams ->
            withGlobalHandle lspState (responder $ Right $ InR Null) $ \h -> do
              referencesHandle <- liftIO $ References.new h
              refsOrNone <- run lspState h $ References.references referencesHandle baseReqParams
              case refsOrNone of
                Nothing ->
                  responder $ Right $ InR Null
                Just refs -> do
                  refs' <- fmap catMaybes $ forM refs $ mapLocationFromStore documentStateStore
                  responder $ Right $ InL refs',
      requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        fileOrNone <- getVirtualFile (toNormalizedUri uri)
        withGlobalHandle lspState (responder $ Right $ InL []) $ \h -> do
          let formatHandle = Format.new h
          textEditList <- run lspState h $ Format.format formatHandle False uri fileOrNone
          let textEditList' = concat $ maybeToList textEditList
          responder $ Right $ InL textEditList',
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let uri = req ^. J.params . J.textDocument . J.uri
        documentState <- liftIO $ DocumentStateStore.lookupDocumentState documentStateStore uri
        case DocumentState.baseParams documentState (req ^. J.params) of
          Nothing ->
            responder $ Right $ InR Null
          Just baseReqParams ->
            withGlobalHandle lspState (responder $ Right $ InR Null) $ \h -> do
              textOrNone <- run lspState h $ GetSymbolInfo.getSymbolInfo baseReqParams
              case textOrNone of
                Nothing ->
                  responder $ Right $ InR Null
                Just text ->
                  responder $
                    Right $
                      InL
                        Hover
                          { _contents = InL $ MarkupContent {_kind = MarkupKind_PlainText, _value = text},
                            _range = Nothing
                          },
      requestHandler SMethod_TextDocumentCodeAction $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        responder $
          Right $
            InL
              [ InL (CA.minimizeImportsCommand uri),
                InL CA.refreshCacheCommand
              ],
      requestHandler SMethod_WorkspaceExecuteCommand $ \req responder -> do
        let params = req ^. J.params
        case params ^. J.command of
          commandName
            | commandName == CA.minimizeImportsCommandName -> do
                case params ^. J.arguments >>= getUriParam of
                  Nothing ->
                    responder $ Right $ InR Null
                  Just uri -> do
                    fileOrNone <- getVirtualFile (toNormalizedUri uri)
                    withGlobalHandle lspState (responder $ Right $ InR Null) $ \h -> do
                      let formatHandle = Format.new h
                      textEditList <- run lspState h $ Format.format formatHandle True uri fileOrNone
                      let textEditList' = concat $ maybeToList textEditList
                      let editParams =
                            ApplyWorkspaceEditParams (Just CA.minimizeImportsCommandTitle) $
                              WorkspaceEdit (Just (M.singleton uri textEditList')) Nothing Nothing
                      _ <- sendRequest SMethod_WorkspaceApplyEdit editParams (const (pure ()))
                      responder $ Right $ InR Null
            | commandName == CA.refreshCacheCommandName -> do
                withGlobalHandle lspState (responder $ Right $ InR Null) $ \h -> do
                  let checkHandle = Check.new h
                  _ <- run lspState h $ Check.checkAll checkHandle
                  responder $ Right $ InR Null
          _ ->
            responder $ Right $ InR Null
    ]

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync =
        Just
          TextDocumentSyncOptions
            { _openClose = Just True,
              _change = Just TextDocumentSyncKind_Incremental,
              _willSave = Just False,
              _willSaveWaitUntil = Just False,
              _save = Just $ InR $ SaveOptions {_includeText = Just False}
            },
      optCompletionTriggerCharacters = Just ['.'],
      optExecuteCommandCommands =
        Just
          [ CA.minimizeImportsCommandName,
            CA.refreshCacheCommandName
          ]
    }
