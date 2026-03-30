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
import Command.LSP.Internal.FindDefinition qualified as FindDefinition
import Command.LSP.Internal.Format qualified as Format
import Command.LSP.Internal.GetSymbolInfo qualified as GetSymbolInfo
import Command.LSP.Internal.Highlight qualified as Highlight
import Command.LSP.Internal.Lint qualified as Lint
import Command.LSP.Internal.References qualified as References
import Command.LSP.Internal.Util (Lsp, getUriParam, report, run)
import CommandParser.Config.Remark (lspConfig)
import Control.Lens hiding (Iso)
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
import Prettyprinter
import System.IO (stdin, stdout)

lsp :: IO Int
lsp = do
  runQuietServer $
    ServerDefinition
      { defaultConfig = (),
        parseConfig = const $ const $ Right (),
        configSection = "Neut",
        onConfigChange = const $ return (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = const handlers,
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

withGlobalHandle :: Lsp () () -> (Global.Handle -> Lsp () ()) -> Lsp () ()
withGlobalHandle defaultAction cont = do
  vOrErr <- liftIO $ Global.newOrError lspConfig Nothing
  case vOrErr of
    Left (_, E.MakeError errors) -> do
      report errors
      defaultAction
    Right v ->
      cont v

handlers :: Handlers (Lsp ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        withGlobalHandle (return ()) $ \h -> do
          let fetchHandle = Fetch.new h
          _ <- run h $ Fetch.fetch fetchHandle (Env.getMainModule (Global.envHandle h))
          return (),
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidOpen $ \_ -> do
        withGlobalHandle (return ()) $ \h -> do
          Lint.lint (Lint.new h),
      notificationHandler SMethod_TextDocumentDidChange $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidSave $ \_ -> do
        withGlobalHandle (return ()) $ \h -> do
          Lint.lint (Lint.new h),
      notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
        return (),
      notificationHandler SMethod_CancelRequest $ \_ -> do
        return (),
      notificationHandler SMethod_SetTrace $ \_ -> do
        return (),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        let pos = req ^. (J.params . J.position)
        fileOrNone <- getVirtualFile (toNormalizedUri uri)
        withGlobalHandle (responder $ Right $ InL $ List []) $ \h -> do
          completeHandle <- liftIO $ Complete.new h
          itemListOrNone <- run h $ Complete.complete completeHandle uri pos fileOrNone
          case itemListOrNone of
            Nothing ->
              responder $ Right $ InL $ List []
            Just itemList ->
              responder $ Right $ InL $ List itemList,
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        withGlobalHandle (responder $ Right $ InR $ InR Null) $ \h -> do
          let findDefHandle = FindDefinition.new h
          mLoc <- run h $ FindDefinition.findDefinition findDefHandle (req ^. J.params)
          case mLoc of
            Nothing -> do
              responder $ Right $ InR $ InR Null
            Just ((_, loc), _) -> do
              responder $ Right $ InR $ InL $ List [loc],
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
        withGlobalHandle (responder $ Right $ InR Null) $ \h -> do
          let highlightHandle = Highlight.new h
          highlightsOrNone <- run h $ Highlight.highlight highlightHandle $ req ^. J.params
          case highlightsOrNone of
            Nothing ->
              responder $ Right $ InR Null
            Just highlights ->
              responder $ Right $ InL $ List highlights,
      requestHandler SMethod_TextDocumentReferences $ \req responder -> do
        withGlobalHandle (responder $ Right $ InR Null) $ \h -> do
          referencesHandle <- liftIO $ References.new h
          refsOrNone <- run h $ References.references referencesHandle $ req ^. J.params
          case refsOrNone of
            Nothing -> do
              responder $ Right $ InR Null
            Just refs -> do
              responder $ Right $ InL refs,
      requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        fileOrNone <- getVirtualFile (toNormalizedUri uri)
        withGlobalHandle (responder $ Right $ InL []) $ \h -> do
          let formatHandle = Format.new h
          textEditList <- run h $ Format.format formatHandle False uri fileOrNone
          let textEditList' = concat $ maybeToList textEditList
          responder $ Right $ InL textEditList',
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        withGlobalHandle (responder $ Right $ InR Null) $ \h -> do
          textOrNone <- run h $ GetSymbolInfo.getSymbolInfo (req ^. J.params)
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
                    withGlobalHandle (responder $ Right $ InR Null) $ \h -> do
                      let formatHandle = Format.new h
                      textEditList <- run h $ Format.format formatHandle True uri fileOrNone
                      let textEditList' = concat $ maybeToList textEditList
                      let editParams =
                            ApplyWorkspaceEditParams (Just CA.minimizeImportsCommandTitle) $
                              WorkspaceEdit (Just (M.singleton uri textEditList')) Nothing Nothing
                      _ <- sendRequest SMethod_WorkspaceApplyEdit editParams (const (pure ()))
                      responder $ Right $ InR Null
            | commandName == CA.refreshCacheCommandName -> do
                withGlobalHandle (responder $ Right $ InR Null) $ \h -> do
                  let checkHandle = Check.new h
                  _ <- run h $ Check.checkAll checkHandle
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
