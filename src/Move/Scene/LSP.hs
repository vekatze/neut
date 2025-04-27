module Move.Scene.LSP (lsp) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Colog.Core qualified as L
import Control.Lens hiding (Iso)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Language.LSP.Logging
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Scene.Check (checkAll)
import Move.Scene.LSP.Complete qualified as Complete
import Move.Scene.LSP.FindDefinition qualified as FindDefinition
import Move.Scene.LSP.Format qualified as LSP
import Move.Scene.LSP.GetSymbolInfo qualified as LSP
import Move.Scene.LSP.Highlight qualified as LSP
import Move.Scene.LSP.Lint qualified as LSP
import Move.Scene.LSP.References qualified as LSP
import Move.Scene.LSP.Util (getUriParam, liftAppM)
import Prettyprinter
import Rule.AppLsp
import Rule.CodeAction qualified as CA
import System.IO (stdin, stdout)

lsp :: App Int
lsp = do
  liftIO $
    runQuietServer $
      ServerDefinition
        { defaultConfig = (),
          parseConfig = const $ const $ Right (),
          configSection = "Neut",
          onConfigChange = const $ return (),
          doInitialize = \env _req -> pure $ Right env,
          staticHandlers = const handlers,
          interpretHandler = \env -> Iso (runApp . runLspT env) liftIO,
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

handlers :: Handlers (AppLsp ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        return (),
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidOpen $ \_ -> do
        LSP.lint,
      notificationHandler SMethod_TextDocumentDidChange $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidSave $ \_ -> do
        LSP.lint,
      notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
        return (),
      notificationHandler SMethod_CancelRequest $ \_ -> do
        return (),
      notificationHandler SMethod_SetTrace $ \_ -> do
        return (),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        let pos = req ^. (J.params . J.position)
        h <- lift Complete.new
        itemListOrNone <- liftAppM $ lift $ toApp $ Complete.complete h uri pos
        let itemList = fromMaybe [] itemListOrNone
        responder $ Right $ InL $ List itemList,
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        h <- lift FindDefinition.new
        mLoc <- liftAppM $ FindDefinition.findDefinition h (req ^. J.params)
        case mLoc of
          Nothing ->
            responder $ Right $ InR $ InR Null
          Just ((_, loc), _) -> do
            responder $ Right $ InR $ InL $ List [loc],
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
        highlightsOrNone <- liftAppM $ LSP.highlight $ req ^. J.params
        case highlightsOrNone of
          Nothing ->
            responder $ Right $ InR Null
          Just highlights ->
            responder $ Right $ InL $ List highlights,
      requestHandler SMethod_TextDocumentReferences $ \req responder -> do
        refsOrNone <- liftAppM $ LSP.references $ req ^. J.params
        case refsOrNone of
          Nothing -> do
            responder $ Right $ InR Null
          Just refs -> do
            responder $ Right $ InL refs,
      requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        textEditList <- LSP.format False uri
        responder $ Right $ InL textEditList,
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        textOrNone <- liftAppM $ LSP.getSymbolInfo (req ^. J.params)
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
                    textEditList <- LSP.format True uri
                    let editParams =
                          ApplyWorkspaceEditParams (Just CA.minimizeImportsCommandTitle) $
                            WorkspaceEdit (Just (M.singleton uri textEditList)) Nothing Nothing
                    _ <- sendRequest SMethod_WorkspaceApplyEdit editParams (const (pure ()))
                    responder $ Right $ InR Null
            | commandName == CA.refreshCacheCommandName -> do
                _ <- liftAppM $ lift checkAll
                responder $ Right $ InR Null
          _ ->
            responder $ Right $ InR Null
    ]
