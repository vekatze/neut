module Scene.LSP (lsp) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Colog.Core qualified as L
import Context.App
import Context.AppM
import Control.Lens hiding (Iso)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Maybe
import Data.Text qualified as T
import Entity.AppLsp
import Entity.Config.Remark qualified as Remark
import Language.LSP.Logging
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Prettyprinter
import Scene.Initialize qualified as Initialize
import Scene.LSP.Complete qualified as LSP
import Scene.LSP.FindDefinition qualified as LSP
import Scene.LSP.Format qualified as LSP
import Scene.LSP.GetSymbolInfo qualified as LSP
import Scene.LSP.Highlight qualified as LSP
import Scene.LSP.Lint qualified as LSP
import Scene.LSP.References qualified as LSP
import System.IO (stdin, stdout)

lsp :: Remark.Config -> App Int
lsp cfg = do
  Initialize.initializeCompiler cfg
  liftIO $
    runQuietServer $
      ServerDefinition
        { defaultConfig = (),
          parseConfig = const $ const $ Right (),
          configSection = "Neut",
          onConfigChange = const $ return (),
          doInitialize = \env _req -> pure $ Right env,
          staticHandlers = const handlers,
          interpretHandler = \env -> Iso (runLSPApp cfg . runLspT env) liftIO,
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
      notificationHandler SMethod_TextDocumentWillSave $ \_ -> do
        return (),
      notificationHandler SMethod_SetTrace $ \_ -> do
        return (),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        let pos = req ^. (J.params . J.position)
        itemListOrNone <- lift $ runAppM $ LSP.complete uri pos
        let itemList = fromMaybe [] itemListOrNone
        responder $ Right $ InL $ List itemList,
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        mLoc <- lift $ runAppM $ LSP.findDefinition (req ^. J.params)
        case mLoc of
          Nothing ->
            responder $ Right $ InR $ InR Null
          Just ((_, loc), _) -> do
            responder $ Right $ InR $ InL $ List [loc],
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
        highlightsOrNone <- lift $ runAppM $ LSP.highlight $ req ^. J.params
        case highlightsOrNone of
          Nothing ->
            responder $ Right $ InR Null
          Just highlights ->
            responder $ Right $ InL $ List highlights,
      requestHandler SMethod_TextDocumentReferences $ \req responder -> do
        refsOrNone <- lift $ runAppM $ LSP.references $ req ^. J.params
        case refsOrNone of
          Nothing -> do
            responder $ Right $ InR Null
          Just refs -> do
            responder $ Right $ InL refs,
      requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        textEditList <- LSP.format uri
        responder $ Right $ InL textEditList,
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        textOrNone <- lift $ runAppM $ LSP.getSymbolInfo (req ^. J.params)
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
                    }
    ]

runLSPApp :: Remark.Config -> App a -> IO a
runLSPApp cfg app = do
  runApp $ do
    Initialize.initializeCompiler cfg
    app
