module Move.Scene.LSP
  ( Handle,
    new,
    lsp,
  )
where

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
import Move.Context.AppM (liftEIO)
import Move.Context.Env qualified as Env
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Check qualified as Check
import Move.Scene.LSP.Complete qualified as Complete
import Move.Scene.LSP.FindDefinition qualified as FindDefinition
import Move.Scene.LSP.Format qualified as Format
import Move.Scene.LSP.GetSymbolInfo qualified as GetSymbolInfo
import Move.Scene.LSP.Highlight qualified as Highlight
import Move.Scene.LSP.Lint qualified as Lint
import Move.Scene.LSP.References qualified as References
import Move.Scene.LSP.Util (getUriParam, liftAppM)
import Prettyprinter
import Rule.AppLsp
import Rule.CodeAction qualified as CA
import System.IO (stdin, stdout)

data Handle
  = Handle
  { envHandle :: Env.Handle,
    gensymHandle :: Gensym.Handle,
    completeHandle :: Complete.Handle,
    findDefinitionHandle :: FindDefinition.Handle,
    highlightHandle :: Highlight.Handle,
    referencesHandle :: References.Handle,
    formatHandle :: Format.Handle
  }

new :: Env.Handle -> Gensym.Handle -> App Handle
new envHandle gensymHandle = do
  completeHandle <- Complete.new gensymHandle
  findDefinitionHandle <- FindDefinition.new gensymHandle
  highlightHandle <- Highlight.new gensymHandle
  referencesHandle <- References.new gensymHandle
  formatHandle <- Format.new envHandle gensymHandle
  return $ Handle {..}

lsp :: Handle -> App Int
lsp h = do
  liftIO $
    runQuietServer $
      ServerDefinition
        { defaultConfig = (),
          parseConfig = const $ const $ Right (),
          configSection = "Neut",
          onConfigChange = const $ return (),
          doInitialize = \env _req -> pure $ Right env,
          staticHandlers = const (handlers h),
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

handlers :: Handle -> Handlers (AppLsp ())
handlers h =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        return (),
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidOpen $ \_ -> do
        h' <- lift $ Lint.new (envHandle h) (gensymHandle h)
        Lint.lint h',
      notificationHandler SMethod_TextDocumentDidChange $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidSave $ \_ -> do
        h' <- lift $ Lint.new (envHandle h) (gensymHandle h)
        Lint.lint h',
      notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
        return (),
      notificationHandler SMethod_CancelRequest $ \_ -> do
        return (),
      notificationHandler SMethod_SetTrace $ \_ -> do
        return (),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        let pos = req ^. (J.params . J.position)
        itemListOrNone <- liftAppM (gensymHandle h) $ liftEIO $ Complete.complete (completeHandle h) uri pos
        let itemList = fromMaybe [] itemListOrNone
        responder $ Right $ InL $ List itemList,
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        mLoc <- liftAppM (gensymHandle h) $ liftEIO $ FindDefinition.findDefinition (findDefinitionHandle h) (req ^. J.params)
        case mLoc of
          Nothing ->
            responder $ Right $ InR $ InR Null
          Just ((_, loc), _) -> do
            responder $ Right $ InR $ InL $ List [loc],
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
        highlightsOrNone <- liftAppM (gensymHandle h) $ liftEIO $ Highlight.highlight (highlightHandle h) $ req ^. J.params
        case highlightsOrNone of
          Nothing ->
            responder $ Right $ InR Null
          Just highlights ->
            responder $ Right $ InL $ List highlights,
      requestHandler SMethod_TextDocumentReferences $ \req responder -> do
        refsOrNone <- liftAppM (gensymHandle h) $ liftEIO $ References.references (referencesHandle h) $ req ^. J.params
        case refsOrNone of
          Nothing -> do
            responder $ Right $ InR Null
          Just refs -> do
            responder $ Right $ InL refs,
      requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        fileOrNone <- getVirtualFile (toNormalizedUri uri)
        textEditList <- liftAppM (gensymHandle h) $ liftEIO $ Format.format (formatHandle h) False uri fileOrNone
        let textEditList' = concat $ maybeToList textEditList
        responder $ Right $ InL textEditList',
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        h' <- lift $ GetSymbolInfo.new (envHandle h) (gensymHandle h)
        textOrNone <- liftAppM (gensymHandle h) $ GetSymbolInfo.getSymbolInfo h' (req ^. J.params)
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
                    textEditList <- liftAppM (gensymHandle h) $ liftEIO $ Format.format (formatHandle h) True uri fileOrNone
                    let textEditList' = concat $ maybeToList textEditList
                    let editParams =
                          ApplyWorkspaceEditParams (Just CA.minimizeImportsCommandTitle) $
                            WorkspaceEdit (Just (M.singleton uri textEditList')) Nothing Nothing
                    _ <- sendRequest SMethod_WorkspaceApplyEdit editParams (const (pure ()))
                    responder $ Right $ InR Null
            | commandName == CA.refreshCacheCommandName -> do
                hck <- lift $ Check.new (envHandle h) (gensymHandle h)
                _ <- liftAppM (gensymHandle h) $ lift $ Check.checkAll hck
                responder $ Right $ InR Null
          _ ->
            responder $ Right $ InR Null
    ]
