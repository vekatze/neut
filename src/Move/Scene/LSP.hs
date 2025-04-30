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
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Language.LSP.Logging
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Move.Scene.Check qualified as Check
import Move.Scene.Init.Compiler qualified as InitCompiler
import Move.Scene.LSP.Complete qualified as Complete
import Move.Scene.LSP.FindDefinition qualified as FindDefinition
import Move.Scene.LSP.Format qualified as Format
import Move.Scene.LSP.GetSymbolInfo qualified as GetSymbolInfo
import Move.Scene.LSP.Highlight qualified as Highlight
import Move.Scene.LSP.Lint qualified as Lint
import Move.Scene.LSP.References qualified as References
import Move.Scene.LSP.Util (getUriParam, runOneShot)
import Move.Scene.LSP.Util qualified as LspUtil
import Prettyprinter
import Rule.CodeAction qualified as CA
import Rule.Lsp
import System.IO (stdin, stdout)

data Handle
  = Handle
  { initCompilerHandle :: InitCompiler.Handle,
    completeHandle :: Complete.Handle,
    findDefinitionHandle :: FindDefinition.Handle,
    highlightHandle :: Highlight.Handle,
    referencesHandle :: References.Handle,
    formatHandle :: Format.Handle,
    checkHandle :: Check.Handle,
    getSymbolInfoHandle :: GetSymbolInfo.Handle,
    lintHandle :: Lint.Handle,
    lspUtilHandle :: LspUtil.Handle
  }

new ::
  InitCompiler.Handle ->
  Complete.Handle ->
  FindDefinition.Handle ->
  Highlight.Handle ->
  References.Handle ->
  Format.Handle ->
  Check.Handle ->
  GetSymbolInfo.Handle ->
  Lint.Handle ->
  LspUtil.Handle ->
  Handle
new initCompilerHandle completeHandle findDefinitionHandle highlightHandle referencesHandle formatHandle checkHandle getSymbolInfoHandle lintHandle lspUtilHandle = do
  Handle {..}

lsp :: Handle -> IO Int
lsp h = do
  runQuietServer $
    ServerDefinition
      { defaultConfig = (),
        parseConfig = const $ const $ Right (),
        configSection = "Neut",
        onConfigChange = const $ return (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = const (handlers h),
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

handlers :: Handle -> Handlers (Lsp ())
handlers h =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        return (),
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidOpen $ \_ -> do
        Lint.lint (lintHandle h),
      notificationHandler SMethod_TextDocumentDidChange $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidSave $ \_ -> do
        Lint.lint (lintHandle h),
      notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
        return (),
      notificationHandler SMethod_CancelRequest $ \_ -> do
        return (),
      notificationHandler SMethod_SetTrace $ \_ -> do
        return (),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        let pos = req ^. (J.params . J.position)
        itemListOrNone <- runOneShot (lspUtilHandle h) $ Complete.complete (completeHandle h) uri pos
        case itemListOrNone of
          Nothing ->
            responder $ Right $ InL $ List []
          Just itemList ->
            responder $ Right $ InL $ List itemList,
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        mLoc <- runOneShot (lspUtilHandle h) $ FindDefinition.findDefinition (findDefinitionHandle h) (req ^. J.params)
        case mLoc of
          Nothing -> do
            responder $ Right $ InR $ InR Null
          Just ((_, loc), _) -> do
            responder $ Right $ InR $ InL $ List [loc],
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
        highlightsOrNone <- runOneShot (lspUtilHandle h) $ Highlight.highlight (highlightHandle h) $ req ^. J.params
        case highlightsOrNone of
          Nothing ->
            responder $ Right $ InR Null
          Just highlights ->
            responder $ Right $ InL $ List highlights,
      requestHandler SMethod_TextDocumentReferences $ \req responder -> do
        refsOrNone <- runOneShot (lspUtilHandle h) $ References.references (referencesHandle h) $ req ^. J.params
        case refsOrNone of
          Nothing -> do
            responder $ Right $ InR Null
          Just refs -> do
            responder $ Right $ InL refs,
      requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        fileOrNone <- getVirtualFile (toNormalizedUri uri)
        textEditList <- runOneShot (lspUtilHandle h) $ Format.format (formatHandle h) False uri fileOrNone
        let textEditList' = concat $ maybeToList textEditList
        responder $ Right $ InL textEditList',
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        textOrNone <- runOneShot (lspUtilHandle h) $ GetSymbolInfo.getSymbolInfo (getSymbolInfoHandle h) (req ^. J.params)
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
                    textEditList <- runOneShot (lspUtilHandle h) $ Format.format (formatHandle h) True uri fileOrNone
                    let textEditList' = concat $ maybeToList textEditList
                    let editParams =
                          ApplyWorkspaceEditParams (Just CA.minimizeImportsCommandTitle) $
                            WorkspaceEdit (Just (M.singleton uri textEditList')) Nothing Nothing
                    _ <- sendRequest SMethod_WorkspaceApplyEdit editParams (const (pure ()))
                    responder $ Right $ InR Null
            | commandName == CA.refreshCacheCommandName -> do
                _ <- runOneShot (lspUtilHandle h) $ Check.checkAll (checkHandle h)
                responder $ Right $ InR Null
          _ ->
            responder $ Right $ InR Null
    ]
