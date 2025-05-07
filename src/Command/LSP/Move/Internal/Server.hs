module Command.LSP.Move.Internal.Server
  ( Handle,
    new,
    lsp,
  )
where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Colog.Core qualified as L
import Command.Common.Move.Check qualified as Check
import Command.LSP.Move.Internal.Complete qualified as Complete
import Command.LSP.Move.Internal.FindDefinition qualified as FindDefinition
import Command.LSP.Move.Internal.Format qualified as Format
import Command.LSP.Move.Internal.GetSymbolInfo qualified as GetSymbolInfo
import Command.LSP.Move.Internal.Highlight qualified as Highlight
import Command.LSP.Move.Internal.Lint qualified as Lint
import Command.LSP.Move.Internal.References qualified as References
import Command.LSP.Move.Internal.Util (getUriParam, run)
import Command.LSP.Rule.CodeAction qualified as CA
import Command.LSP.Rule.Lsp
import Control.Lens hiding (Iso)
import Control.Monad.IO.Class
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Kernel.Move.Scene.Init.Base qualified as Base
import Language.LSP.Logging
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Prettyprinter
import System.IO (stdin, stdout)

newtype Handle = Handle
  { baseHandle :: Base.Handle
  }

new :: Base.Handle -> Handle
new baseHandle = do
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
        baseHandle <- liftIO $ Base.refresh (baseHandle h)
        Lint.lint (Lint.new baseHandle),
      notificationHandler SMethod_TextDocumentDidChange $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidSave $ \_ -> do
        baseHandle <- liftIO $ Base.refresh (baseHandle h)
        Lint.lint (Lint.new baseHandle),
      notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
        return (),
      notificationHandler SMethod_CancelRequest $ \_ -> do
        return (),
      notificationHandler SMethod_SetTrace $ \_ -> do
        return (),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        let pos = req ^. (J.params . J.position)
        baseHandle <- liftIO $ Base.refresh (baseHandle h)
        completeHandle <- liftIO $ Complete.new baseHandle
        itemListOrNone <- run baseHandle $ Complete.complete completeHandle uri pos
        case itemListOrNone of
          Nothing ->
            responder $ Right $ InL $ List []
          Just itemList ->
            responder $ Right $ InL $ List itemList,
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        baseHandle <- liftIO $ Base.refresh (baseHandle h)
        let findDefHandle = FindDefinition.new baseHandle
        mLoc <- run baseHandle $ FindDefinition.findDefinition findDefHandle (req ^. J.params)
        case mLoc of
          Nothing -> do
            responder $ Right $ InR $ InR Null
          Just ((_, loc), _) -> do
            responder $ Right $ InR $ InL $ List [loc],
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
        baseHandle <- liftIO $ Base.refresh (baseHandle h)
        let highlightHandle = Highlight.new baseHandle
        highlightsOrNone <- run baseHandle $ Highlight.highlight highlightHandle $ req ^. J.params
        case highlightsOrNone of
          Nothing ->
            responder $ Right $ InR Null
          Just highlights ->
            responder $ Right $ InL $ List highlights,
      requestHandler SMethod_TextDocumentReferences $ \req responder -> do
        baseHandle <- liftIO $ Base.refresh (baseHandle h)
        referencesHandle <- liftIO $ References.new baseHandle
        refsOrNone <- run baseHandle $ References.references referencesHandle $ req ^. J.params
        case refsOrNone of
          Nothing -> do
            responder $ Right $ InR Null
          Just refs -> do
            responder $ Right $ InL refs,
      requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        fileOrNone <- getVirtualFile (toNormalizedUri uri)
        baseHandle <- liftIO $ Base.refresh (baseHandle h)
        let formatHandle = Format.new baseHandle
        textEditList <- run baseHandle $ Format.format formatHandle False uri fileOrNone
        let textEditList' = concat $ maybeToList textEditList
        responder $ Right $ InL textEditList',
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        baseHandle <- liftIO $ Base.refresh (baseHandle h)
        textOrNone <- run baseHandle $ GetSymbolInfo.getSymbolInfo (req ^. J.params)
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
                    baseHandle <- liftIO $ Base.refresh (baseHandle h)
                    let formatHandle = Format.new baseHandle
                    textEditList <- run baseHandle $ Format.format formatHandle True uri fileOrNone
                    let textEditList' = concat $ maybeToList textEditList
                    let editParams =
                          ApplyWorkspaceEditParams (Just CA.minimizeImportsCommandTitle) $
                            WorkspaceEdit (Just (M.singleton uri textEditList')) Nothing Nothing
                    _ <- sendRequest SMethod_WorkspaceApplyEdit editParams (const (pure ()))
                    responder $ Right $ InR Null
            | commandName == CA.refreshCacheCommandName -> do
                baseHandle <- liftIO $ Base.refresh (baseHandle h)
                let checkHandle = Check.new baseHandle
                _ <- run baseHandle $ Check.checkAll checkHandle
                responder $ Right $ InR Null
          _ ->
            responder $ Right $ InR Null
    ]
