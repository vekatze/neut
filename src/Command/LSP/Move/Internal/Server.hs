module Command.LSP.Move.Internal.Server
  ( lsp,
  )
where

import CommandParser.Rule.Config.Remark (lspConfig)
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
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
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

handlers :: Handlers (Lsp ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        return (),
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidOpen $ \_ -> do
        globalHandle <- liftIO $ Global.new lspConfig Nothing
        Lint.lint (Lint.new globalHandle),
      notificationHandler SMethod_TextDocumentDidChange $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidSave $ \_ -> do
        globalHandle <- liftIO $ Global.new lspConfig Nothing
        Lint.lint (Lint.new globalHandle),
      notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
        return (),
      notificationHandler SMethod_CancelRequest $ \_ -> do
        return (),
      notificationHandler SMethod_SetTrace $ \_ -> do
        return (),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        let pos = req ^. (J.params . J.position)
        globalHandle <- liftIO $ Global.new lspConfig Nothing
        completeHandle <- liftIO $ Complete.new globalHandle
        itemListOrNone <- run globalHandle $ Complete.complete completeHandle uri pos
        case itemListOrNone of
          Nothing ->
            responder $ Right $ InL $ List []
          Just itemList ->
            responder $ Right $ InL $ List itemList,
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        globalHandle <- liftIO $ Global.new lspConfig Nothing
        let findDefHandle = FindDefinition.new globalHandle
        mLoc <- run globalHandle $ FindDefinition.findDefinition findDefHandle (req ^. J.params)
        case mLoc of
          Nothing -> do
            responder $ Right $ InR $ InR Null
          Just ((_, loc), _) -> do
            responder $ Right $ InR $ InL $ List [loc],
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
        globalHandle <- liftIO $ Global.new lspConfig Nothing
        let highlightHandle = Highlight.new globalHandle
        highlightsOrNone <- run globalHandle $ Highlight.highlight highlightHandle $ req ^. J.params
        case highlightsOrNone of
          Nothing ->
            responder $ Right $ InR Null
          Just highlights ->
            responder $ Right $ InL $ List highlights,
      requestHandler SMethod_TextDocumentReferences $ \req responder -> do
        globalHandle <- liftIO $ Global.new lspConfig Nothing
        referencesHandle <- liftIO $ References.new globalHandle
        refsOrNone <- run globalHandle $ References.references referencesHandle $ req ^. J.params
        case refsOrNone of
          Nothing -> do
            responder $ Right $ InR Null
          Just refs -> do
            responder $ Right $ InL refs,
      requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
        let uri = req ^. (J.params . J.textDocument . J.uri)
        fileOrNone <- getVirtualFile (toNormalizedUri uri)
        globalHandle <- liftIO $ Global.new lspConfig Nothing
        let formatHandle = Format.new globalHandle
        textEditList <- run globalHandle $ Format.format formatHandle False uri fileOrNone
        let textEditList' = concat $ maybeToList textEditList
        responder $ Right $ InL textEditList',
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        globalHandle <- liftIO $ Global.new lspConfig Nothing
        textOrNone <- run globalHandle $ GetSymbolInfo.getSymbolInfo (req ^. J.params)
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
                    globalHandle <- liftIO $ Global.new lspConfig Nothing
                    let formatHandle = Format.new globalHandle
                    textEditList <- run globalHandle $ Format.format formatHandle True uri fileOrNone
                    let textEditList' = concat $ maybeToList textEditList
                    let editParams =
                          ApplyWorkspaceEditParams (Just CA.minimizeImportsCommandTitle) $
                            WorkspaceEdit (Just (M.singleton uri textEditList')) Nothing Nothing
                    _ <- sendRequest SMethod_WorkspaceApplyEdit editParams (const (pure ()))
                    responder $ Right $ InR Null
            | commandName == CA.refreshCacheCommandName -> do
                globalHandle <- liftIO $ Global.new lspConfig Nothing
                let checkHandle = Check.new globalHandle
                _ <- run globalHandle $ Check.checkAll checkHandle
                responder $ Right $ InR Null
          _ ->
            responder $ Right $ InR Null
    ]
