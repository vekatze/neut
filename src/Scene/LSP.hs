module Scene.LSP (lsp) where

import Context.App
import Control.Lens hiding (Iso)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Maybe
import Entity.AppLsp
import Entity.Config.Remark qualified as Remark
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Scene.Initialize qualified as Initialize
import Scene.LSP.Complete qualified as LSP
import Scene.LSP.FindDefinition qualified as LSP
import Scene.LSP.FindReferences qualified as LSP
import Scene.LSP.GetLocationTree qualified as LSP
import Scene.LSP.Lint qualified as LSP

lsp :: Remark.Config -> App Int
lsp cfg = do
  liftIO $
    runServer $
      ServerDefinition
        { defaultConfig = (),
          onConfigurationChange = const $ pure $ Right (),
          doInitialize = \env _req -> pure $ Right env,
          staticHandlers = handlers,
          interpretHandler = \env -> Iso (runLSPApp cfg . runLspT env) liftIO,
          options = lspOptions
        }

handlers :: Handlers (AppLsp ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        return (),
      notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
        LSP.lint msg,
      notificationHandler SMethod_TextDocumentDidChange $ \_ -> do
        return (),
      notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
        LSP.lint msg,
      notificationHandler SMethod_CancelRequest $ \_ -> do
        return (),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        itemList <- lift $ LSP.complete $ req ^. J.params . J.textDocument . J.uri
        responder $ Right $ InL $ List itemList,
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        mLocTree <- lift $ LSP.getLocationTree $ req ^. J.params
        case mLocTree of
          Nothing ->
            return ()
          Just locTree -> do
            mLoc <- lift $ LSP.findDefinition (req ^. J.params) locTree
            responder $ Right $ InR $ InL $ List $ maybeToList mLoc,
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
        mLocTree <- lift $ LSP.getLocationTree $ req ^. J.params
        case mLocTree of
          Nothing ->
            return ()
          Just locTree -> do
            mLoc <- lift $ LSP.findDefinition (req ^. J.params) locTree
            case mLoc of
              Nothing ->
                return ()
              Just (DefinitionLink (LocationLink {_targetRange})) -> do
                let Range {_start = Position {_line, _character}} = _targetRange
                let line = fromIntegral $ _line + 1
                let col = fromIntegral $ _character + 1
                refs <- lift $ LSP.findReferences (line, col) locTree
                let _kind = Just DocumentHighlightKind_Write
                responder $ Right $ InL $ List $ DocumentHighlight {_range = _targetRange, _kind} : refs
    ]

runLSPApp :: Remark.Config -> App a -> IO a
runLSPApp cfg app = do
  runApp $ do
    Initialize.initializeCompiler cfg Nothing
    app
