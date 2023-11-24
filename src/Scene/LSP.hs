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
import Scene.LSP.GetSource qualified as LSP
import Scene.LSP.Highlight qualified as LSP
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
      notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
        return (),
      notificationHandler SMethod_CancelRequest $ \_ -> do
        return (),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        itemList <- lift $ LSP.complete $ req ^. J.params . J.textDocument . J.uri
        responder $ Right $ InL $ List itemList,
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        mLoc <- lift $ LSP.findDefinition (req ^. J.params)
        case mLoc of
          Nothing ->
            return ()
          Just (loc, _) -> do
            responder $ Right $ InR $ InL $ List [loc],
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
        highlightsOrNone <- lift $ LSP.highlight $ req ^. J.params
        case highlightsOrNone of
          Nothing ->
            return ()
          Just highlights ->
            responder $ Right $ InL $ List highlights
    ]

runLSPApp :: Remark.Config -> App a -> IO a
runLSPApp cfg app = do
  runApp $ do
    Initialize.initializeCompiler cfg Nothing
    app
