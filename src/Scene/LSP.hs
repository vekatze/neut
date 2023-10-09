module Scene.LSP (lsp) where

import Context.App
import Control.Lens hiding (Iso)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Maybe
import Entity.AppLsp
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Scene.LSP.Complete qualified as LSP
import Scene.LSP.FindDefinition qualified as LSP
import Scene.LSP.Lint qualified as LSP

lsp :: App Int
lsp = do
  appEnv <- getEnv
  liftIO $
    runServer $
      ServerDefinition
        { defaultConfig = (),
          onConfigurationChange = const $ pure $ Right (),
          doInitialize = \env _req -> pure $ Right env,
          staticHandlers = handlers,
          interpretHandler = \env -> Iso (runAppInEnv appEnv . runLspT env) liftIO,
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
        mLoc <- lift $ LSP.findDefinition $ req ^. J.params
        responder $ Right $ InR $ InL $ List $ maybeToList mLoc
    ]
