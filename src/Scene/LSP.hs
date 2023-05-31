module Scene.LSP (lsp) where

import Context.App
import Control.Lens hiding (Iso, List)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Maybe
import Entity.AppLsp
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens qualified as J
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
    [ notificationHandler SInitialized $ \_not -> do
        return (),
      notificationHandler STextDocumentDidOpen $ \msg -> do
        LSP.lint msg,
      notificationHandler STextDocumentDidChange $ \_ -> do
        return (),
      notificationHandler STextDocumentDidSave $ \msg -> do
        LSP.lint msg,
      notificationHandler SCancelRequest $ \_ -> do
        return (),
      requestHandler STextDocumentCompletion $ \req responder -> do
        itemList <- lift $ LSP.complete $ req ^. J.params . J.textDocument . J.uri
        responder $ Right $ InL $ List itemList,
      requestHandler STextDocumentDefinition $ \req responder -> do
        mLoc <- lift $ LSP.findDefinition $ req ^. J.params
        responder $ Right $ InR $ InL $ List $ maybeToList mLoc
    ]
