module Scene.LSP (lsp) where

import Context.App
import Control.Monad.IO.Class
import Control.Monad.Trans
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types qualified as J
import Scene.LSP.FindDefinition qualified as LSP

type AppLsp config = LspT config App

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
          options = defaultOptions
        }

handlers :: Handlers (AppLsp ())
handlers =
  mconcat
    [ requestHandler STextDocumentDefinition $ \req responder -> do
        let RequestMessage _ _ _ (DefinitionParams ident reqPos _ _) = req
        let TextDocumentIdentifier uri = ident
        case J.uriToFilePath uri of
          Just path -> do
            let reqLine = _line reqPos
            let reqCol = _character reqPos
            mLoc <- lift $ LSP.findDefinition path (fromEnum reqLine + 1, fromEnum reqCol + 1)
            case mLoc of
              Just (defFilePath, (defLine, defCol)) -> do
                let defFilePath' = J.filePathToUri defFilePath
                let pos = Position {_line = fromIntegral (defLine - 1), _character = fromIntegral (defCol - 1)}
                let range = Range {_start = pos, _end = pos}
                let loc = Location {_uri = defFilePath', _range = range}
                responder $ Right $ InL loc
              Nothing ->
                responder $ Right $ InR $ InL $ List []
          Nothing ->
            responder $ Right $ InR $ InL $ List []
    ]
