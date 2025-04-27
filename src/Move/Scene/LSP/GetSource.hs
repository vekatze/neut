module Move.Scene.LSP.GetSource
  ( Handle,
    new,
    getSource,
  )
where

import Control.Lens hiding (Iso, List)
import Control.Monad.Trans
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Move.Context.App (App)
import Move.Context.AppM
import Move.Context.EIO (toApp)
import Move.Scene.Source.Reflect qualified as SourceReflect
import Rule.Source (Source)

newtype Handle
  = Handle
  { sourceReflectHandle :: SourceReflect.Handle
  }

new :: App Handle
new = do
  sourceReflectHandle <- SourceReflect.new
  return $ Handle {..}

getSource ::
  Handle ->
  (J.HasTextDocument p a1, J.HasUri a1 Uri) =>
  p ->
  AppM Source
getSource h params = do
  fp <- liftMaybe $ uriToFilePath $ params ^. J.textDocument . J.uri
  lift (toApp $ SourceReflect.reflect (sourceReflectHandle h) fp) >>= liftMaybe
