module Move.Scene.LSP.GetSource (getSource) where

import Control.Lens hiding (Iso, List)
import Control.Monad.Trans
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Move.Context.AppM
import Move.Context.EIO (toApp)
import Move.Scene.Source.Reflect qualified as SourceReflect
import Rule.Source (Source)

getSource ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri) =>
  p ->
  AppM Source
getSource params = do
  fp <- liftMaybe $ uriToFilePath $ params ^. J.textDocument . J.uri
  h' <- lift SourceReflect.new
  lift (toApp $ SourceReflect.reflect h' fp) >>= liftMaybe
