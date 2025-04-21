module Move.Scene.LSP.GetSource (getSource) where

import Move.Context.AppM
import Control.Lens hiding (Iso, List)
import Control.Monad.Trans
import Rule.Source (Source)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Move.Scene.Source.Reflect qualified as Source

getSource ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri) =>
  p ->
  AppM Source
getSource params = do
  fp <- liftMaybe $ uriToFilePath $ params ^. J.textDocument . J.uri
  lift (Source.reflect fp) >>= liftMaybe
