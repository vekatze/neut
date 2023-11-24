module Scene.LSP.GetSource (getSource) where

import Context.App
import Control.Lens hiding (Iso, List)
import Entity.Source (Source)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Scene.Source.Reflect qualified as Source

getSource ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri) =>
  p ->
  App (Maybe Source)
getSource params = do
  case uriToFilePath $ params ^. J.textDocument . J.uri of
    Nothing ->
      return Nothing
    Just fp -> do
      Source.reflect fp
