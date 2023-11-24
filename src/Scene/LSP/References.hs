module Scene.LSP.References (references) where

import Context.AppM
import Control.Monad
import Entity.Source (Source (sourceModule))
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Path
import Scene.LSP.FindDefinition qualified as LSP
import Scene.LSP.FindReferences qualified as LSP
import Scene.LSP.GetAllCachesInModule qualified as LSP
import Scene.LSP.GetSource qualified as LSP

references ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  AppM [Location]
references params = do
  currentSource <- LSP.getSource params
  (defLink, _) <- LSP.findDefinition params
  locTreeSeq <- LSP.getAllCachesInModule $ sourceModule currentSource
  fmap concat $ forM locTreeSeq $ \(path, locTree) -> do
    refList <- LSP.findReferences defLink locTree
    return $ map (toLocation path) refList

toLocation :: Path Abs File -> DocumentHighlight -> Location
toLocation path (DocumentHighlight {_range}) = do
  Location {_uri = filePathToUri (toFilePath path), _range}
