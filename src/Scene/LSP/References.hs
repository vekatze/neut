module Scene.LSP.References (references) where

import Context.AppM
import Control.Monad.Trans
import Entity.Cache qualified as Cache
import Entity.Source (Source (sourceFilePath, sourceModule))
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Path
import Scene.LSP.FindDefinition qualified as LSP
import Scene.LSP.FindReferences qualified as LSP
import Scene.LSP.GetAllCachesInModule qualified as LSP
import Scene.LSP.GetSource qualified as LSP
import UnliftIO.Async (forConcurrently)

references ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  AppM [Location]
references params = do
  currentSource <- LSP.getSource params
  ((_, defLink), _) <- LSP.findDefinition params
  cacheSeq <- lift $ LSP.getAllCachesInModule $ sourceModule currentSource
  fmap concat $ lift $ forConcurrently cacheSeq $ \(path, cache) -> do
    refList <- LSP.findReferences defLink (Cache.locationTree cache)
    return $ map (toLocation $ sourceFilePath path) refList

toLocation :: Path Abs File -> DocumentHighlight -> Location
toLocation path (DocumentHighlight {_range}) = do
  Location {_uri = filePathToUri (toFilePath path), _range}
