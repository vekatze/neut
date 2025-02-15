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
import Scene.Unravel (registerShiftMap)
import UnliftIO.Async (pooledForConcurrently)

references ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  AppM [Location]
references params = do
  lift registerShiftMap
  currentSource <- LSP.getSource params
  ((_, defLink), _) <- LSP.findDefinition params
  cacheSeq <- lift $ LSP.getAllLocationCachesInModule $ sourceModule currentSource
  fmap concat $ lift $ pooledForConcurrently cacheSeq $ \(path, cache) -> do
    refList <- LSP.findReferences defLink (Cache.locationTree cache)
    return $ map (toLocation $ sourceFilePath path) refList

toLocation :: Path Abs File -> DocumentHighlight -> Location
toLocation path (DocumentHighlight {_range}) = do
  Location {_uri = filePathToUri (toFilePath path), _range}
