module Move.Scene.LSP.References (references) where

import Move.Context.AppM
import Control.Monad.Trans
import Rule.Cache qualified as Cache
import Rule.Source (Source (sourceFilePath, sourceModule))
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Path
import Move.Scene.LSP.FindDefinition qualified as LSP
import Move.Scene.LSP.FindReferences qualified as LSP
import Move.Scene.LSP.GetAllCachesInModule qualified as LSP
import Move.Scene.LSP.GetSource qualified as LSP
import Move.Scene.Unravel (registerShiftMap)
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
