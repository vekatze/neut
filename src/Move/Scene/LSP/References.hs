module Move.Scene.LSP.References (references) where

import Control.Monad.Trans
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Move.Context.AppM
import Move.Context.EIO (toApp)
import Move.Scene.LSP.FindDefinition qualified as FindDefinition
import Move.Scene.LSP.FindReferences qualified as LSP
import Move.Scene.LSP.GetAllCachesInModule qualified as GAC
import Move.Scene.LSP.GetSource qualified as GetSource
import Move.Scene.Unravel qualified as Unravel
import Path
import Rule.Cache qualified as Cache
import Rule.Source (Source (sourceFilePath, sourceModule))
import UnliftIO.Async (pooledForConcurrently)

references ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  AppM [Location]
references params = do
  lift $ do
    h <- Unravel.new
    toApp $ Unravel.registerShiftMap h
  hgs <- lift GetSource.new
  currentSource <- lift $ toApp $ GetSource.getSource hgs params
  h <- lift FindDefinition.new
  ((_, defLink), _) <- lift $ toApp $ FindDefinition.findDefinition h params
  hgac <- lift GAC.new
  cacheSeq <- lift $ toApp $ GAC.getAllLocationCachesInModule hgac $ sourceModule currentSource
  fmap concat $ lift $ pooledForConcurrently cacheSeq $ \(path, cache) -> do
    let refList = LSP.findReferences defLink (Cache.locationTree cache)
    return $ map (toLocation $ sourceFilePath path) refList

toLocation :: Path Abs File -> DocumentHighlight -> Location
toLocation path (DocumentHighlight {_range}) = do
  Location {_uri = filePathToUri (toFilePath path), _range}
