module Main.Move.Scene.LSP.References
  ( Handle,
    new,
    references,
  )
where

import Control.Monad.Trans
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Main.Move.Context.EIO (EIO)
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.LSP.FindDefinition qualified as FindDefinition
import Main.Move.Scene.LSP.FindReferences qualified as LSP
import Main.Move.Scene.LSP.GetAllCachesInModule qualified as GAC
import Main.Move.Scene.LSP.GetSource qualified as GetSource
import Main.Move.Scene.Unravel qualified as Unravel
import Main.Rule.Cache qualified as Cache
import Main.Rule.Source (Source (sourceFilePath, sourceModule))
import Path
import UnliftIO.Async (pooledForConcurrently)

data Handle = Handle
  { unravelHandle :: Unravel.Handle,
    getSourceHandle :: GetSource.Handle,
    findDefinitionHandle :: FindDefinition.Handle,
    gacHandle :: GAC.Handle
  }

new ::
  Base.Handle ->
  IO Handle
new baseHandle = do
  unravelHandle <- liftIO $ Unravel.new baseHandle
  let getSourceHandle = GetSource.new baseHandle
  let findDefinitionHandle = FindDefinition.new baseHandle
  let gacHandle = GAC.new baseHandle
  return $ Handle {..}

references ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  Handle ->
  p ->
  EIO [Location]
references h params = do
  Unravel.registerShiftMap (unravelHandle h)
  currentSource <- GetSource.getSource (getSourceHandle h) params
  ((_, defLink), _) <- FindDefinition.findDefinition (findDefinitionHandle h) params
  cacheSeq <- GAC.getAllLocationCachesInModule (gacHandle h) $ sourceModule currentSource
  fmap concat $ lift $ pooledForConcurrently cacheSeq $ \(path, cache) -> do
    let refList = LSP.findReferences defLink (Cache.locationTree cache)
    return $ map (toLocation $ sourceFilePath path) refList

toLocation :: Path Abs File -> DocumentHighlight -> Location
toLocation path (DocumentHighlight {_range}) = do
  Location {_uri = filePathToUri (toFilePath path), _range}
