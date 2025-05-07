module Command.LSP.Move.Internal.References
  ( Handle,
    new,
    references,
  )
where

import Command.LSP.Move.Internal.FindDefinition qualified as FindDefinition
import Command.LSP.Move.Internal.FindReferences qualified as LSP
import Command.LSP.Move.Internal.GetAllCachesInModule qualified as GAC
import Command.LSP.Move.Internal.GetSource qualified as GetSource
import Control.Monad.Trans
import Error.Rule.EIO (EIO)
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Rule.Cache qualified as Cache
import Kernel.Rule.Source (Source (sourceFilePath, sourceModule))
import Kernel.Unravel.Move.Unravel qualified as Unravel
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
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
