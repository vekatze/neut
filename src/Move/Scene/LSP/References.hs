module Move.Scene.LSP.References
  ( Handle,
    new,
    references,
  )
where

import Control.Monad.Trans
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App (App)
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.Locator qualified as Locator
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Tag qualified as Tag
import Move.Context.Unused qualified as Unused
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.LSP.FindDefinition qualified as FindDefinition
import Move.Scene.LSP.FindReferences qualified as LSP
import Move.Scene.LSP.GetAllCachesInModule qualified as GAC
import Move.Scene.LSP.GetSource qualified as GetSource
import Move.Scene.Unravel qualified as Unravel
import Path
import Rule.Cache qualified as Cache
import Rule.Source (Source (sourceFilePath, sourceModule))
import UnliftIO.Async (pooledForConcurrently)

data Handle
  = Handle
  { unravelHandle :: Unravel.Handle,
    getSourceHandle :: GetSource.Handle,
    findDefinitionHandle :: FindDefinition.Handle,
    gacHandle :: GAC.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Debug.Handle -> Locator.Handle -> OptimizableData.Handle -> KeyArg.Handle -> Unused.Handle -> Tag.Handle -> Antecedent.Handle -> App Handle
new envHandle gensymHandle debugHandle locatorHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle = do
  unravelHandle <- Unravel.new envHandle gensymHandle debugHandle locatorHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle
  getSourceHandle <- GetSource.new envHandle gensymHandle
  findDefinitionHandle <- FindDefinition.new envHandle gensymHandle debugHandle
  gacHandle <- GAC.new envHandle debugHandle antecedentHandle
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
