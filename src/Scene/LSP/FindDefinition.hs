module Scene.LSP.FindDefinition (findDefinition) where

import Context.App
import Context.Cache qualified as Cache
import Context.Unravel qualified as Unravel
import Control.Lens hiding (Iso, List)
import Entity.Cache qualified as Cache
import Entity.Hint qualified as H
import Entity.LocationTree qualified as LT
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Scene.Source.Reflect qualified as Source
import Scene.Unravel

findDefinition ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  App (Maybe DefinitionLink)
findDefinition params = do
  case uriToFilePath $ params ^. J.textDocument . J.uri of
    Nothing ->
      return Nothing
    Just fp -> do
      mSrc <- Source.reflect fp
      case mSrc of
        Nothing -> do
          return Nothing
        Just src -> do
          Unravel.initialize
          _ <- unravel' src
          mCache <- Cache.loadCacheOptimistically src
          case mCache of
            Nothing -> do
              return Nothing
            Just cache -> do
              let locationTree = Cache.locationTree cache
              let line = fromEnum (params ^. J.position . J.line) + 1
              let col = fromEnum (params ^. J.position . J.character) + 1
              case LT.find line col locationTree of
                Nothing -> do
                  return Nothing
                Just m -> do
                  let defPath = H.metaFileName m
                  let (defLine, defCol) = H.metaLocation m
                  let defFilePath' = filePathToUri defPath
                  let pos = Position {_line = fromIntegral (defLine - 1), _character = fromIntegral (defCol - 1)}
                  let range = Range {_start = pos, _end = pos}
                  return $
                    Just $
                      DefinitionLink $
                        LocationLink
                          { _originSelectionRange = Nothing,
                            _targetUri = defFilePath',
                            _targetRange = range,
                            _targetSelectionRange = range
                          }
