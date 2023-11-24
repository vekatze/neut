module Scene.LSP.FindDefinition (findDefinition) where

import Context.App
import Control.Lens hiding (Iso, List)
import Entity.Hint qualified as H
import Entity.LocationTree (LocationTree)
import Entity.LocationTree qualified as LT
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Scene.LSP.GetLocationTree qualified as LSP
import Scene.LSP.GetSource qualified as LSP

findDefinition ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  App (Maybe (DefinitionLink, LocationTree))
findDefinition params = do
  srcOrNone <- LSP.getSource params
  case srcOrNone of
    Nothing ->
      return Nothing
    Just src -> do
      mLocTree <- LSP.getLocationTree src
      case mLocTree of
        Nothing ->
          return Nothing
        Just locTree -> do
          defLinkOrNone <- _findDefinition params locTree
          case defLinkOrNone of
            Nothing ->
              return Nothing
            Just defLink ->
              return $ Just (defLink, locTree)

_findDefinition ::
  (J.HasPosition p Position) =>
  p ->
  LT.LocationTree ->
  App (Maybe DefinitionLink)
_findDefinition params locationTree = do
  let line = fromEnum (params ^. J.position . J.line) + 1
  let col = fromEnum (params ^. J.position . J.character) + 1
  case LT.find line col locationTree of
    Nothing -> do
      return Nothing
    Just (m, (colFrom, colTo)) -> do
      let defPath = H.metaFileName m
      let (defLine, defCol) = H.metaLocation m
      let defFilePath' = filePathToUri defPath
      let symbolLen = fromIntegral $ colTo - colFrom
      let _start = Position {_line = fromIntegral (defLine - 1), _character = fromIntegral (defCol - 1)}
      let _end = _start {_character = _character _start + symbolLen}
      let range = Range {_start, _end}
      return $
        Just $
          DefinitionLink $
            LocationLink
              { _originSelectionRange = Nothing,
                _targetUri = defFilePath',
                _targetRange = range,
                _targetSelectionRange = range
              }
