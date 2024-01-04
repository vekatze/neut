module Scene.LSP.FindDefinition (findDefinition) where

import Context.AppM
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
  AppM (DefinitionLink, LocationTree)
findDefinition params = do
  src <- LSP.getSource params
  locTree <- LSP.getLocationTree src
  defLink <- _findDefinition params locTree
  return (defLink, locTree)

_findDefinition ::
  (J.HasPosition p Position) =>
  p ->
  LT.LocationTree ->
  AppM DefinitionLink
_findDefinition params locationTree = do
  let line = fromEnum (params ^. J.position . J.line) + 1
  let col = fromEnum (params ^. J.position . J.character) + 1
  (_, m, (colFrom, colTo)) <- liftMaybe $ LT.find line col locationTree
  let defPath = H.metaFileName m
  let (defLine, defCol) = H.metaLocation m
  let defFilePath' = filePathToUri defPath
  let symbolLen = fromIntegral $ colTo - colFrom
  let _start = Position {_line = fromIntegral (defLine - 1), _character = fromIntegral (defCol - 1)}
  let _end = _start {_character = _character _start + symbolLen}
  let range = Range {_start, _end}
  return $
    DefinitionLink $
      LocationLink
        { _originSelectionRange = Nothing,
          _targetUri = defFilePath',
          _targetRange = range,
          _targetSelectionRange = range
        }
