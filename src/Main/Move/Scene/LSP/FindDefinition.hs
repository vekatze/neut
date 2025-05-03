module Main.Move.Scene.LSP.FindDefinition
  ( Handle,
    new,
    findDefinition,
  )
where

import Control.Lens hiding (Iso, List)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Main.Move.Context.EIO (EIO, liftMaybe)
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.LSP.GetLocationTree qualified as GetLocationTree
import Main.Move.Scene.LSP.GetSource qualified as GetSource
import Main.Rule.Hint qualified as H
import Main.Rule.LocationTree (LocationTree)
import Main.Rule.LocationTree qualified as LT

data Handle
  = Handle
  { getSourceHandle :: GetSource.Handle,
    getLocationTreeHandle :: GetLocationTree.Handle
  }

new :: Base.Handle -> Handle
new baseHandle = do
  let getSourceHandle = GetSource.new baseHandle
  let getLocationTreeHandle = GetLocationTree.new baseHandle
  Handle {..}

findDefinition ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  Handle ->
  p ->
  EIO ((LT.LocType, DefinitionLink), LocationTree)
findDefinition h params = do
  src <- GetSource.getSource (getSourceHandle h) params
  locTree <- GetLocationTree.getLocationTree (getLocationTreeHandle h) src
  defLink <- liftMaybe $ _findDefinition params locTree
  return (defLink, locTree)

_findDefinition ::
  (J.HasPosition p Position) =>
  p ->
  LT.LocationTree ->
  Maybe (LT.LocType, DefinitionLink)
_findDefinition params locationTree = do
  let line = fromEnum (params ^. J.position . J.line) + 1
  let col = fromEnum (params ^. J.position . J.character) + 1
  (locType, m, _, symbolLen) <- LT.find line col locationTree
  let defPath = H.metaFileName m
  let (defLine, defCol) = H.metaLocation m
  let defFilePath' = filePathToUri defPath
  let _start = Position {_line = fromIntegral (defLine - 1), _character = fromIntegral (defCol - 1)}
  let _end = _start {_character = _character _start + fromIntegral symbolLen}
  let range = Range {_start, _end}
  return
    ( locType,
      DefinitionLink $
        LocationLink
          { _originSelectionRange = Nothing,
            _targetUri = defFilePath',
            _targetRange = range,
            _targetSelectionRange = range
          }
    )
