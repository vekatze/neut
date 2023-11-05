module Scene.LSP.FindReferences (findReferences) where

import Context.App
import Control.Monad
import Entity.Hint qualified as H
import Entity.LocationTree qualified as LT
import Language.LSP.Protocol.Types

findReferences ::
  H.Loc ->
  LT.LocationTree ->
  App [DocumentHighlight]
findReferences loc locationTree = do
  let locs = LT.findRef loc locationTree
  forM locs $ \(line, (colFrom, colTo)) -> do
    let symbolLen = fromIntegral $ colTo - colFrom
    let _start = Position {_line = fromIntegral (line - 1), _character = fromIntegral (colFrom - 1)}
    let _end = _start {_character = _character _start + symbolLen}
    let _range = Range {_start, _end}
    let _kind = Just DocumentHighlightKind_Read
    return $ DocumentHighlight {_range, _kind}
