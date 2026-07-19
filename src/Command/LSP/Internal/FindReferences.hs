module Command.LSP.Internal.FindReferences (findReferences) where

import Kernel.Common.LocationTree qualified as LT
import Language.LSP.Protocol.Types

findReferences ::
  LT.SymbolName ->
  LT.LocationTree ->
  [DocumentHighlight]
findReferences symbolName locationTree = do
  let locs = LT.findSymbolRef symbolName locationTree
  flip map locs $ \(_, (line, (colFrom, colTo))) -> do
    let symbolLen = fromIntegral $ colTo - colFrom
    let baseCol = fromIntegral (colFrom - 1)
    let _start = Position {_line = fromIntegral (line - 1), _character = baseCol}
    let _end = _start {_character = baseCol + symbolLen}
    let _range = Range {_start, _end}
    let _kind = Just DocumentHighlightKind_Read
    DocumentHighlight {_range, _kind}
