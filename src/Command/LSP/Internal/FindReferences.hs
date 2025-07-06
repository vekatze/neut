module Command.LSP.Internal.FindReferences (findReferences) where

import Data.Maybe (fromMaybe)
import Kernel.Common.LocationTree qualified as LT
import Language.LSP.Protocol.Types
import Path

findReferences ::
  DefinitionLink ->
  LT.LocationTree ->
  [DocumentHighlight]
findReferences (DefinitionLink (LocationLink {_targetRange, _targetUri})) locationTree = do
  let Range {_start = Position {_line, _character}} = _targetRange
  let loc = (fromIntegral $ _line + 1, fromIntegral $ _character + 1)
  case uriToFilePath _targetUri of
    Nothing ->
      []
    Just defPath -> do
      let locs = LT.findRef loc locationTree
      let locs' = filter (\(path, _) -> pathEq defPath path) locs
      flip map locs' $ \(_, (line, (colFrom, colTo))) -> do
        let symbolLen = fromIntegral $ colTo - colFrom
        let baseCol = fromIntegral (colFrom - 1)
        let _start = Position {_line = fromIntegral (line - 1), _character = baseCol}
        let _end = _start {_character = baseCol + symbolLen}
        let _range = Range {_start, _end}
        let _kind = Just DocumentHighlightKind_Read
        DocumentHighlight {_range, _kind}

pathEq :: FilePath -> FilePath -> Bool
pathEq path1 path2 = do
  fromMaybe False $ do
    path1' <- parseAbsFile path1
    path2' <- parseAbsFile path2
    return $ path1' == path2'
