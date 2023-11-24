module Scene.LSP.FindReferences (findReferences) where

import Context.AppM
import Control.Monad
import Data.Maybe (fromMaybe)
import Entity.Hint qualified as H
import Entity.LocationTree qualified as LT
import Language.LSP.Protocol.Types
import Path

findReferences ::
  H.Loc ->
  Uri ->
  LT.LocationTree ->
  AppM [DocumentHighlight]
findReferences loc defUri locationTree = do
  defPath <- liftMaybe $ uriToFilePath defUri
  let locs = LT.findRef loc locationTree
  let locs' = filter (\(path, _) -> pathEq defPath path) locs
  forM locs' $ \(_, (line, (colFrom, colTo))) -> do
    let symbolLen = fromIntegral $ colTo - colFrom
    let _start = Position {_line = fromIntegral (line - 1), _character = fromIntegral (colFrom - 1)}
    let _end = _start {_character = _character _start + symbolLen}
    let _range = Range {_start, _end}
    let _kind = Just DocumentHighlightKind_Read
    return $ DocumentHighlight {_range, _kind}

pathEq :: FilePath -> FilePath -> Bool
pathEq path1 path2 = do
  fromMaybe False $ do
    path1' <- parseAbsFile path1
    path2' <- parseAbsFile path2
    return $ path1' == path2'
