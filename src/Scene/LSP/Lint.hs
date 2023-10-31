module Scene.LSP.Lint (lint) where

import Context.App
import Control.Lens hiding (Iso, List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.ByteString qualified as B
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding
import Debug.Trace (trace)
import Entity.AppLsp
import Entity.FilePos qualified as FP
import Entity.Remark
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Path
import Scene.Check qualified as Check
import Scene.Parse.Core qualified as Parse

lint ::
  (J.HasParams p a1, J.HasTextDocument a1 a2, J.HasUri a2 Uri) =>
  p ->
  AppLsp () ()
lint msg = do
  let doc = msg ^. J.params . J.textDocument . J.uri
  case uriToFilePath doc of
    Just path -> do
      flushDiagnosticsBySource maxDiagNum (Just "neut")
      logList <- lift $ Check.check (Just path)
      let diagGroupList = mapMaybe remarkToDignostic logList
      let diagGroupList' = NE.groupBy (\(u1, _) (u2, _) -> u1 == u2) diagGroupList
      forM_ diagGroupList' $ \diagGroup -> do
        let (uri, _) = NE.head diagGroup
        let diags = map snd $ NE.toList diagGroup
        diags' <- lift $ updateCol uri diags
        publishDiagnostics maxDiagNum uri Nothing (partitionBySource diags')
    Nothing -> do
      return ()

maxDiagNum :: Int
maxDiagNum =
  100

remarkToDignostic :: Remark -> Maybe (NormalizedUri, Diagnostic)
remarkToDignostic (mLoc, _, level, msg) = do
  FP.FilePos fp (line, col) <- mLoc
  let pos = Position {_line = fromIntegral $ line - 1, _character = fromIntegral $ col - 1}
  let pos2 = Position {_line = fromIntegral $ line - 1, _character = fromIntegral $ col + 2}
  let range = Range {_start = pos, _end = pos2}
  let uri = Uri (T.pack fp)
  return
    ( toNormalizedUri uri,
      Diagnostic
        { _range = range,
          _severity = Just (levelToSeverity level),
          _code = Nothing,
          _source = Just "neut",
          _message = msg,
          _tags = Nothing,
          _relatedInformation = Nothing,
          _codeDescription = Nothing,
          _data_ = Nothing
        }
    )

updateCol :: NormalizedUri -> [Diagnostic] -> App [Diagnostic]
updateCol uri diags = do
  case uriToPath uri of
    Nothing -> do
      return []
    Just path -> do
      let diags' = sortOn (\diag -> diag ^. J.range . J.start) diags
      content <- liftIO $ fmap decodeUtf8 $ B.readFile $ toFilePath path
      return $ updateCol' (zip [0 ..] $ T.lines content) diags'

updateCol' :: [(Int, T.Text)] -> [Diagnostic] -> [Diagnostic]
updateCol' sourceLines diags =
  case (diags, sourceLines) of
    ([], _) ->
      []
    (_, []) -> do
      trace "shouldn't occur\n" []
    (diag : rest, (currentLineNumber, currentLine) : sourceLines') -> do
      let startPos@(Position l c) = diag ^. J.range . J.start
      if fromEnum l /= currentLineNumber
        then updateCol' sourceLines' diags
        else do
          let foo = T.drop (fromEnum c) currentLine
          let offset = T.length $ T.takeWhile (`S.notMember` Parse.nonSymbolCharSet) foo
          let endPos = Position l (fromIntegral $ fromEnum c + offset)
          let diag' = set J.range (Range startPos endPos) diag
          diag' : updateCol' sourceLines' rest

uriToPath :: NormalizedUri -> Maybe (Path Abs File)
uriToPath (NormalizedUri _ pathText) = do
  parseAbsFile $ T.unpack pathText

levelToSeverity :: RemarkLevel -> DiagnosticSeverity
levelToSeverity level =
  case level of
    Note ->
      DiagnosticSeverity_Information
    Warning ->
      DiagnosticSeverity_Warning
    Error ->
      DiagnosticSeverity_Error
    Critical ->
      DiagnosticSeverity_Error
    Pass ->
      DiagnosticSeverity_Information
    Fail ->
      DiagnosticSeverity_Error
