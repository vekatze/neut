module Move.Scene.LSP.Util
  ( liftAppM,
    report,
    maxDiagNum,
    getUriParam,
  )
where

import Control.Lens hiding (Iso, List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Aeson.Types qualified as A
import Data.ByteString qualified as B
import Data.Function (on)
import Data.List (sortBy, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Move.Context.AppM
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Parse.Core qualified as Parse
import Path
import Rule.AppLsp
import Rule.FilePos qualified as FP
import Rule.Remark
import Rule.Remark qualified as R

liftAppM :: Gensym.Handle -> AppM a -> AppLsp b (Maybe a)
liftAppM h action = do
  resultOrRemarks <- lift $ runAppM h action
  case resultOrRemarks of
    Right result ->
      return $ Just result
    Left remarks -> do
      report remarks
      return Nothing

report :: [R.Remark] -> AppLsp b ()
report logList = do
  let uriDiagList = mapMaybe remarkToDignostic logList
  let diagGroupList' = NE.groupBy ((==) `on` fst) $ sortBy (compare `on` fst) uriDiagList
  let diagGroups = map (\g -> (fst $ NE.head g, NE.map snd g)) diagGroupList'
  forM_ diagGroups $ \(uri, diags) -> do
    diags' <- liftIO $ updateCol uri $ NE.toList diags
    publishDiagnostics maxDiagNum uri Nothing (partitionBySource diags')

maxDiagNum :: Int
maxDiagNum =
  100

remarkToDignostic :: Remark -> Maybe (NormalizedUri, Diagnostic)
remarkToDignostic (mLoc, _, level, msg) = do
  FP.FilePos path (line, col) <- mLoc
  let pos = Position {_line = fromIntegral $ line - 1, _character = fromIntegral $ col - 1}
  let range = Range {_start = pos, _end = pos}
  let uri = toNormalizedUri $ filePathToUri $ toFilePath path
  return
    ( uri,
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

updateCol :: NormalizedUri -> [Diagnostic] -> IO [Diagnostic]
updateCol uri diags = do
  case uriToPath uri of
    Nothing -> do
      return []
    Just path -> do
      let diags' = sortOn (\diag -> diag ^. J.range . J.start) diags
      content <- fmap decodeUtf8 $ B.readFile $ toFilePath path
      return $ updateCol' (zip [0 ..] $ T.lines content) diags'

updateCol' :: [(Int, T.Text)] -> [Diagnostic] -> [Diagnostic]
updateCol' sourceLines diags =
  case (diags, sourceLines) of
    ([], _) ->
      []
    (_, []) -> do
      diags
    (diag : rest, (currentLineNumber, currentLine) : sourceLines') -> do
      let startPos@(Position l c) = diag ^. J.range . J.start
      if fromEnum l /= currentLineNumber
        then updateCol' sourceLines' diags
        else do
          let currentLine' = T.drop (fromEnum c) currentLine
          let offset = T.length $ T.takeWhile (`S.notMember` Parse.nonSymbolCharSet) currentLine'
          let endPos = Position l (fromIntegral $ fromEnum c + offset)
          let diag' = set J.range (Range startPos endPos) diag
          diag' : updateCol' ((currentLineNumber, currentLine) : sourceLines') rest

uriToPath :: NormalizedUri -> Maybe (Path Abs File)
uriToPath uri = do
  uriToFilePath (fromNormalizedUri uri) >>= parseAbsFile

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

getUriParam :: [A.Value] -> Maybe Uri
getUriParam args =
  case args of
    [A.String uri] ->
      Just $ Uri uri
    _ ->
      Nothing
