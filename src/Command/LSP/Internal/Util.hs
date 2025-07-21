module Command.LSP.Internal.Util
  ( run,
    report,
    maxDiagNum,
    getUriParam,
    Lsp,
  )
where

import App.App (App)
import App.Error qualified as E
import App.Run (runApp)
import CodeParser.Parser (nonSymbolCharSet)
import Control.Lens hiding (Iso, List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson.Types qualified as A
import Data.Function (on)
import Data.List (sortBy, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.GlobalRemark qualified as GlobalRemark
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Logger.Hint
import Logger.Log
import Logger.Log qualified as L
import Logger.LogLevel
import Path
import Path.Read (readTextFromPath)

type Lsp config =
  LspT config IO

run :: Global.Handle -> App a -> Lsp b (Maybe a)
run h comp = do
  resultOrErr <- liftIO $ runApp comp
  remarkList <- liftIO $ GlobalRemark.get (Global.globalRemarkHandle h)
  case resultOrErr of
    Left (E.MakeError logList) -> do
      report $ logList ++ remarkList
      return Nothing
    Right result ->
      return $ Just result

report :: [L.Log] -> Lsp b ()
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

remarkToDignostic :: Log -> Maybe (NormalizedUri, Diagnostic)
remarkToDignostic Log {position, logLevel, content} = do
  SavedHint (Hint {metaFileName = path, metaLocation = (line, col)}) <- position
  let pos = Position {_line = fromIntegral $ line - 1, _character = fromIntegral $ col - 1}
  let range = Range {_start = pos, _end = pos}
  let uri = toNormalizedUri $ filePathToUri path
  return
    ( uri,
      Diagnostic
        { _range = range,
          _severity = Just (levelToSeverity logLevel),
          _code = Nothing,
          _source = Just "neut",
          _message = content,
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
      contentOrError <- runExceptT $ readTextFromPath path
      case contentOrError of
        Left _ ->
          return []
        Right content ->
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
          let offset = T.length $ T.takeWhile (`S.notMember` nonSymbolCharSet) currentLine'
          let endPos = Position l (fromIntegral $ fromEnum c + offset)
          let diag' = set J.range (Range startPos endPos) diag
          diag' : updateCol' ((currentLineNumber, currentLine) : sourceLines') rest

uriToPath :: NormalizedUri -> Maybe (Path Abs File)
uriToPath uri = do
  uriToFilePath (fromNormalizedUri uri) >>= parseAbsFile

levelToSeverity :: LogLevel -> DiagnosticSeverity
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

getUriParam :: [A.Value] -> Maybe Uri
getUriParam args =
  case args of
    [A.String uri] ->
      Just $ Uri uri
    _ ->
      Nothing
