module Kernel.Elaborate.Trace (renderFailureTrace) where

import Control.Exception (IOException, SomeException, try)
import Data.HashMap.Strict qualified as Map
import Data.List (find, intercalate, isPrefixOf, nub)
import Data.Map.Strict qualified as OrdMap
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Module qualified as GlobalModule
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Module qualified as Module
import Kernel.Common.Source qualified as Source
import Kernel.Elaborate.Internal.Handle.Elaborate qualified as ElaborateHandle
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.Trace qualified as TermTrace
import Language.Term.TraceSites qualified as TraceSites
import Logger.Hint
import Path (Abs, File, Path, parseAbsFile, toFilePath)
import System.FilePath (addTrailingPathSeparator)

data FrameInfo = FrameInfo
  { frameInfoHint :: Hint,
    frameInfoTitle :: Title
  }

data Title
  = HeadTitle T.Text
  | FrameTitle T.Text T.Text
  | DefaultArgTitle

data RenderStep = RenderStep
  { renderStepHint :: Hint,
    renderStepTitle :: Title,
    renderStepAnnotation :: Maybe T.Text
  }

renderFailureTrace :: ElaborateHandle.Handle -> DD.DefiniteDescription -> Hint -> TraceSites.Blocker -> IO T.Text
renderFailureTrace h constantName bodyHint blocker = do
  blockerPath <- TermTrace.resolve (Global.termTraceHandle $ ElaborateHandle.globalHandle h) (TraceSites.blockerTraceID blocker)
  frameInfos <- collectFrameInfos h $ TermTrace.frames blockerPath
  let blockerHint = maybe bodyHint savedToHint $ TermTrace.sourceSite blockerPath
  let blockerLabel = TraceSites.describeTraceBlocker blocker
  let constantNameText = TraceSites.sourceLevelName constantName
  let steps = buildSteps constantNameText frameInfos blockerHint blockerLabel
  let moduleDir = T.pack $ Env.getMainModuleDir (ElaborateHandle.envHandle h)
  modulePathMap <- ModulePath.get $ Global.modulePathHandle (ElaborateHandle.globalHandle h)
  moduleCacheMap <- GlobalModule.getModuleCacheMap $ Global.moduleHandle (ElaborateHandle.globalHandle h)
  let mainModule = Module.extractModule $ Env.getMainModule $ ElaborateHandle.envHandle h
  let modules = mainModule : Map.elems moduleCacheMap
  renderSourceTrace moduleDir modulePathMap modules steps

buildSteps :: T.Text -> [FrameInfo] -> Hint -> T.Text -> [RenderStep]
buildSteps constantNameText frameInfos blockerHint blockerLabel =
  case frameInfos of
    [] ->
      [RenderStep blockerHint (HeadTitle constantNameText) (Just blockerLabel)]
    _ -> do
      let hints = map frameInfoHint frameInfos ++ [blockerHint]
      let titles = HeadTitle constantNameText : map frameInfoTitle frameInfos
      let annotations = replicate (length frameInfos) Nothing ++ [Just blockerLabel]
      zipWith3 RenderStep hints titles annotations

collectFrameInfos :: ElaborateHandle.Handle -> [TermTrace.Frame] -> IO [FrameInfo]
collectFrameInfos h frames = do
  expanded <- concat <$> mapM (expandConstantFrame h) (reverse frames)
  catMaybes <$> mapM (frameInfo h) (uniqueFrames expanded)

frameInfo :: ElaborateHandle.Handle -> TermTrace.Frame -> IO (Maybe FrameInfo)
frameInfo h frame = do
  callPath <- TermTrace.resolve (Global.termTraceHandle $ ElaborateHandle.globalHandle h) (TermTrace.callSite frame)
  return $ do
    hint <- savedToHint <$> TermTrace.sourceSite callPath
    return $
      FrameInfo
        { frameInfoHint = hint,
          frameInfoTitle = frameTitleOf frame
        }

frameTitleOf :: TermTrace.Frame -> Title
frameTitleOf frame =
  case TermTrace.frameKind frame of
    TermTrace.DefaultArgFrame ->
      DefaultArgTitle
    frameKind ->
      FrameTitle (frameKindText frameKind) (TraceSites.sourceLevelName (TermTrace.callee frame))

frameKindText :: TermTrace.FrameKind -> T.Text
frameKindText frameKind =
  case frameKind of
    TermTrace.NoInlineFrame ->
      "define"
    TermTrace.InlineFrame ->
      "inline"
    TermTrace.MacroFrame ->
      "define-meta"
    TermTrace.MacroInlineFrame ->
      "inline-meta"
    TermTrace.ConstantMetaFrame ->
      "constant-meta"
    TermTrace.DataIntroFrame ->
      "data-intro"
    TermTrace.DefaultArgFrame ->
      "default"

savedToHint :: SavedHint -> Hint
savedToHint (SavedHint sourceHint) =
  sourceHint

expandConstantFrame :: ElaborateHandle.Handle -> TermTrace.Frame -> IO [TermTrace.Frame]
expandConstantFrame h frame = do
  callPath <- TermTrace.resolve (Global.termTraceHandle $ ElaborateHandle.globalHandle h) (TermTrace.callSite frame)
  ancestors <- concat <$> mapM (expandConstantFrame h) (reverse $ TermTrace.frames callPath)
  return $ ancestors ++ [frame]

uniqueFrames :: [TermTrace.Frame] -> [TermTrace.Frame]
uniqueFrames frames = do
  case frames of
    [] ->
      []
    frame : rest ->
      frame : uniqueFrames (filter (/= frame) rest)

renderSourceTrace :: T.Text -> ModulePath.ModulePathMap -> [Module.Module] -> [RenderStep] -> IO T.Text
renderSourceTrace moduleDir modulePathMap modules steps = do
  let paths = nub $ map (metaFileName . renderStepHint) steps
  fileMap <- OrdMap.fromList <$> mapM (\path -> (,) path <$> readFileLines path) paths
  let lineWidth = maximum $ 1 : map (length . show . fst . metaLocation . renderStepHint) steps
  traceLines <- renderTraceSteps moduleDir modulePathMap modules fileMap lineWidth steps
  return $ T.intercalate "\n" traceLines

renderTraceSteps ::
  T.Text ->
  ModulePath.ModulePathMap ->
  [Module.Module] ->
  OrdMap.Map FilePath [T.Text] ->
  Int ->
  [RenderStep] ->
  IO [T.Text]
renderTraceSteps moduleDir modulePathMap modules fileMap lineWidth steps = do
  traceLines <- mapM (renderTraceStep moduleDir modulePathMap modules fileMap lineWidth) steps
  return $ intercalate [] traceLines

renderTraceStep ::
  T.Text ->
  ModulePath.ModulePathMap ->
  [Module.Module] ->
  OrdMap.Map FilePath [T.Text] ->
  Int ->
  RenderStep ->
  IO [T.Text]
renderTraceStep moduleDir modulePathMap modules fileMap lineWidth step = do
  let hint = renderStepHint step
  let path = metaFileName hint
  let (line, column) = metaLocation hint
  let sourceLine = fromMaybe "" $ lineAt (OrdMap.findWithDefault [] path fileMap) line
  let gutter = T.justifyRight lineWidth ' ' (T.pack (show line))
  let source = "    " <> gutter <> " | " <> sourceLine
  locator <- renderTraceLocation moduleDir modulePathMap modules hint
  let titleLine = renderTitle (renderStepTitle step) locator
  let caret = T.replicate (max 0 (column - 1)) " " <> "^" <> maybe "" (" " <>) (renderStepAnnotation step)
  let caretLine = T.replicate (lineWidth + 5) " " <> "| " <> caret
  return [titleLine, source, caretLine]

renderTitle :: Title -> T.Text -> T.Text
renderTitle title locator =
  case title of
    HeadTitle name ->
      "   constant " <> name <> " -- " <> locator
    FrameTitle kind name ->
      "=> " <> kind <> " " <> name <> " -- " <> locator
    DefaultArgTitle ->
      "=> (default argument -- " <> locator <> ")"

renderTraceLocation :: T.Text -> ModulePath.ModulePathMap -> [Module.Module] -> Hint -> IO T.Text
renderTraceLocation moduleDir modulePathMap modules hint = do
  let fallback = showFileRelative moduleDir hint
  let path = metaFileName hint
  parsedPath <- try (parseAbsFile path) :: IO (Either SomeException (Path Abs File))
  case parsedPath of
    Left _ ->
      return fallback
    Right sourcePath -> do
      case find (isSourceFileOf sourcePath) modules of
        Nothing ->
          return fallback
        Just sourceModule -> do
          let source =
                Source.Source
                  { Source.sourceFilePath = sourcePath,
                    Source.sourceModule = sourceModule,
                    Source.sourceHint = Nothing,
                    Source.sourceImportLocator = Nothing
                  }
          rendered <- try (ModulePath.renderCanonicalSource modulePathMap source) :: IO (Either SomeException T.Text)
          case rendered of
            Left _ ->
              return fallback
            Right locator ->
              return locator

showFileRelative :: T.Text -> Hint -> T.Text
showFileRelative moduleDir hint = do
  let filePath = T.pack $ metaFileName hint
  fromMaybe filePath (T.stripPrefix moduleDir filePath)

isSourceFileOf :: Path Abs File -> Module.Module -> Bool
isSourceFileOf sourcePath sourceModule = do
  let sourceDir = addTrailingPathSeparator $ toFilePath $ Module.getSourceDir sourceModule
  sourceDir `isPrefixOf` toFilePath sourcePath

lineAt :: [T.Text] -> Int -> Maybe T.Text
lineAt sourceLines n =
  if n >= 1 && n <= length sourceLines
    then Just (sourceLines !! (n - 1))
    else Nothing

readFileLines :: FilePath -> IO [T.Text]
readFileLines path = do
  result <- try (TIO.readFile path) :: IO (Either IOException T.Text)
  case result of
    Left _ ->
      return []
    Right content ->
      return $ T.lines content
