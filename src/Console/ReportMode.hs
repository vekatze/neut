module Console.ReportMode
  ( ReportMode (..),
    TraceConfig (..),
    TracePhase (..),
    emptyTraceConfig,
    parseTraceConfig,
  )
where

import Data.Text qualified as T

data ReportMode
  = NoReport
  | PlainReport
  | FancyReport
  | TraceReport TraceConfig
  deriving (Eq)

data TraceConfig = TraceConfig
  { tracePhaseList :: [TracePhase],
    tracePrefixList :: [T.Text],
    shouldReportActivity :: Bool
  }
  deriving (Eq)

data TracePhase
  = PreTermPhase
  | TermPhase
  | PreCompPhase
  | CompPhase
  | LowCompPhase
  | LLVMPhase
  deriving (Eq)

emptyTraceConfig :: TraceConfig
emptyTraceConfig =
  TraceConfig
    { tracePhaseList = [],
      tracePrefixList = [],
      shouldReportActivity = False
    }

parseTraceConfig :: T.Text -> Either T.Text TraceConfig
parseTraceConfig text = do
  itemList <- mapM parseItem $ T.splitOn "," text
  return $
    TraceConfig
      { tracePhaseList = [phase | TracePhaseItem phase <- itemList],
        tracePrefixList = [prefix | TracePrefixItem prefix <- itemList],
        shouldReportActivity = any isActivity itemList
      }

data TraceItem
  = TracePhaseItem TracePhase
  | TracePrefixItem T.Text
  | ActivityItem

parseItem :: T.Text -> Either T.Text TraceItem
parseItem text = do
  case T.stripPrefix "@" text of
    Nothing ->
      return $ TracePrefixItem text
    Just selector ->
      parseSelector selector

parseSelector :: T.Text -> Either T.Text TraceItem
parseSelector text = do
  case text of
    "activity" ->
      return ActivityItem
    "preterm" ->
      return $ TracePhaseItem PreTermPhase
    "term" ->
      return $ TracePhaseItem TermPhase
    "precomp" ->
      return $ TracePhaseItem PreCompPhase
    "comp" ->
      return $ TracePhaseItem CompPhase
    "lowcomp" ->
      return $ TracePhaseItem LowCompPhase
    "llvm" ->
      return $ TracePhaseItem LLVMPhase
    _ ->
      Left "TRACE selectors must be: @activity, @preterm, @term, @precomp, @comp, @lowcomp, or @llvm"

isActivity :: TraceItem -> Bool
isActivity item = do
  case item of
    ActivityItem -> True
    _ -> False
