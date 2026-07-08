module Console.Handle
  ( Handle (..),
    ReportMode (..),
    stdoutSupportsANSI,
    stderrSupportsANSI,
    shouldColorizeStdout,
    shouldColorizeStderr,
    getReportMode,
    isDebugMode,
    supportsInteractiveOutput,
  )
where

import Console.ReportMode

data Handle = InternalHandle
  { _stdoutIsTTY :: Bool,
    _stderrIsTTY :: Bool,
    _termIsDumb :: Bool,
    _shouldColorizeStdoutByUser :: Bool,
    _shouldColorizeStderrByUser :: Bool,
    _reportModeByUser :: Maybe ReportMode
  }

stdoutSupportsANSI :: Handle -> Bool
stdoutSupportsANSI h = do
  _stdoutIsTTY h && not (_termIsDumb h)

stderrSupportsANSI :: Handle -> Bool
stderrSupportsANSI h = do
  _stderrIsTTY h && not (_termIsDumb h)

shouldColorizeStdout :: Handle -> Bool
shouldColorizeStdout h = do
  _shouldColorizeStdoutByUser h && stdoutSupportsANSI h

shouldColorizeStderr :: Handle -> Bool
shouldColorizeStderr h = do
  _shouldColorizeStderrByUser h && stderrSupportsANSI h

getReportMode :: Handle -> ReportMode
getReportMode h = do
  case _reportModeByUser h of
    Nothing -> do
      if supportsInteractiveOutput h
        then FancyReport
        else PlainReport
    Just mode ->
      mode

supportsInteractiveOutput :: Handle -> Bool
supportsInteractiveOutput h = do
  stdoutSupportsANSI h && stderrSupportsANSI h

isDebugMode :: Handle -> Bool
isDebugMode h =
  case getReportMode h of
    DebugReport ->
      True
    _ ->
      False
