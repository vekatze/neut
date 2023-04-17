module Entity.Log where

import Control.Exception
import Data.Text qualified as T
import Entity.FilePos qualified as FP
import Entity.Hint
import System.Console.ANSI

data LogLevel
  = Note
  | Warning
  | Error
  | Critical -- "impossible" happened
  | Pass -- for test
  | Fail -- for test
  deriving (Show, Eq)

type Log =
  (Maybe FP.FilePos, ShouldInsertPadding, LogLevel, T.Text)

type ShouldInsertPadding =
  Bool

type ColorFlag =
  Bool

-- fixme: ErrorSyntax, ErrorType, ...
newtype Error
  = MakeError [Log]
  deriving (Show)

instance Exception Error

fromHint :: LogLevel -> Hint -> T.Text -> Error
fromHint level m text = do
  MakeError [(Just (FP.fromHint m), True, level, text)]

newLog :: Maybe FP.FilePos -> LogLevel -> T.Text -> Log
newLog pos level text = do
  (pos, True, level, text)

deactivatePadding :: Log -> Log
deactivatePadding (mpos, _, level, text) =
  (mpos, False, level, text)

newError :: Hint -> T.Text -> Error
newError m text = do
  MakeError [newLog (Just (FP.fromHint m)) Error text]

newError' :: T.Text -> Error
newError' text = do
  MakeError [newLog Nothing Error text]

newCritical :: Hint -> T.Text -> Error
newCritical m text = do
  MakeError [newLog (Just (FP.fromHint m)) Critical text]

logLevelToText :: LogLevel -> T.Text
logLevelToText level =
  case level of
    Note ->
      "note"
    Pass ->
      "pass"
    Warning ->
      "warning"
    Error ->
      "error"
    Fail ->
      "fail"
    Critical ->
      "critical"

logLevelToSGR :: LogLevel -> [SGR]
logLevelToSGR level =
  case level of
    Note ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
    Pass ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
    Warning ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    Error ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    Fail ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    Critical ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]

logNote :: FP.FilePos -> T.Text -> Log
logNote pos =
  newLog (Just pos) Note

logNote' :: T.Text -> Log
logNote' =
  newLog Nothing Note

logWarning :: FP.FilePos -> T.Text -> Log
logWarning pos =
  newLog (Just pos) Warning

logError :: FP.FilePos -> T.Text -> Log
logError pos =
  newLog (Just pos) Error

logError' :: T.Text -> Log
logError' =
  newLog Nothing Error

logCritical :: FP.FilePos -> T.Text -> Log
logCritical pos =
  newLog (Just pos) Critical

logCritical' :: T.Text -> Log
logCritical' =
  newLog Nothing Critical
