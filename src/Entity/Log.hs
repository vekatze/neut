module Entity.Log where

import Control.Exception.Safe
import qualified Data.Text as T
import Entity.FilePos
import System.Console.ANSI

data LogLevel
  = LogLevelNote
  | LogLevelWarning
  | LogLevelError
  | LogLevelCritical -- "impossible" happened
  | LogLevelPass -- for test
  | LogLevelFail -- for test
  deriving (Show, Eq)

type Log =
  (Maybe FilePos, LogLevel, T.Text)

type ColorFlag =
  Bool

-- fixme: ErrorSyntax, ErrorType, ...
newtype Error
  = Error [Log]
  deriving (Show)

instance Exception Error

logLevelToText :: LogLevel -> T.Text
logLevelToText level =
  case level of
    LogLevelNote ->
      "note"
    LogLevelPass ->
      "pass"
    LogLevelWarning ->
      "warning"
    LogLevelError ->
      "error"
    LogLevelFail ->
      "fail"
    LogLevelCritical ->
      "critical"

logLevelToSGR :: LogLevel -> [SGR]
logLevelToSGR level =
  case level of
    LogLevelNote ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
    LogLevelPass ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
    LogLevelWarning ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
    LogLevelError ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    LogLevelFail ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    LogLevelCritical ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]

forgetFilePosIfNecessary :: Bool -> Log -> Log
forgetFilePosIfNecessary _ (_, l, t) =
  (Nothing, l, t)

logNote :: FilePos -> T.Text -> Log
logNote pos text =
  (Just pos, LogLevelNote, text)

logNote' :: T.Text -> Log
logNote' text =
  (Nothing, LogLevelNote, text)

logWarning :: FilePos -> T.Text -> Log
logWarning pos text =
  (Just pos, LogLevelWarning, text)

logError :: FilePos -> T.Text -> Log
logError pos text =
  (Just pos, LogLevelError, text)

logError' :: T.Text -> Log
logError' text =
  (Nothing, LogLevelError, text)

logCritical :: FilePos -> T.Text -> Log
logCritical pos text =
  (Just pos, LogLevelCritical, text)

logCritical' :: T.Text -> Log
logCritical' text =
  (Nothing, LogLevelCritical, text)
