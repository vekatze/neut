module Entity.Log where

import Control.Exception.Safe
import Data.Text qualified as T
import Entity.FilePos
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
  (Maybe FilePos, LogLevel, T.Text)

type ColorFlag =
  Bool

-- fixme: ErrorSyntax, ErrorType, ...
newtype Error
  = MakeError [Log]
  deriving (Show)

instance Exception Error

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

forgetFilePosIfNecessary :: Bool -> Log -> Log
forgetFilePosIfNecessary _ (_, l, t) =
  (Nothing, l, t)

logNote :: FilePos -> T.Text -> Log
logNote pos text =
  (Just pos, Note, text)

logNote' :: T.Text -> Log
logNote' text =
  (Nothing, Note, text)

logWarning :: FilePos -> T.Text -> Log
logWarning pos text =
  (Just pos, Warning, text)

logError :: FilePos -> T.Text -> Log
logError pos text =
  (Just pos, Error, text)

logError' :: T.Text -> Log
logError' text =
  (Nothing, Error, text)

logCritical :: FilePos -> T.Text -> Log
logCritical pos text =
  (Just pos, Critical, text)

logCritical' :: T.Text -> Log
logCritical' text =
  (Nothing, Critical, text)
