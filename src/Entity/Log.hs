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
  (Maybe FP.FilePos, LogLevel, T.Text)

type ColorFlag =
  Bool

-- fixme: ErrorSyntax, ErrorType, ...
newtype Error
  = MakeError [Log]
  deriving (Show)

instance Exception Error

fromHint :: LogLevel -> Hint -> T.Text -> Error
fromHint level m txt = do
  MakeError [(Just (FP.fromHint m), level, txt)]

newError :: Hint -> T.Text -> Error
newError m txt = do
  MakeError [(Just (FP.fromHint m), Error, txt)]

newError' :: T.Text -> Error
newError' txt = do
  MakeError [(Nothing, Error, txt)]

newCritical :: Hint -> T.Text -> Error
newCritical m txt = do
  MakeError [(Just (FP.fromHint m), Critical, txt)]

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

logNote :: FP.FilePos -> T.Text -> Log
logNote pos text =
  (Just pos, Note, text)

logNote' :: T.Text -> Log
logNote' text =
  (Nothing, Note, text)

logWarning :: FP.FilePos -> T.Text -> Log
logWarning pos text =
  (Just pos, Warning, text)

logError :: FP.FilePos -> T.Text -> Log
logError pos text =
  (Just pos, Error, text)

logError' :: T.Text -> Log
logError' text =
  (Nothing, Error, text)

logCritical :: FP.FilePos -> T.Text -> Log
logCritical pos text =
  (Just pos, Critical, text)

logCritical' :: T.Text -> Log
logCritical' text =
  (Nothing, Critical, text)
