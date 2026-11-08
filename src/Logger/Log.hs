module Logger.Log
  ( Log (..),
    isFailure,
    newLog,
    sortByPosition,
    _logLevelToText,
    _logLevelToSGR,
  )
where

import Binary.Derive (deriveBinary)
import Data.List qualified as List
import Data.Text qualified as T
import Logger.Hint
import Logger.LogLevel
import System.Console.ANSI

data Log = Log
  { position :: Maybe SavedHint,
    logLevel :: LogLevel,
    content :: T.Text
  }
  deriving (Show)

isFailure :: Log -> Bool
isFailure logItem = do
  case logLevel logItem of
    Error ->
      True
    Critical ->
      True
    _ ->
      False

_logLevelToText :: LogLevel -> T.Text
_logLevelToText level =
  case level of
    Note ->
      "Note"
    Warning ->
      "Warning"
    Error ->
      "Error"
    Critical ->
      "Critical"

_logLevelToSGR :: LogLevel -> [SGR]
_logLevelToSGR level =
  case level of
    Note ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
    Warning ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta]
    Error ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    Critical ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]

newLog :: Hint -> LogLevel -> T.Text -> Log
newLog m level text = do
  Log
    { position = Just (SavedHint m),
      logLevel = level,
      content = text
    }

sortByPosition :: [Log] -> [Log]
sortByPosition =
  List.sortOn logPositionKey

logPositionKey :: Log -> (Int, FilePath, Line, Column)
logPositionKey logItem = do
  case position logItem of
    Just (SavedHint Hint {metaFileName, metaLocation = (line, column)}) ->
      (1, metaFileName, line, column)
    Nothing ->
      (0, "", 0, 0)

deriveBinary [''Log]
