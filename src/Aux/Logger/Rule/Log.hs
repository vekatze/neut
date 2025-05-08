module Aux.Logger.Rule.Log
  ( Log (..),
    ShouldInsertPadding,
    newLog,
    _logLevelToText,
    _logLevelToSGR,
  )
where

import Aux.Logger.Rule.Hint
import Aux.Logger.Rule.LogLevel
import Data.Binary (Binary)
import Data.Text qualified as T
import GHC.Generics (Generic)
import System.Console.ANSI

type ShouldInsertPadding =
  Bool

data Log = Log
  { position :: Maybe Hint,
    shouldInsertPadding :: ShouldInsertPadding,
    logLevel :: LogLevel,
    content :: T.Text
  }
  deriving (Show, Generic)

instance Binary Log

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
    { position = Just m,
      shouldInsertPadding = True,
      logLevel = level,
      content = text
    }
