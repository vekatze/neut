module Logger.Rule.Log
  ( Log (..),
    ShouldInsertPadding,
    _logLevelToText,
    _logLevelToSGR,
  )
where

import Data.Binary (Binary)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Logger.Rule.FilePos qualified as FP
import Logger.Rule.LogLevel
import System.Console.ANSI

type ShouldInsertPadding =
  Bool

data Log = Log
  { position :: Maybe FP.FilePos,
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
