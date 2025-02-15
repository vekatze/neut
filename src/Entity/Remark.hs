module Entity.Remark
  ( RemarkLevel (..),
    Remark,
    ShouldInsertPadding,
    newRemark,
    newRemark',
    remarkLevelToText,
    remarkLevelToSGR,
    attachSuffix,
    deactivatePadding,
  )
where

import Data.Binary
import Data.Text qualified as T
import Entity.FilePos qualified as FP
import Entity.Hint
import GHC.Generics (Generic)
import System.Console.ANSI

data RemarkLevel
  = Note
  | Warning
  | Error
  | Critical -- "impossible" happened
  | Pass -- for test
  | Fail -- for test
  deriving (Show, Eq, Generic)

instance Binary RemarkLevel

type Remark =
  (Maybe FP.FilePos, ShouldInsertPadding, RemarkLevel, T.Text)

type ShouldInsertPadding =
  Bool

newRemark :: Hint -> RemarkLevel -> T.Text -> Remark
newRemark m level text = do
  (FP.fromHint m, True, level, text)

newRemark' :: RemarkLevel -> T.Text -> Remark
newRemark' level text = do
  (Nothing, True, level, text)

deactivatePadding :: Remark -> Remark
deactivatePadding (mpos, _, level, text) =
  (mpos, False, level, text)

remarkLevelToText :: RemarkLevel -> T.Text
remarkLevelToText level =
  case level of
    Note ->
      "Note"
    Pass ->
      "Pass"
    Warning ->
      "Warning"
    Error ->
      "Error"
    Fail ->
      "Fail"
    Critical ->
      "Critical"

remarkLevelToSGR :: RemarkLevel -> [SGR]
remarkLevelToSGR level =
  case level of
    Note ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
    Pass ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
    Warning ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta]
    Error ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    Fail ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
    Critical ->
      [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]

attachSuffix :: Remark -> T.Text -> Remark
attachSuffix (mpos, b, level, text) suffix =
  (mpos, b, level, text <> suffix)
