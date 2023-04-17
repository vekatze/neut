module Entity.Remark where

import Control.Exception
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

type ColorFlag =
  Bool

-- fixme: ErrorSyntax, ErrorType, ...
newtype Error
  = MakeError [Remark]
  deriving (Show)

instance Exception Error

fromHint :: RemarkLevel -> Hint -> T.Text -> Error
fromHint level m text = do
  MakeError [(Just (FP.fromHint m), True, level, text)]

newRemark :: Maybe FP.FilePos -> RemarkLevel -> T.Text -> Remark
newRemark pos level text = do
  (pos, True, level, text)

deactivatePadding :: Remark -> Remark
deactivatePadding (mpos, _, level, text) =
  (mpos, False, level, text)

newError :: Hint -> T.Text -> Error
newError m text = do
  MakeError [newRemark (Just (FP.fromHint m)) Error text]

newError' :: T.Text -> Error
newError' text = do
  MakeError [newRemark Nothing Error text]

newCritical :: Hint -> T.Text -> Error
newCritical m text = do
  MakeError [newRemark (Just (FP.fromHint m)) Critical text]

remarkLevelToText :: RemarkLevel -> T.Text
remarkLevelToText level =
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

remarkNote :: FP.FilePos -> T.Text -> Remark
remarkNote pos =
  newRemark (Just pos) Note

remarkNote' :: T.Text -> Remark
remarkNote' =
  newRemark Nothing Note

remarkWarning :: FP.FilePos -> T.Text -> Remark
remarkWarning pos =
  newRemark (Just pos) Warning

remarkError :: FP.FilePos -> T.Text -> Remark
remarkError pos =
  newRemark (Just pos) Error

remarkError' :: T.Text -> Remark
remarkError' =
  newRemark Nothing Error

remarkCritical :: FP.FilePos -> T.Text -> Remark
remarkCritical pos =
  newRemark (Just pos) Critical

remarkCritical' :: T.Text -> Remark
remarkCritical' =
  newRemark Nothing Critical
