module Kernel.Common.Target
  ( Target (..),
    TargetSummary (..),
    MainTarget (..),
    emptyZen,
    getEntryPointName,
    getTargetConfig,
    getCompileOption,
    getLinkOption,
    getExecuteCommand,
    getTargetName,
  )
where

import Data.Text qualified as T
import Kernel.Common.ClangOption qualified as CL
import Kernel.Common.PlatformPattern (PlatformPattern)
import Kernel.Common.TargetConfig
import Language.Common.BaseName qualified as BN
import Language.Common.SourceLocator qualified as SL
import Logger.Hint (Hint)
import Path

data Target
  = Main MainTarget
  | Peripheral
  | PeripheralSingle (Path Abs File)
  deriving (Show, Eq)

data TargetSummary = TargetSummary
  { entryPoint :: SL.SourceLocator,
    targetPlatform :: (Hint, PlatformPattern),
    targetConfig :: TargetConfig
  }
  deriving (Show, Eq)

data MainTarget
  = Named T.Text TargetSummary
  | Zen (Path Abs File) TargetConfig
  deriving (Show, Eq)

emptyZen :: Path Abs File -> MainTarget
emptyZen path =
  Zen path emptyTargetConfig

getEntryPointName :: MainTarget -> BN.BaseName
getEntryPointName target =
  case target of
    Named {} ->
      BN.mainName
    Zen {} ->
      BN.zenName

getTargetConfig :: MainTarget -> TargetConfig
getTargetConfig target =
  case target of
    Named _ targetSummary ->
      targetConfig targetSummary
    Zen _ zenConfig ->
      zenConfig

getCompileOption :: Target -> [String]
getCompileOption target =
  case target of
    Peripheral {} ->
      []
    PeripheralSingle {} ->
      []
    Main mainTarget ->
      map T.unpack $ CL.compileOption $ clangOption $ getTargetConfig mainTarget

getLinkOption :: MainTarget -> [String]
getLinkOption =
  map T.unpack . CL.linkOption . clangOption . getTargetConfig

getTargetName :: MainTarget -> Maybe T.Text
getTargetName target =
  case target of
    Named name _ ->
      Just name
    Zen {} ->
      Nothing

getExecuteCommand :: MainTarget -> Maybe (Hint, T.Text)
getExecuteCommand =
  executeCommand . getTargetConfig
