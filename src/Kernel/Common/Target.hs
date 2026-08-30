module Kernel.Common.Target
  ( Target (..),
    TargetSummary (..),
    MainTarget (..),
    emptyZen,
    getEntryPointName,
    getCompileOption,
    getLinkOption,
    getExecuteCommand,
  )
where

import Data.Hashable
import Data.Text qualified as T
import GHC.Generics (Generic)
import Kernel.Common.Allocator (Allocator, defaultAllocator)
import Kernel.Common.ClangOption qualified as CL
import Kernel.Common.Platform qualified as P
import Kernel.Common.ZenConfig (ZenConfig)
import Kernel.Common.ZenConfig qualified as Z
import Language.Common.BaseName qualified as BN
import Language.Common.SourceLocator qualified as SL
import Path

data Target
  = Main MainTarget
  | Peripheral
  | PeripheralSingle (Path Abs File)
  deriving (Show, Eq, Generic)

data TargetSummary = TargetSummary
  { entryPoint :: SL.SourceLocator,
    clangOption :: CL.ClangOption,
    allocator :: Allocator,
    platform :: P.PlatformSelector,
    executeCommand :: Maybe [T.Text]
  }
  deriving (Show, Eq, Generic)

data MainTarget
  = Named T.Text TargetSummary
  | Zen (Path Abs File) ZenConfig
  deriving (Show, Eq, Generic)

instance Hashable Target

instance Hashable TargetSummary

instance Hashable MainTarget

emptyZen :: Path Abs File -> MainTarget
emptyZen path =
  Zen path $ Z.ZenConfig {clangOption = CL.empty, allocator = defaultAllocator, platform = P.SelectHost, executeCommand = Nothing}

getEntryPointName :: MainTarget -> BN.BaseName
getEntryPointName target =
  case target of
    Named {} ->
      BN.mainName
    Zen {} ->
      BN.zenName

getCompileOption :: Target -> [String]
getCompileOption target =
  case target of
    Peripheral {} ->
      []
    PeripheralSingle {} ->
      []
    Main c ->
      case c of
        Named _ targetSummary -> do
          map T.unpack $ CL.compileOption (clangOption targetSummary)
        Zen _ zenConfig ->
          map T.unpack $ CL.compileOption (Z.clangOption zenConfig)

getLinkOption :: MainTarget -> [String]
getLinkOption target =
  case target of
    Named _ targetSummary ->
      map T.unpack $ CL.linkOption (clangOption targetSummary)
    Zen _ zenConfig ->
      map T.unpack $ CL.linkOption (Z.clangOption zenConfig)

getExecuteCommand :: MainTarget -> Maybe [T.Text]
getExecuteCommand target =
  case target of
    Named _ targetSummary ->
      executeCommand targetSummary
    Zen _ zenConfig ->
      Z.executeCommand zenConfig
