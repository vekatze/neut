module Main.Rule.Target
  ( Target (..),
    TargetSummary (..),
    MainTarget (..),
    emptyZen,
    getEntryPointName,
    getCompileOption,
    getLinkOption,
  )
where

import Data.Hashable
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.SourceLocator qualified as SL
import Main.Rule.ClangOption qualified as CL
import Path

data Target
  = Main MainTarget
  | Peripheral
  | PeripheralSingle (Path Abs File)
  deriving (Show, Eq, Generic)

data TargetSummary = TargetSummary
  { entryPoint :: SL.SourceLocator,
    clangOption :: CL.ClangOption
  }
  deriving (Show, Eq, Generic)

data MainTarget
  = Named T.Text TargetSummary
  | Zen (Path Abs File) CL.ClangOption
  deriving (Show, Eq, Generic)

instance Hashable Target

instance Hashable TargetSummary

instance Hashable MainTarget

emptyZen :: Path Abs File -> MainTarget
emptyZen path =
  Zen path $ CL.ClangOption {compileOption = [], linkOption = []}

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
        Zen _ clangOption ->
          map T.unpack $ CL.compileOption clangOption

getLinkOption :: Target -> [String]
getLinkOption target =
  case target of
    Peripheral {} ->
      []
    PeripheralSingle {} ->
      []
    Main c ->
      case c of
        Named _ targetSummary ->
          map T.unpack $ CL.linkOption (clangOption targetSummary)
        Zen _ clangOption ->
          map T.unpack $ CL.linkOption clangOption
