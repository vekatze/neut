module Entity.Target where

import Data.Hashable
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.ClangOption qualified as CL
import Entity.SourceLocator qualified as SL
import GHC.Generics (Generic)
import Path

data Target
  = Abstract AbstractTarget
  | Concrete ConcreteTarget
  deriving (Show, Eq, Generic)

data TargetSummary = TargetSummary
  { entryPoint :: SL.SourceLocator,
    clangOption :: CL.ClangOption
  }
  deriving (Show, Eq, Generic)

data AbstractTarget
  = Foundation
  deriving (Show, Eq, Generic)

data ConcreteTarget
  = Named T.Text TargetSummary
  | Zen (Path Abs File) T.Text T.Text
  deriving (Show, Eq, Generic)

instance Hashable Target

instance Hashable AbstractTarget

instance Hashable TargetSummary

instance Hashable ConcreteTarget

emptyZen :: Path Abs File -> ConcreteTarget
emptyZen path =
  Zen path "" ""

getEntryPointName :: ConcreteTarget -> BN.BaseName
getEntryPointName target =
  case target of
    Named {} ->
      BN.mainName
    Zen {} ->
      BN.zenName

getCompileOption :: Target -> [String]
getCompileOption target =
  case target of
    Abstract {} ->
      []
    Concrete c ->
      case c of
        Named _ targetSummary -> do
          map T.unpack $ CL.compileOption (clangOption targetSummary)
        Zen _ compileOption _ ->
          [T.unpack compileOption]

getLinkOption :: Target -> [String]
getLinkOption target =
  case target of
    Abstract {} ->
      []
    Concrete c ->
      case c of
        Named _ targetSummary ->
          map T.unpack $ CL.linkOption (clangOption targetSummary)
        Zen _ _ linkOption ->
          [T.unpack linkOption]
