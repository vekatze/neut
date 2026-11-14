module Kernel.Common.Platform
  ( PlatformName (..),
    Platform (..),
    reifyName,
    reflectName,
    reifyPlatform,
    allPlatforms,
    isBuildable,
    detectHost,
    getArch,
    getOS,
  )
where

import Data.List (find)
import Data.Text qualified as T
import Kernel.Common.Arch qualified as Arch
import Kernel.Common.OS qualified as OS
import System.Info qualified as SI

data PlatformName
  = Arm64Darwin
  | Amd64Linux
  | Arm64Linux
  | Wasm32Wasip1
  | Wasm32Wasip1Web
  deriving (Eq, Enum, Bounded)

data Platform = Platform
  { host :: PlatformName,
    target :: PlatformName
  }

reifyPlatform :: Platform -> T.Text
reifyPlatform platform =
  reifyName (host platform) <> "/" <> reifyName (target platform)

allPlatforms :: [Platform]
allPlatforms =
  [Platform {host, target} | host <- [minBound .. maxBound], target <- [minBound .. maxBound]]

reifyName :: PlatformName -> T.Text
reifyName name =
  case name of
    Arm64Darwin ->
      "arm64-darwin"
    Amd64Linux ->
      "amd64-linux"
    Arm64Linux ->
      "arm64-linux"
    Wasm32Wasip1 ->
      "wasm32-wasip1"
    Wasm32Wasip1Web ->
      "wasm32-wasip1-web"

reflectName :: T.Text -> Maybe PlatformName
reflectName text =
  find (\name -> reifyName name == text) [minBound .. maxBound]

getArch :: PlatformName -> Arch.Arch
getArch name =
  case name of
    Arm64Darwin ->
      Arch.Arm64
    Amd64Linux ->
      Arch.Amd64
    Arm64Linux ->
      Arch.Arm64
    Wasm32Wasip1 ->
      Arch.Wasm32
    Wasm32Wasip1Web ->
      Arch.Wasm32

getOS :: PlatformName -> OS.OS
getOS name =
  case name of
    Arm64Darwin ->
      OS.Darwin
    Amd64Linux ->
      OS.Linux
    Arm64Linux ->
      OS.Linux
    Wasm32Wasip1 ->
      OS.Wasip1
    Wasm32Wasip1Web ->
      OS.Wasip1

isNative :: PlatformName -> Bool
isNative name =
  getOS name /= OS.Wasip1

isBuildable :: Platform -> Bool
isBuildable platform =
  isNative (host platform) && (target platform == host platform || not (isNative (target platform)))

detectHost :: Either T.Text PlatformName
detectHost = do
  hostArch <-
    case SI.arch of
      "amd64" ->
        Right Arch.Amd64
      "x86_64" ->
        Right Arch.Amd64
      "arm64" ->
        Right Arch.Arm64
      "aarch64" ->
        Right Arch.Arm64
      unknownArch ->
        Left $ "Unknown architecture: " <> T.pack unknownArch
  hostOS <-
    case SI.os of
      "linux" ->
        Right OS.Linux
      "darwin" ->
        Right OS.Darwin
      unknownOS ->
        Left $ "Unknown OS: " <> T.pack unknownOS
  case find (\name -> getArch name == hostArch && getOS name == hostOS) [minBound .. maxBound] of
    Just name ->
      Right name
    Nothing ->
      Left $ "Unsupported platform: " <> Arch.reify hostArch <> "-" <> OS.reify hostOS
