module Kernel.Common.Handle.Global.Platform
  ( Handle (..),
    new,
    getArch,
    getDataSize,
    getPlatform,
    getSelector,
    getPlatformText,
    getClang,
    getToolchainOption,
    getToolchainSearchPathOption,
    getSysrootOption,
    getClangTargetTriple,
    getClangDigest,
    getBaseBuildDir,
    ensureExecutables,
  )
where

import App.App (App)
import App.Error (newError')
import App.Run (raiseError, raiseError', run)
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Version qualified as V
import Kernel.Common.Arch qualified as Arch
import Kernel.Common.Const (envVarHome)
import Kernel.Common.Module
import Kernel.Common.OS qualified as O
import Kernel.Common.Platform qualified as P
import Kernel.Common.RunProcess qualified as RunProcess
import Language.Common.DataSize qualified as DS
import Language.Common.Digest (hashAndEncode)
import Logger.Debug qualified as Logger
import Logger.Handle qualified as Logger
import Logger.Hint
import Path
import Paths_neut
import System.Directory
import System.FilePath qualified as FP
import System.Environment (lookupEnv)
import System.Info qualified as SI
import System.Process (CmdSpec (RawCommand))
import Text.ParserCombinators.ReadP (readP_to_S)

data Handle = Handle
  { _arch :: Arch.Arch,
    _os :: O.OS,
    _clangTargetTriple :: String,
    _clangDigest :: T.Text,
    _baseSize :: DS.DataSize,
    _selector :: P.PlatformSelector,
    _toolchainRoot :: FilePath,
    _clang :: String
  }

getArch :: Handle -> Arch.Arch
getArch =
  _arch

getOS :: Handle -> O.OS
getOS =
  _os

getSelector :: Handle -> P.PlatformSelector
getSelector =
  _selector

getPlatform :: Handle -> P.Platform
getPlatform h = do
  let arch = getArch h
  let os = getOS h
  P.Platform {arch, os}

getPlatformText :: Handle -> T.Text
getPlatformText h =
  case _selector h of
    P.SelectHost ->
      P.reify (getPlatform h)
    selector ->
      P.reifySelector selector

getDataSize :: Handle -> DS.DataSize
getDataSize =
  _baseSize

getClangDigest :: Handle -> T.Text
getClangDigest =
  _clangDigest

getClangTargetTriple :: Handle -> String
getClangTargetTriple =
  _clangTargetTriple

new :: Logger.Handle -> P.PlatformSelector -> IO Handle
new loggerHandle selector = do
  run loggerHandle $ do
    let _selector = selector
    (_arch, _os) <-
      case selector of
        P.SelectHost -> do
          hostArch <- getArch' Nothing
          hostOS <- getOS' Nothing
          return (hostArch, hostOS)
        P.SelectWasm32 ->
          return (Arch.Wasm32, O.Wasi)
        P.SelectWeb ->
          return (Arch.Wasm32, O.Wasi)
    _clangTargetTriple <- resolveClangTargetTriple _arch _os
    let _baseSize = Arch.dataSizeOf _arch
    hostArch <- getArch' Nothing
    hostOS <- getOS' Nothing
    _toolchainRoot <- resolveToolchainRoot hostArch hostOS
    let _clang = resolveClang _toolchainRoot
    ensureClang _clang
    _clangDigest <- calculateClangDigest loggerHandle _clang _clangTargetTriple
    return $ Handle {..}

getArch' :: Maybe Hint -> App Arch.Arch
getArch' mm = do
  case SI.arch of
    "amd64" ->
      return Arch.Amd64
    "x86_64" ->
      return Arch.Amd64
    "arm64" ->
      return Arch.Arm64
    "aarch64" ->
      return Arch.Arm64
    arch ->
      case mm of
        Just m ->
          raiseError m $ "Unknown architecture: " <> T.pack arch
        Nothing ->
          raiseError' $ "Unknown architecture: " <> T.pack arch

getOS' :: Maybe Hint -> App O.OS
getOS' mm = do
  case SI.os of
    "linux" ->
      return O.Linux
    "darwin" ->
      return O.Darwin
    os ->
      case mm of
        Just m ->
          raiseError m $ "Unknown OS: " <> T.pack os
        Nothing ->
          raiseError' $ "Unknown OS: " <> T.pack os

getClang :: Handle -> String
getClang =
  _clang

resolveHome :: App FilePath
resolveHome = do
  mDir <- liftIO $ lookupEnv envVarHome
  case mDir >>= nonEmpty of
    Just dir -> do
      dirOrNone <- liftIO $ existingDir dir
      case dirOrNone of
        Just home ->
          return home
        Nothing ->
          raiseError' $
            T.pack envVarHome <> " names a directory that does not exist: " <> T.pack dir
    Nothing ->
      liftIO $ getXdgDirectory XdgData "neut"

resolveToolchainRoot :: Arch.Arch -> O.OS -> App FilePath
resolveToolchainRoot hostArch hostOS = do
  home <- resolveHome
  let name = platformName hostArch hostOS
  dirOrNone <- liftIO $ existingDir $ home `FP.combine` "toolchain" `FP.combine` name
  case dirOrNone of
    Just root ->
      return root
    Nothing ->
      raiseError' $ "No toolchain is installed for " <> T.pack name

nonEmpty :: FilePath -> Maybe FilePath
nonEmpty dir =
  if null dir then Nothing else Just dir

existingDir :: FilePath -> IO (Maybe FilePath)
existingDir dir = do
  b <- doesDirectoryExist dir
  return $ if b then Just dir else Nothing

platformName :: Arch.Arch -> O.OS -> FilePath
platformName arch os =
  T.unpack (Arch.reify arch) <> "-" <> T.unpack (O.reify os)

resolveClang :: FilePath -> String
resolveClang root =
  root `FP.combine` "bin" `FP.combine` "clang"

getToolchainOption :: Handle -> [String]
getToolchainOption h =
  getToolchainSearchPathOption h ++ ["-fuse-ld=lld"]

getToolchainSearchPathOption :: Handle -> [String]
getToolchainSearchPathOption h =
  ["-B" ++ (_toolchainRoot h `FP.combine` "bin")]

getSysrootOption :: Logger.Handle -> Handle -> IO [String]
getSysrootOption loggerHandle h =
  case P.os (getPlatform h) of
    O.Wasi ->
      return ["--sysroot=" ++ (_toolchainRoot h `FP.combine` "share" `FP.combine` "wasi-sysroot")]
    O.Darwin ->
      maybe [] (\sdk -> ["--sysroot=" ++ sdk]) <$> resolveMacOSSDK loggerHandle
    _ ->
      return []

resolveMacOSSDK :: Logger.Handle -> IO (Maybe String)
resolveMacOSSDK loggerHandle = do
  let spec = RunProcess.Spec {cmdspec = RawCommand "xcrun" ["--show-sdk-path"], cwd = Nothing}
  output <- RunProcess.run01 (RunProcess.new loggerHandle) spec
  case output of
    Left _ ->
      return Nothing
    Right value ->
      case T.lines (decodeUtf8 value) of
        [] ->
          return Nothing
        sdk : _ ->
          return $ Just $ T.unpack $ T.strip sdk

resolveClangTargetTriple :: Arch.Arch -> O.OS -> App String
resolveClangTargetTriple arch os = do
  case (arch, os) of
    (Arch.Amd64, O.Linux) ->
      return "x86_64-unknown-linux-gnu"
    (Arch.Arm64, O.Linux) ->
      return "aarch64-unknown-linux-gnu"
    (Arch.Arm64, O.Darwin) -> do
      deploymentTarget <- resolveMacOSDeploymentTarget
      return $ "arm64-apple-macosx" <> deploymentTarget
    (Arch.Wasm32, O.Wasi) ->
      return "wasm32-unknown-wasip1"
    _ -> do
      let p = P.Platform {P.arch = arch, P.os = os}
      raiseError' $ "Unsupported target platform: " <> P.reify p

resolveMacOSDeploymentTarget :: App String
resolveMacOSDeploymentTarget = do
  mDeploymentTarget <- liftIO $ lookupEnv "MACOSX_DEPLOYMENT_TARGET"
  case mDeploymentTarget of
    Nothing ->
      return "11.0.0"
    Just deploymentTarget ->
      normalizeMacOSDeploymentTarget deploymentTarget

normalizeMacOSDeploymentTarget :: String -> App String
normalizeMacOSDeploymentTarget deploymentTarget = do
  let parsedVersionList =
        [ parsedVersion
          | (parsedVersion, rest) <- readP_to_S V.parseVersion deploymentTarget,
            null rest
        ]
  case parsedVersionList of
    [parsedVersion] -> do
      let componentList = V.versionBranch parsedVersion
      case componentList of
        [major] ->
          validateMacOSDeploymentTarget deploymentTarget [major, 0, 0]
        [major, minor] ->
          validateMacOSDeploymentTarget deploymentTarget [major, minor, 0]
        [major, minor, patch] ->
          validateMacOSDeploymentTarget deploymentTarget [major, minor, patch]
        _ ->
          invalidMacOSDeploymentTarget deploymentTarget
    _ ->
      invalidMacOSDeploymentTarget deploymentTarget

validateMacOSDeploymentTarget :: String -> [Int] -> App String
validateMacOSDeploymentTarget original componentList = do
  if componentList < [11, 0, 0]
    then raiseError' $ "MACOSX_DEPLOYMENT_TARGET must be at least 11.0 for arm64: " <> T.pack original
    else return $ V.showVersion $ V.makeVersion componentList

invalidMacOSDeploymentTarget :: String -> App a
invalidMacOSDeploymentTarget deploymentTarget =
  raiseError' $ "Invalid MACOSX_DEPLOYMENT_TARGET: " <> T.pack deploymentTarget

calculateClangDigest :: Logger.Handle -> String -> String -> App T.Text
calculateClangDigest h clang targetTriple = do
  let spec = RunProcess.Spec {cmdspec = RawCommand clang ["--version"], cwd = Nothing}
  let h' = RunProcess.new h
  output <- liftIO $ RunProcess.run01 h' spec
  case output of
    Right value -> do
      liftIO $ Logger.report h $ "Clang info:\n" <> decodeUtf8 value
      let targetInfo = encodeUtf8 $ "\nTarget triple: " <> T.pack targetTriple
      return $ decodeUtf8 $ hashAndEncode $ value <> targetInfo
    Left err ->
      throwError $ newError' err

getPlatformPrefix :: Handle -> App (Path Rel Dir)
getPlatformPrefix h = do
  let p = getPlatform h
  parseRelDir $ T.unpack $ P.reify p

getBaseBuildDir :: Handle -> Module -> App (Path Abs Dir)
getBaseBuildDir h baseModule = do
  platformPrefix <- getPlatformPrefix h
  versionDir <- parseRelDir $ "compiler-" ++ V.showVersion version
  let moduleRootDir = getModuleRootDir baseModule
  return $ moduleRootDir </> moduleCacheDir baseModule </> $(mkRelDir "build") </> platformPrefix </> versionDir

ensureExecutables :: App ()
ensureExecutables = do
  mapM_
    ensureExecutable
    [ "curl",
      "tar",
      "zstd"
    ]

ensureClang :: FilePath -> App ()
ensureClang clang = do
  if FP.isAbsolute clang
    then do
      b <- liftIO $ doesFileExist clang
      unless b $
        raiseError' $
          "Command not found: " <> T.pack clang
    else
      ensureExecutable clang

ensureExecutable :: String -> App ()
ensureExecutable name = do
  mPath <- liftIO $ findExecutable name
  case mPath of
    Just _ ->
      return ()
    Nothing ->
      raiseError' $ "Command not found: " <> T.pack name
