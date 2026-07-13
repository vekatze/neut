module Kernel.Common.Handle.Global.Platform
  ( Handle (..),
    new,
    getArch,
    getDataSize,
    getPlatform,
    getClang,
    getClangTargetTriple,
    getClangDigest,
    getBaseBuildDir,
    ensureExecutables,
  )
where

import App.App (App)
import App.Error (newError')
import App.Run (raiseError, raiseError', run)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Version qualified as V
import Kernel.Common.Arch qualified as Arch
import Kernel.Common.Const (envVarClang)
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
import System.Environment (lookupEnv)
import System.Info qualified as SI
import System.Process (CmdSpec (RawCommand))
import Text.ParserCombinators.ReadP (readP_to_S)

data Handle = Handle
  { _arch :: Arch.Arch,
    _os :: O.OS,
    _clangTargetTriple :: String,
    _clangDigest :: T.Text,
    _baseSize :: DS.DataSize
  }

getArch :: Handle -> Arch.Arch
getArch =
  _arch

getOS :: Handle -> O.OS
getOS =
  _os

getPlatform :: Handle -> P.Platform
getPlatform h = do
  let arch = getArch h
  let os = getOS h
  P.Platform {arch, os}

getDataSize :: Handle -> DS.DataSize
getDataSize =
  _baseSize

getClangDigest :: Handle -> T.Text
getClangDigest =
  _clangDigest

getClangTargetTriple :: Handle -> String
getClangTargetTriple =
  _clangTargetTriple

new :: Logger.Handle -> IO Handle
new loggerHandle = do
  run loggerHandle $ do
    _arch <- getArch' Nothing
    _os <- getOS' Nothing
    _clangTargetTriple <- resolveClangTargetTriple _arch _os
    let _baseSize = Arch.dataSizeOf _arch
    _clangDigest <- calculateClangDigest loggerHandle _clangTargetTriple
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

getClang :: IO String
getClang = do
  mClang <- lookupEnv envVarClang
  case mClang of
    Just clang -> do
      return clang
    Nothing -> do
      return "clang"

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

calculateClangDigest :: Logger.Handle -> String -> App T.Text
calculateClangDigest h targetTriple = do
  clang <- liftIO getClang
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
  clang <- liftIO getClang
  mapM_
    ensureExecutable
    [ clang,
      "curl",
      "tar",
      "zstd"
    ]

ensureExecutable :: String -> App ()
ensureExecutable name = do
  mPath <- liftIO $ findExecutable name
  case mPath of
    Just _ ->
      return ()
    Nothing ->
      raiseError' $ "Command not found: " <> T.pack name
