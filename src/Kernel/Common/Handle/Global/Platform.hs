module Kernel.Common.Handle.Global.Platform
  ( Handle (..),
    new,
    detectHost,
    ensureBuildable,
    getArch,
    getOS,
    getDataSize,
    getPlatform,
    getTargetPlatform,
    getForeign,
    getClang,
    getToolchainOption,
    getToolchainSearchPathOption,
    getSysrootOption,
    getClangTargetTriple,
    getToolchainDigest,
    getBaseBuildDir,
    getCurl,
    getCurlOptions,
  )
where

import App.App (App)
import App.Run (raiseError, raiseError')
import Control.Monad (filterM, mfilter, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Version qualified as V
import Kernel.Common.Arch qualified as Arch
import Kernel.Common.Const (envVarHome)
import Kernel.Common.Module
import Kernel.Common.OS qualified as O
import Kernel.Common.Platform qualified as P
import Kernel.Common.PlatformPattern qualified as PP
import Kernel.Common.RunProcess qualified as RunProcess
import Language.Common.DataSize qualified as DS
import Language.Common.Digest (hashAndEncode)
import Logger.Debug qualified as Logger
import Logger.Handle qualified as Logger
import Logger.Hint (Hint)
import Path
import Paths_neut
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath qualified as FP
import System.Process (CmdSpec (RawCommand))
import Text.ParserCombinators.ReadP (readP_to_S)

data Handle = Handle
  { _clangTargetTriple :: String,
    _toolchainDigest :: T.Text,
    _platform :: P.Platform,
    _patternHint :: Hint,
    _toolchainRoot :: FilePath,
    _clang :: String,
    _curl :: FilePath,
    _curlTrustOption :: [String]
  }

getArch :: Handle -> Arch.Arch
getArch h =
  P.getArch (getTargetPlatform h)

getOS :: Handle -> O.OS
getOS h =
  P.getOS (getTargetPlatform h)

getPlatform :: Handle -> P.Platform
getPlatform =
  _platform

getTargetPlatform :: Handle -> P.PlatformName
getTargetPlatform h =
  P.target (_platform h)

getForeign :: Handle -> Module -> Foreign
getForeign h =
  selectForeign (_platform h)

getDataSize :: Handle -> DS.DataSize
getDataSize h =
  Arch.dataSizeOf (getArch h)

getToolchainDigest :: Handle -> T.Text
getToolchainDigest =
  _toolchainDigest

getClangTargetTriple :: Handle -> String
getClangTargetTriple =
  _clangTargetTriple

detectHost :: App P.PlatformName
detectHost =
  either raiseError' return P.detectHost

new :: Logger.Handle -> P.PlatformName -> (Hint, PP.PlatformPattern) -> App Handle
new loggerHandle host (_patternHint, platformPattern) = do
  target <- resolveTarget _patternHint platformPattern host
  let _platform = P.Platform {host, target}
  _clangTargetTriple <- resolveClangTargetTriple target
  _toolchainRoot <- resolveToolchainRoot host
  let _clang = resolveClang _toolchainRoot
  ensureClang _clang
  let _curl = resolveCurl _toolchainRoot
  _curlTrustOption <- liftIO $ resolveCurlTrustOption (P.getOS host)
  _toolchainDigest <- calculateToolchainDigest loggerHandle _toolchainRoot
  return $ Handle {..}

resolveTarget :: Hint -> PP.PlatformPattern -> P.PlatformName -> App P.PlatformName
resolveTarget m platformPattern host =
  case PP.resolveTarget platformPattern host of
    Just target ->
      return target
    Nothing ->
      raiseError m $ "The platform pattern does not determine a platform on " <> P.reifyName host <> ": " <> PP.reify platformPattern

ensureBuildable :: Handle -> App ()
ensureBuildable h =
  unless (P.isBuildable (_platform h)) $
    raiseError (_patternHint h) $
      "Cannot build on this machine: " <> P.reifyPlatform (_platform h)

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

resolveToolchainRoot :: P.PlatformName -> App FilePath
resolveToolchainRoot host = do
  home <- resolveHome
  let name = T.unpack $ P.reifyName host
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

resolveClang :: FilePath -> String
resolveClang root =
  root `FP.combine` "bin" `FP.combine` "clang"

getCurl :: Handle -> FilePath
getCurl =
  _curl

getCurlOptions :: Handle -> [String]
getCurlOptions h =
  "-q" : _curlTrustOption h

resolveCurl :: FilePath -> FilePath
resolveCurl root =
  root `FP.combine` "bin" `FP.combine` "curl"

resolveCurlTrustOption :: O.OS -> IO [String]
resolveCurlTrustOption hostOS =
  case hostOS of
    O.Darwin ->
      return ["--ca-native", "--proxy-ca-native"]
    O.Linux -> do
      caBundle <- lookupNonEmptyEnv "CURL_CA_BUNDLE"
      certDir <- lookupNonEmptyEnv "SSL_CERT_DIR"
      certFile <- lookupNonEmptyEnv "SSL_CERT_FILE"
      case (caBundle, certDir, certFile) of
        (Just bundle, _, _) ->
          return $ makeCAFileOption bundle
        (Nothing, Nothing, Nothing) -> do
          bundles <- filterM doesFileExist linuxCABundleCandidates
          return $ concatMap makeCAFileOption $ take 1 bundles
        (Nothing, _, _) ->
          return $ concatMap makeCADirOption certDir ++ concatMap makeCAFileOption certFile
    O.Wasip1 ->
      return []

lookupNonEmptyEnv :: String -> IO (Maybe String)
lookupNonEmptyEnv name = do
  value <- lookupEnv name
  return $ mfilter (not . null) value

makeCAFileOption :: FilePath -> [String]
makeCAFileOption path =
  ["--cacert", path, "--proxy-cacert", path]

makeCADirOption :: FilePath -> [String]
makeCADirOption path =
  ["--capath", path, "--proxy-capath", path]

linuxCABundleCandidates :: [FilePath]
linuxCABundleCandidates =
  [ "/etc/ssl/certs/ca-certificates.crt",
    "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem",
    "/etc/ssl/ca-bundle.pem"
  ]

getToolchainOption :: Handle -> [String]
getToolchainOption h =
  getToolchainSearchPathOption h ++ ["-fuse-ld=lld"]

getToolchainSearchPathOption :: Handle -> [String]
getToolchainSearchPathOption h =
  ["-B" ++ (_toolchainRoot h `FP.combine` "bin")]

getSysrootOption :: Logger.Handle -> Handle -> IO [String]
getSysrootOption loggerHandle h =
  case getOS h of
    O.Wasip1 ->
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
      case T.lines (decodeUtf8With lenientDecode value) of
        [] ->
          return Nothing
        sdk : _ ->
          return $ Just $ T.unpack $ T.strip sdk

resolveClangTargetTriple :: P.PlatformName -> App String
resolveClangTargetTriple name = do
  case name of
    P.Amd64Linux ->
      return "x86_64-unknown-linux-gnu"
    P.Arm64Linux ->
      return "aarch64-unknown-linux-gnu"
    P.Arm64Darwin -> do
      deploymentTarget <- resolveMacOSDeploymentTarget
      return $ "arm64-apple-macosx" <> deploymentTarget
    P.Wasm32Wasip1 ->
      return "wasm32-unknown-wasip1"
    P.Wasm32Wasip1Web ->
      return "wasm32-unknown-wasip1"

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

calculateToolchainDigest :: Logger.Handle -> FilePath -> App T.Text
calculateToolchainDigest h toolchainRoot = do
  let stampPath = toolchainRoot `FP.combine` ".stamp"
  stampExists <- liftIO $ doesFileExist stampPath
  unless stampExists $
    raiseError' $ "The toolchain has no stamp: " <> T.pack stampPath
  stamp <- liftIO $ B.readFile stampPath
  liftIO $ Logger.report h $ "Toolchain stamp:\n" <> decodeUtf8With lenientDecode stamp
  return $ decodeUtf8 $ hashAndEncode stamp

getBaseBuildDir :: P.PlatformName -> Module -> App (Path Abs Dir)
getBaseBuildDir name baseModule = do
  platformPrefix <- parseRelDir $ T.unpack $ P.reifyName name
  versionDir <- parseRelDir $ "compiler-" ++ V.showVersion version
  let moduleRootDir = getModuleRootDir baseModule
  return $ moduleRootDir </> moduleCacheDir baseModule </> $(mkRelDir "build") </> platformPrefix </> versionDir

ensureClang :: FilePath -> App ()
ensureClang clang = do
  if FP.isAbsolute clang
    then do
      b <- liftIO $ doesFileExist clang
      unless b $
        raiseError' $
          "Command not found: " <> T.pack clang
    else ensureExecutable clang

ensureExecutable :: String -> App ()
ensureExecutable name = do
  mPath <- liftIO $ findExecutable name
  case mPath of
    Just _ ->
      return ()
    Nothing ->
      raiseError' $ "Command not found: " <> T.pack name
