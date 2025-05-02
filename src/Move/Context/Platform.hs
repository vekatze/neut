module Move.Context.Platform
  ( Handle,
    new,
    getArch,
    getDataSizeValue,
    getDataSize,
    getOS,
    getPlatform,
    getClang,
    getClangDigest,
    getBaseBuildDir,
    ensureExecutables,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Version qualified as V
import Move.Console.Report qualified as Report
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, raiseError, raiseError', run)
import Move.Context.External (ensureExecutable)
import Move.Context.ProcessRunner qualified as ProcessRunner
import Path
import Paths_neut
import Rule.Arch qualified as Arch
import Rule.Const (envVarClang)
import Rule.DataSize qualified as DS
import Rule.Digest (hashAndEncode)
import Rule.Hint
import Rule.Module
import Rule.OS qualified as O
import Rule.Platform qualified as P
import System.Environment (lookupEnv)
import System.Info qualified as SI
import System.Process (CmdSpec (RawCommand))

data Handle
  = Handle
  { arch :: Arch.Arch,
    os :: O.OS,
    clangDigest :: T.Text,
    baseSize :: DS.DataSize
  }

new :: Report.Handle -> Debug.Handle -> IO Handle
new reportHandle debugHandle = do
  run reportHandle $ do
    arch <- getArch' Nothing
    baseSize <- Arch.dataSizeOf <$> getArch' Nothing
    os <- getOS' Nothing
    clangDigest <- calculateClangDigest debugHandle
    return $ Handle {..}

getDataSizeValue :: Handle -> Int
getDataSizeValue h =
  DS.reify $ baseSize h

getArch :: Handle -> Arch.Arch
getArch =
  arch

getOS :: Handle -> O.OS
getOS =
  os

getPlatform :: Handle -> P.Platform
getPlatform h = do
  let arch = getArch h
  let os = getOS h
  P.Platform {arch, os}

getDataSize :: Handle -> DS.DataSize
getDataSize =
  baseSize

getArch' :: Maybe Hint -> EIO Arch.Arch
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

getOS' :: Maybe Hint -> EIO O.OS
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

getClangDigest :: Handle -> T.Text
getClangDigest = do
  clangDigest

getClang :: IO String
getClang = do
  mClang <- lookupEnv envVarClang
  case mClang of
    Just clang -> do
      return clang
    Nothing -> do
      return "clang"

calculateClangDigest :: Debug.Handle -> EIO T.Text
calculateClangDigest h = do
  clang <- liftIO getClang
  let spec = ProcessRunner.Spec {cmdspec = RawCommand clang ["--version"], cwd = Nothing}
  output <- liftIO $ ProcessRunner.run01 spec
  case output of
    Right value -> do
      Debug.report h $ "Clang info:\n" <> decodeUtf8 value
      return $ decodeUtf8 $ hashAndEncode value
    Left err ->
      throwError $ ProcessRunner.toCompilerError err

getPlatformPrefix :: Handle -> EIO (Path Rel Dir)
getPlatformPrefix h = do
  let p = getPlatform h
  parseRelDir $ T.unpack $ P.reify p

getBaseBuildDir :: Handle -> Module -> EIO (Path Abs Dir)
getBaseBuildDir h baseModule = do
  platformPrefix <- getPlatformPrefix h
  versionDir <- parseRelDir $ "compiler-" ++ V.showVersion version
  let moduleRootDir = getModuleRootDir baseModule
  return $ moduleRootDir </> moduleCacheDir baseModule </> $(mkRelDir "build") </> platformPrefix </> versionDir

ensureExecutables :: EIO ()
ensureExecutables = do
  clang <- liftIO getClang
  mapM_
    ensureExecutable
    [ clang,
      "curl",
      "tar",
      "zstd"
    ]
