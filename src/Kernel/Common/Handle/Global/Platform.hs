module Kernel.Common.Handle.Global.Platform
  ( Handle (..),
    new,
    getArch,
    getDataSizeValue,
    getDataSize,
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
import Error.EIO (EIO)
import Error.Error (newError')
import Error.Run (raiseError, raiseError', run)
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

data Handle = Handle
  { _arch :: Arch.Arch,
    _os :: O.OS,
    _clangDigest :: T.Text,
    _baseSize :: DS.DataSize
  }

getDataSizeValue :: Handle -> Int
getDataSizeValue h =
  DS.reify $ _baseSize h

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

new :: Logger.Handle -> IO Handle
new loggerHandle = do
  run loggerHandle $ do
    _arch <- getArch' Nothing
    _baseSize <- Arch.dataSizeOf <$> getArch' Nothing
    _os <- getOS' Nothing
    _clangDigest <- calculateClangDigest loggerHandle
    return $ Handle {..}

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

getClang :: IO String
getClang = do
  mClang <- lookupEnv envVarClang
  case mClang of
    Just clang -> do
      return clang
    Nothing -> do
      return "clang"

calculateClangDigest :: Logger.Handle -> EIO T.Text
calculateClangDigest h = do
  clang <- liftIO getClang
  let spec = RunProcess.Spec {cmdspec = RawCommand clang ["--version"], cwd = Nothing}
  let h' = RunProcess.new h
  output <- liftIO $ RunProcess.run01 h' spec
  case output of
    Right value -> do
      liftIO $ Logger.report h $ "Clang info:\n" <> decodeUtf8 value
      return $ decodeUtf8 $ hashAndEncode value
    Left err ->
      throwError $ newError' err

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

ensureExecutable :: String -> EIO ()
ensureExecutable name = do
  mPath <- liftIO $ findExecutable name
  case mPath of
    Just _ ->
      return ()
    Nothing ->
      raiseError' $ "Command not found: " <> T.pack name
