{-# LANGUAGE TemplateHaskell #-}

module Entity.Const where

import qualified Data.Text as T
import Path

sourceFileExtension :: String
sourceFileExtension =
  ".nt"

nsSepChar :: Char
nsSepChar =
  '.'

nsSep :: T.Text
nsSep =
  T.singleton nsSepChar

definiteSep :: T.Text
definiteSep =
  "::"

envVarCacheDir :: String
envVarCacheDir =
  "NEUT_CACHE_DIR"

envVarTargetOS :: String
envVarTargetOS =
  "NEUT_TARGET_OS"

envVarTargetArch :: String
envVarTargetArch =
  "NEUT_TARGET_ARCH"

moduleFile :: Path Rel File
moduleFile =
  $(mkRelFile "module.ens")

sourceRelDir :: Path Rel Dir
sourceRelDir =
  $(mkRelDir "source")

targetRelDir :: Path Rel Dir
targetRelDir =
  $(mkRelDir "target")

releaseRelDir :: Path Rel Dir
releaseRelDir =
  $(mkRelDir "release")

artifactRelDir :: Path Rel Dir
artifactRelDir =
  $(mkRelDir "artifact")

executableRelDir :: Path Rel Dir
executableRelDir =
  $(mkRelDir "executable")
