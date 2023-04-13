{-# LANGUAGE TemplateHaskell #-}

module Entity.Const where

import Data.Text qualified as T
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

envVarClang :: String
envVarClang =
  "NEUT_CLANG"

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

core :: T.Text
core = "core"

coreTop :: T.Text
coreTop = core <> T.singleton nsSepChar <> "top" <> definiteSep <> "top"

coreBool :: T.Text
coreBool = core <> T.singleton nsSepChar <> "bool" <> definiteSep <> "bool"

coreBoolTrue :: T.Text
coreBoolTrue = core <> T.singleton nsSepChar <> "bool" <> definiteSep <> "bool" <> T.singleton nsSepChar <> "true"

coreBoolFalse :: T.Text
coreBoolFalse = core <> T.singleton nsSepChar <> "bool" <> definiteSep <> "bool" <> T.singleton nsSepChar <> "false"

coreSum :: T.Text
coreSum = core <> T.singleton nsSepChar <> "sum" <> definiteSep <> "sum"

coreSumLeft :: T.Text
coreSumLeft = core <> T.singleton nsSepChar <> "sum" <> definiteSep <> "sum" <> T.singleton nsSepChar <> "left"

coreSumRight :: T.Text
coreSumRight = core <> T.singleton nsSepChar <> "sum" <> definiteSep <> "sum" <> T.singleton nsSepChar <> "right"
