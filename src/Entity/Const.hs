{-# LANGUAGE TemplateHaskell #-}

module Entity.Const where

import Data.Text qualified as T
import Path

sourceFileExtension :: String
sourceFileExtension =
  ".nt"

packageFileExtension :: T.Text
packageFileExtension =
  ".tar.zst"

nsSepChar :: Char
nsSepChar =
  '.'

nsSep :: T.Text
nsSep =
  T.singleton nsSepChar

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
core =
  "core"

coreTop :: T.Text
coreTop =
  core <> nsSep <> "top" <> nsSep <> "top"

coreBool :: T.Text
coreBool =
  core <> nsSep <> "bool" <> nsSep <> "bool"

coreBoolTrue :: T.Text
coreBoolTrue =
  core <> nsSep <> "bool" <> nsSep <> "True"

coreBoolFalse :: T.Text
coreBoolFalse =
  core <> nsSep <> "bool" <> nsSep <> "False"

coreOption :: T.Text
coreOption =
  core <> nsSep <> "option" <> nsSep <> "option"

coreOptionNone :: T.Text
coreOptionNone =
  core <> nsSep <> "option" <> nsSep <> "None"

coreOptionSome :: T.Text
coreOptionSome =
  core <> nsSep <> "option" <> nsSep <> "Some"

coreSum :: T.Text
coreSum =
  core <> nsSep <> "sum" <> nsSep <> "sum"

coreSumLeft :: T.Text
coreSumLeft =
  core <> nsSep <> "sum" <> nsSep <> "Left"

coreSumRight :: T.Text
coreSumRight =
  core <> nsSep <> "sum" <> nsSep <> "Right"

coreList :: T.Text
coreList =
  core <> nsSep <> "list" <> nsSep <> "list"

coreListNil :: T.Text
coreListNil =
  core <> nsSep <> "list" <> nsSep <> "Nil"

coreListCons :: T.Text
coreListCons =
  core <> nsSep <> "list" <> nsSep <> "Cons"
