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

buildRelDir :: Path Rel Dir
buildRelDir =
  $(mkRelDir ".build")

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

coreTopUnit :: T.Text
coreTopUnit =
  core <> nsSep <> "top" <> nsSep <> "Unit"

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

coreText :: T.Text
coreText =
  core <> nsSep <> "text" <> nsSep <> "text"

coreSystemAdmit :: T.Text
coreSystemAdmit =
  core <> nsSep <> "system" <> nsSep <> "admit"

coreThreadFlowInner :: T.Text
coreThreadFlowInner =
  core <> nsSep <> "thread" <> nsSep <> "flow-inner"

coreThreadDetach :: T.Text
coreThreadDetach =
  core <> nsSep <> "thread" <> nsSep <> "detach"

coreThreadAttach :: T.Text
coreThreadAttach =
  core <> nsSep <> "thread" <> nsSep <> "attach"

defaultImports :: [T.Text]
defaultImports =
  [ "core.bool",
    "core.bottom",
    "core.i8-array",
    "core.list",
    "core.option",
    "core.sum",
    "core.text",
    "core.text.io",
    "core.top",
    "core.vector"
  ]

holeVarPrefix :: T.Text
holeVarPrefix =
  "{}"
