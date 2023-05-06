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

envVarCoreModuleURL :: String
envVarCoreModuleURL =
  "NEUT_CORE_MODULE_URL"

envVarCoreModuleChecksum :: String
envVarCoreModuleChecksum =
  "NEUT_CORE_MODULE_CHECKSUM"

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

corePreface :: T.Text
corePreface =
  "core" <> nsSep <> "preface"

coreTop :: T.Text
coreTop =
  corePreface <> nsSep <> "top"

coreTopUnit :: T.Text
coreTopUnit =
  corePreface <> nsSep <> "Unit"

coreBool :: T.Text
coreBool =
  corePreface <> nsSep <> "bool"

coreBoolTrue :: T.Text
coreBoolTrue =
  corePreface <> nsSep <> "True"

coreBoolFalse :: T.Text
coreBoolFalse =
  corePreface <> nsSep <> "False"

coreOption :: T.Text
coreOption =
  corePreface <> nsSep <> "option"

coreOptionNone :: T.Text
coreOptionNone =
  corePreface <> nsSep <> "None"

coreOptionSome :: T.Text
coreOptionSome =
  corePreface <> nsSep <> "Some"

coreSum :: T.Text
coreSum =
  corePreface <> nsSep <> "sum"

coreSumLeft :: T.Text
coreSumLeft =
  corePreface <> nsSep <> "Left"

coreSumRight :: T.Text
coreSumRight =
  corePreface <> nsSep <> "Right"

coreList :: T.Text
coreList =
  corePreface <> nsSep <> "list"

coreListNil :: T.Text
coreListNil =
  corePreface <> nsSep <> "Nil"

coreListCons :: T.Text
coreListCons =
  corePreface <> nsSep <> "Cons"

coreText :: T.Text
coreText =
  corePreface <> nsSep <> "text"

coreSystemAdmit :: T.Text
coreSystemAdmit =
  corePreface <> nsSep <> "admit"

coreThreadFlowInner :: T.Text
coreThreadFlowInner =
  corePreface <> nsSep <> "flow-inner"

coreThreadDetach :: T.Text
coreThreadDetach =
  corePreface <> nsSep <> "detach"

coreThreadAttach :: T.Text
coreThreadAttach =
  corePreface <> nsSep <> "attach"

defaultImports :: [T.Text]
defaultImports =
  [corePreface]

holeVarPrefix :: T.Text
holeVarPrefix =
  "{}"
