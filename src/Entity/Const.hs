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

verSep :: T.Text
verSep =
  "-"

envVarCacheDir :: String
envVarCacheDir =
  "NEUT_CACHE_DIR"

envVarCoreModuleURL :: String
envVarCoreModuleURL =
  "NEUT_CORE_MODULE_URL"

envVarCoreModuleDigest :: String
envVarCoreModuleDigest =
  "NEUT_CORE_MODULE_DIGEST"

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

coreUnit :: T.Text
coreUnit =
  core <> nsSep <> "unit" <> nsSep <> "unit"

coreUnitUnit :: T.Text
coreUnitUnit =
  core <> nsSep <> "unit" <> nsSep <> "Unit"

coreBool :: T.Text
coreBool =
  core <> nsSep <> "bool" <> nsSep <> "bool"

coreBoolTrue :: T.Text
coreBoolTrue =
  core <> nsSep <> "bool" <> nsSep <> "True"

coreBoolFalse :: T.Text
coreBoolFalse =
  core <> nsSep <> "bool" <> nsSep <> "False"

coreEither :: T.Text
coreEither =
  core <> nsSep <> "either" <> nsSep <> "either"

coreEitherLeft :: T.Text
coreEitherLeft =
  core <> nsSep <> "either" <> nsSep <> "Left"

coreEitherRight :: T.Text
coreEitherRight =
  core <> nsSep <> "either" <> nsSep <> "Right"

coreEitherOption :: T.Text
coreEitherOption =
  core <> nsSep <> "either" <> nsSep <> "option"

coreEitherNoneInternal :: T.Text
coreEitherNoneInternal =
  core <> nsSep <> "either" <> nsSep <> "none-internal"

coreEitherSomeInternal :: T.Text
coreEitherSomeInternal =
  core <> nsSep <> "either" <> nsSep <> "some-internal"

coreBoth :: T.Text
coreBoth =
  core <> nsSep <> "both" <> nsSep <> "both"

coreBothBoth :: T.Text
coreBothBoth =
  core <> nsSep <> "both" <> nsSep <> "Both"

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

coreSystemAssert :: T.Text
coreSystemAssert =
  core <> nsSep <> "system" <> nsSep <> "assert"

coreThreadFlowInner :: T.Text
coreThreadFlowInner =
  core <> nsSep <> "thread" <> nsSep <> "flow-inner"

coreThreadDetach :: T.Text
coreThreadDetach =
  core <> nsSep <> "thread" <> nsSep <> "detach"

coreThreadAttach :: T.Text
coreThreadAttach =
  core <> nsSep <> "thread" <> nsSep <> "attach"

holeVarPrefix :: T.Text
holeVarPrefix =
  "{}"

unsafeArgcName :: T.Text
unsafeArgcName =
  "neut-unsafe-argc"

unsafeArgvName :: T.Text
unsafeArgvName =
  "neut-unsafe-argv"
