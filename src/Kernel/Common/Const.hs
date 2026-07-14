module Kernel.Common.Const (module Kernel.Common.Const) where

import Data.Text qualified as T
import Path

sourceFileExtension :: String
sourceFileExtension =
  ".nt"

packageFileExtension :: T.Text
packageFileExtension =
  ".tar.zst"

ensFileExtension :: String
ensFileExtension =
  ".ens"

nsSepChar :: Char
nsSepChar =
  '.'

nsSep :: T.Text
nsSep =
  T.singleton nsSepChar

doubleColon :: T.Text
doubleColon =
  "::"

verSep :: T.Text
verSep =
  "-"

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

cacheRelDir :: Path Rel Dir
cacheRelDir =
  $(mkRelDir "cache")

archiveRelDir :: Path Rel Dir
archiveRelDir =
  $(mkRelDir "archive")

artifactRelDir :: Path Rel Dir
artifactRelDir =
  $(mkRelDir "artifact")

entryRelDir :: Path Rel Dir
entryRelDir =
  $(mkRelDir "entry")

linkRelDir :: Path Rel Dir
linkRelDir =
  $(mkRelDir "link")

executableRelDir :: Path Rel Dir
executableRelDir =
  $(mkRelDir "executable")

foreignRelDir :: Path Rel Dir
foreignRelDir =
  $(mkRelDir "foreign")

allocatorRelDir :: Path Rel Dir
allocatorRelDir =
  $(mkRelDir "allocator")

ltoRelDir :: Path Rel Dir
ltoRelDir =
  $(mkRelDir "lto")

zenRelDir :: Path Rel Dir
zenRelDir =
  $(mkRelDir "zen")

defaultInlineLimit :: Int
defaultInlineLimit =
  1000000

core :: T.Text
core =
  "core"

coreUnit :: T.Text
coreUnit =
  core <> doubleColon <> "unit" <> doubleColon <> "unit"

coreUnitUnit :: T.Text
coreUnitUnit =
  core <> doubleColon <> "unit" <> doubleColon <> "Unit"

coreBool :: T.Text
coreBool =
  core <> doubleColon <> "bool" <> doubleColon <> "bool"

coreBoolTrue :: T.Text
coreBoolTrue =
  core <> doubleColon <> "bool" <> doubleColon <> "True"

coreBoolFalse :: T.Text
coreBoolFalse =
  core <> doubleColon <> "bool" <> doubleColon <> "False"

coreEither :: T.Text
coreEither =
  core <> doubleColon <> "either" <> doubleColon <> "either"

coreEitherLeft :: T.Text
coreEitherLeft =
  core <> doubleColon <> "either" <> doubleColon <> "Left"

coreEitherRight :: T.Text
coreEitherRight =
  core <> doubleColon <> "either" <> doubleColon <> "Right"

coreString :: T.Text
coreString =
  core <> doubleColon <> "string" <> doubleColon <> "string"

coreBinary :: T.Text
coreBinary =
  core <> doubleColon <> "binary" <> doubleColon <> "binary"

coreCSize :: T.Text
coreCSize =
  core <> doubleColon <> "c-size" <> doubleColon <> "c-size"

coreVector :: T.Text
coreVector =
  core <> doubleColon <> "vector"

coreDebugPanic :: T.Text
coreDebugPanic =
  core <> doubleColon <> "debug" <> doubleColon <> "panic"

coreLayerEmbody :: T.Text
coreLayerEmbody =
  core <> doubleColon <> "layer" <> doubleColon <> "embody"

coreTypeValueTypeValue :: T.Text
coreTypeValueTypeValue =
  core <> doubleColon <> "type-value" <> doubleColon <> "type-value"

holeLiteral :: T.Text
holeLiteral =
  "_"

unsafeArgcName :: T.Text
unsafeArgcName =
  "neut-unsafe-argc"

unsafeArgvName :: T.Text
unsafeArgvName =
  "neut-unsafe-argv"
