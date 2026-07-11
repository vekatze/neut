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

routeSep :: T.Text
routeSep =
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
  core <> routeSep <> "unit" <> nsSep <> "unit"

coreUnitUnit :: T.Text
coreUnitUnit =
  core <> routeSep <> "unit" <> nsSep <> "Unit"

coreBool :: T.Text
coreBool =
  core <> routeSep <> "bool" <> nsSep <> "bool"

coreBoolTrue :: T.Text
coreBoolTrue =
  core <> routeSep <> "bool" <> nsSep <> "True"

coreBoolFalse :: T.Text
coreBoolFalse =
  core <> routeSep <> "bool" <> nsSep <> "False"

coreEither :: T.Text
coreEither =
  core <> routeSep <> "either" <> nsSep <> "either"

coreEitherLeft :: T.Text
coreEitherLeft =
  core <> routeSep <> "either" <> nsSep <> "Left"

coreEitherRight :: T.Text
coreEitherRight =
  core <> routeSep <> "either" <> nsSep <> "Right"

coreString :: T.Text
coreString =
  core <> routeSep <> "string" <> nsSep <> "string"

coreBinary :: T.Text
coreBinary =
  core <> routeSep <> "binary" <> nsSep <> "binary"

coreCSize :: T.Text
coreCSize =
  core <> routeSep <> "c-size" <> nsSep <> "c-size"

coreVector :: T.Text
coreVector =
  core <> routeSep <> "vector"

coreDebugPanic :: T.Text
coreDebugPanic =
  core <> routeSep <> "debug" <> nsSep <> "panic"

coreLayerEmbody :: T.Text
coreLayerEmbody =
  core <> routeSep <> "layer" <> nsSep <> "embody"

coreTypeValueTypeValue :: T.Text
coreTypeValueTypeValue =
  core <> routeSep <> "type-value" <> nsSep <> "type-value"

holeLiteral :: T.Text
holeLiteral =
  "_"

unsafeArgcName :: T.Text
unsafeArgcName =
  "neut-unsafe-argc"

unsafeArgvName :: T.Text
unsafeArgvName =
  "neut-unsafe-argv"
