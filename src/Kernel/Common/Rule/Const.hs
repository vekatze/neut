module Kernel.Common.Rule.Const (module Kernel.Common.Rule.Const) where

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

signatureFile :: Path Rel File
signatureFile =
  $(mkRelFile "signature.ens")

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

executableRelDir :: Path Rel Dir
executableRelDir =
  $(mkRelDir "executable")

foreignRelDir :: Path Rel Dir
foreignRelDir =
  $(mkRelDir "foreign")

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

coreRune :: T.Text
coreRune =
  core <> nsSep <> "rune" <> nsSep <> "rune"

coreRuneRune :: T.Text
coreRuneRune =
  core <> nsSep <> "rune" <> nsSep <> "_Rune"

coreTrickPanic :: T.Text
coreTrickPanic =
  core <> nsSep <> "trick" <> nsSep <> "panic"

coreTrickAssert :: T.Text
coreTrickAssert =
  core <> nsSep <> "trick" <> nsSep <> "_assert"

coreThreadDetach :: T.Text
coreThreadDetach =
  core <> nsSep <> "thread" <> nsSep <> "detach"

coreThreadAttach :: T.Text
coreThreadAttach =
  core <> nsSep <> "thread" <> nsSep <> "attach"

coreCellMakeCell :: T.Text
coreCellMakeCell =
  core <> nsSep <> "cell" <> nsSep <> "_make-cell"

coreChannelMakeChannel :: T.Text
coreChannelMakeChannel =
  core <> nsSep <> "channel" <> nsSep <> "_make-channel"

coreBoxEmbody :: T.Text
coreBoxEmbody =
  core <> nsSep <> "box" <> nsSep <> "embody"

holeLiteral :: T.Text
holeLiteral =
  "_"

holeVarPrefix :: T.Text
holeVarPrefix =
  "{}"

expVarPrefix :: T.Text
expVarPrefix =
  "!"

unsafeArgcName :: T.Text
unsafeArgcName =
  "neut-unsafe-argc"

unsafeArgvName :: T.Text
unsafeArgvName =
  "neut-unsafe-argv"
