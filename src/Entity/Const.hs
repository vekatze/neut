{-# LANGUAGE TemplateHaskell #-}

module Entity.Const where

import qualified Data.Text as T
import Entity.ModuleAlias
import Path

sourceFileExtension :: String
sourceFileExtension =
  ".nt"

-- sourceFileExtension :: T.Text
-- sourceFileExtension =
--   "nt"

nsSepChar :: Char
nsSepChar =
  '.'

nsSep :: T.Text
nsSep =
  T.singleton nsSepChar

definiteSep :: T.Text
definiteSep =
  "::"

unsafePtr :: T.Text
unsafePtr =
  "unsafe-pointer"

cartImmName :: T.Text
cartImmName =
  "imm"

cartClsName :: T.Text
cartClsName =
  "cls"

cartCellName :: T.Text
cartCellName =
  "cell"

moduleFile :: Path Rel File
moduleFile =
  $(mkRelFile "module.ens")

defaultModulePrefix :: ModuleAlias
defaultModulePrefix =
  ModuleAlias "this"

baseModulePrefix :: ModuleAlias
baseModulePrefix =
  ModuleAlias "base"

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
