module Entity.Const where

import qualified Data.Text as T

sourceFileExtension :: T.Text
sourceFileExtension =
  "neut"

nsSep :: T.Text
nsSep =
  "."

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
