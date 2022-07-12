module Entity.Const where

import qualified Data.Text as T

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
