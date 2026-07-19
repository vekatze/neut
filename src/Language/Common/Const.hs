module Language.Common.Const (module Language.Common.Const) where

import Data.Text qualified as T

nsSepChar :: Char
nsSepChar =
  '.'

nsSep :: T.Text
nsSep =
  T.singleton nsSepChar

doubleColon :: T.Text
doubleColon =
  "::"

holeVarPrefix :: T.Text
holeVarPrefix =
  "{}"

expVarPrefix :: T.Text
expVarPrefix =
  "!"
