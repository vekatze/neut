module Language.Common.Rule.Const (module Language.Common.Rule.Const) where

import Data.Text qualified as T

nsSepChar :: Char
nsSepChar =
  '.'

nsSep :: T.Text
nsSep =
  T.singleton nsSepChar

holeVarPrefix :: T.Text
holeVarPrefix =
  "{}"

expVarPrefix :: T.Text
expVarPrefix =
  "!"
