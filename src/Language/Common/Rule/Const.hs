module Language.Common.Rule.Const
  ( nsSepChar,
    nsSep,
    holeVarPrefix,
    expVarPrefix,
  )
where

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
