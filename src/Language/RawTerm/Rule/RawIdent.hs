module Language.RawTerm.Rule.RawIdent
  ( RawIdent,
    Item,
    isHole,
  )
where

import Data.Text qualified as T
import Language.Common.Rule.Const (holeVarPrefix)

type RawIdent =
  T.Text

type Item a = (a, a)

isHole :: RawIdent -> Bool
isHole var =
  holeVarPrefix `T.isPrefixOf` var
