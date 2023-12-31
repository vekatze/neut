module Entity.RawIdent
  ( RawIdent,
    Item,
    isHole,
  )
where

import Data.Text qualified as T
import Entity.Const (holeVarPrefix)

type RawIdent =
  T.Text

type Item a = (a, a)

isHole :: RawIdent -> Bool
isHole var =
  holeVarPrefix `T.isPrefixOf` var
