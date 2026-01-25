module Language.RawTerm.RawPattern
  ( RawPattern (..),
    RawPatternRow,
    ConsArgs (..),
  )
where

import Language.Common.Rune qualified as RU
import Language.RawTerm.Key
import Language.RawTerm.Name
import Logger.Hint
import SyntaxTree.C
import SyntaxTree.Series qualified as SE
import Language.Common.VarKind

data RawPattern
  = Var VarKind Name
  | Cons Name C ConsArgs
  | RuneIntro RU.Rune

data ConsArgs
  = Paren (SE.Series (Hint, RawPattern))
  | Of (SE.Series (Key, (Hint, C, RawPattern)))

type RawPatternRow a =
  (SE.Series (Hint, RawPattern), C, a)
