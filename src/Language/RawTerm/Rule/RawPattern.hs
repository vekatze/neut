module Language.RawTerm.Rule.RawPattern
  ( RawPattern (..),
    RawPatternRow,
    ConsArgs (..),
  )
where

import Language.RawTerm.Rule.C
import Language.Common.Rule.Hint
import Language.RawTerm.Rule.Key
import Language.RawTerm.Rule.Name
import Language.Common.Rule.Rune qualified as RU
import Language.RawTerm.Rule.Syntax.Series qualified as SE

data RawPattern
  = Var Name
  | Cons Name C ConsArgs
  | ListIntro (SE.Series (Hint, RawPattern))
  | RuneIntro RU.Rune

data ConsArgs
  = Paren (SE.Series (Hint, RawPattern))
  | Of (SE.Series (Key, (Hint, C, RawPattern)))

type RawPatternRow a =
  (SE.Series (Hint, RawPattern), C, a, Loc)
