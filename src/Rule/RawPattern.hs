module Rule.RawPattern
  ( RawPattern (..),
    RawPatternRow,
    ConsArgs (..),
  )
where

import Rule.C
import Rule.Hint hiding (new)
import Rule.Key
import Rule.Name
import Rule.Rune qualified as RU
import Rule.Syntax.Series qualified as SE

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
