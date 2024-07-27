module Entity.RawPattern
  ( RawPattern (..),
    RawPatternRow,
    ConsArgs (..),
  )
where

import Entity.C
import Entity.Hint hiding (new)
import Entity.Key
import Entity.Name
import Entity.Rune qualified as RU
import Entity.Syntax.Series qualified as SE

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
