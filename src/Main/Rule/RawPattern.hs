module Main.Rule.RawPattern
  ( RawPattern (..),
    RawPatternRow,
    ConsArgs (..),
  )
where

import Main.Rule.C
import Main.Rule.Hint
import Main.Rule.Key
import Main.Rule.Name
import Main.Rule.Rune qualified as RU
import Main.Rule.Syntax.Series qualified as SE

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
