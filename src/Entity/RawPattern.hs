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
import Entity.Syntax.Series qualified as SE

data RawPattern
  = Var Name
  | Cons Name C ConsArgs
  | ListIntro (SE.Series (Hint, RawPattern))

data ConsArgs
  = Paren (SE.Series (Hint, RawPattern))
  | Of (SE.Series (Key, (Hint, C, RawPattern)))

type RawPatternRow a =
  ([(C, ((Hint, RawPattern), C))], C, a)
