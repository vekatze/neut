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

data RawPattern
  = Var Name
  | Cons Name C ConsArgs
  | ListIntro [(C, ((Hint, RawPattern), C))]
  deriving (Show)

data ConsArgs
  = Paren [(C, ((Hint, RawPattern), C))]
  | Of C C [(C, (Key, ((Hint, RawPattern), C)))]
  deriving (Show)

type RawPatternRow a =
  ([(C, ((Hint, RawPattern), C))], C, a)
