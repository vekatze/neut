module Entity.RawPattern
  ( RawPattern (..),
    RawPatternRow,
    RawPatternMatrix,
    ConsArgs (..),
    new,
    unconsRow,
    toList,
  )
where

import Data.Vector qualified as V
import Entity.C
import Entity.Hint hiding (new)
import Entity.Key
import Entity.Name

data RawPattern
  = Var Name
  | Cons Name ConsArgs
  | ListIntro [(Hint, RawPattern)]
  deriving (Show)

data ConsArgs
  = Paren [(Hint, RawPattern)]
  | Of [(Key, (Hint, RawPattern))]
  deriving (Show)

type RawPatternRow a =
  (V.Vector ((Hint, RawPattern), C), C, a)

newtype RawPatternMatrix a
  = MakeRawPatternMatrix (V.Vector (C, RawPatternRow a))

new :: [(C, RawPatternRow a)] -> RawPatternMatrix a
new rows =
  MakeRawPatternMatrix $ V.fromList rows

unconsRow :: RawPatternMatrix a -> Maybe (RawPatternRow a, RawPatternMatrix a)
unconsRow (MakeRawPatternMatrix mat) = do
  ((_, headRow), rest) <- V.uncons mat
  return (headRow, MakeRawPatternMatrix rest)

toList :: RawPatternMatrix a -> [(C, RawPatternRow a)]
toList (MakeRawPatternMatrix mat) =
  V.toList mat
