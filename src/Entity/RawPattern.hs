module Entity.RawPattern
  ( RawPattern (..),
    RawPatternRow,
    RawPatternMatrix,
    new,
    consRow,
    unconsRow,
  )
where

import Data.Vector qualified as V
import Entity.Hint hiding (new)
import Entity.Name

data RawPattern
  = Var Name
  | Cons Name [(Hint, RawPattern)]
  deriving (Show)

type RawPatternRow a =
  (V.Vector (Hint, RawPattern), a)

newtype RawPatternMatrix a
  = MakeRawPatternMatrix (V.Vector (RawPatternRow a))

new :: [RawPatternRow a] -> RawPatternMatrix a
new rows =
  MakeRawPatternMatrix $ V.fromList rows

consRow :: RawPatternRow a -> RawPatternMatrix a -> RawPatternMatrix a
consRow row (MakeRawPatternMatrix mat) =
  MakeRawPatternMatrix $ V.cons row mat

unconsRow :: RawPatternMatrix a -> Maybe (RawPatternRow a, RawPatternMatrix a)
unconsRow (MakeRawPatternMatrix mat) = do
  (headRow, rest) <- V.uncons mat
  return (headRow, MakeRawPatternMatrix rest)
