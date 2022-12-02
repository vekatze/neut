module Entity.RawPattern
  ( RawPattern (..),
    RawPatternRow,
    RawPatternMatrix,
    new,
    consRow,
    rowVars,
    unconsRow,
  )
where

import qualified Data.Vector as V
import qualified Entity.GlobalLocator as GL
import Entity.Hint hiding (new)
import Entity.Ident
import qualified Entity.LocalLocator as LL
import qualified Entity.UnresolvedName as UN

data RawPattern
  = Var Ident
  | Cons (Either UN.UnresolvedName (GL.GlobalLocator, LL.LocalLocator)) [(Hint, RawPattern)]
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

vars :: (Hint, RawPattern) -> [(Hint, Ident)]
vars (m, pat) =
  case pat of
    Var x ->
      [(m, x)]
    Cons _ pats ->
      concatMap vars pats

rowVars :: V.Vector (Hint, RawPattern) -> [(Hint, Ident)]
rowVars =
  concatMap vars
