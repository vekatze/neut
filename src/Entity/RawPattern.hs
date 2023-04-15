module Entity.RawPattern
  ( RawPattern (..),
    RawConsName (..),
    RawPatternRow,
    RawPatternMatrix,
    new,
    showRawConsName,
    consRow,
    unconsRow,
  )
where

import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Const
import Entity.DefiniteDescription qualified as DD
import Entity.GlobalLocator qualified as GL
import Entity.Hint hiding (new)
import Entity.Ident
import Entity.LocalLocator qualified as LL
import Entity.UnresolvedName qualified as UN

data RawPattern
  = Var Ident
  | NullaryCons GL.GlobalLocator LL.LocalLocator
  | Cons RawConsName [(Hint, RawPattern)]
  deriving (Show)

data RawConsName
  = UnresolvedName UN.UnresolvedName
  | LocatorPair GL.GlobalLocator LL.LocalLocator
  | DefiniteDescription DD.DefiniteDescription
  deriving (Show)

showRawConsName :: RawConsName -> T.Text
showRawConsName consName =
  case consName of
    UnresolvedName (UN.UnresolvedName value) ->
      value
    LocatorPair gl ll ->
      GL.reify gl <> nsSep <> LL.reify ll
    DefiniteDescription dd ->
      LL.reify (DD.localLocator dd)

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
