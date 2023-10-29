module Entity.Name
  ( Name (..),
    showName,
    isConsName,
  )
where

import Data.Char
import Data.Text qualified as T
import Entity.DefiniteDescription qualified as DD
import Entity.Locator qualified as L
import Entity.RawIdent

data Name
  = Var RawIdent
  | Locator L.Locator
  | DefiniteDescription DD.DefiniteDescription
  deriving (Show)

showName :: Name -> T.Text
showName consName =
  case consName of
    Var value ->
      value
    Locator l ->
      L.reify l
    DefiniteDescription dd ->
      DD.reify dd

isConsName :: T.Text -> Bool
isConsName name = do
  case T.uncons (T.dropWhile (== '_') name) of
    Just (c, _)
      | isUpper c ->
          True
    _ ->
      False
