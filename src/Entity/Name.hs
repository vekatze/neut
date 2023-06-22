module Entity.Name
  ( Name (..),
    showName,
  )
where

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
