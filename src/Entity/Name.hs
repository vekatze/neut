module Entity.Name
  ( Name (..),
    showName,
  )
where

import Data.Text qualified as T
import Entity.Locator qualified as L
import Entity.RawIdent

data Name
  = Var RawIdent
  | Locator L.Locator
  deriving (Show)

showName :: Name -> T.Text
showName consName =
  case consName of
    Var value ->
      value
    Locator l ->
      L.reify l
