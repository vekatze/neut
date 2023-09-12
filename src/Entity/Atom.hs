module Entity.Atom
  ( Atom (..),
    asAttrKey,
  )
where

import Data.Text qualified as T
import Entity.Const (attrPrefix)

data Atom
  = Symbol T.Text
  | String T.Text
  deriving (Show)

asAttrKey :: Atom -> Maybe T.Text
asAttrKey atom =
  case atom of
    Symbol sym ->
      T.stripPrefix attrPrefix sym
    _ ->
      Nothing
