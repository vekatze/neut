module Entity.Atom
  ( Atom (..),
    asAttrKey,
  )
where

import Data.Binary
import Data.Text qualified as T
import Entity.Const (attrPrefix)
import GHC.Generics (Generic)

data Atom
  = Symbol T.Text
  | String T.Text
  deriving (Show, Generic)

instance Binary Atom

asAttrKey :: Atom -> Maybe T.Text
asAttrKey atom =
  case atom of
    Symbol sym ->
      T.stripPrefix attrPrefix sym
    _ ->
      Nothing
