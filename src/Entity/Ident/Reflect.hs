module Entity.Ident.Reflect (fromText) where

import qualified Data.Text as T
import Entity.Ident

fromText :: T.Text -> Ident
fromText s =
  I (s, 0)
