module Entity.Ident.Reflect (fromText) where

import Data.Text qualified as T
import Entity.Ident

fromText :: T.Text -> Ident
fromText s =
  I (s, 0)
