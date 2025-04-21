module Rule.Ident.Reflect (fromText) where

import Data.Text qualified as T
import Rule.Ident

fromText :: T.Text -> Ident
fromText s =
  I (s, 0)
