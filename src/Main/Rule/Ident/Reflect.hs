module Main.Rule.Ident.Reflect (fromText) where

import Data.Text qualified as T
import Language.Common.Rule.Ident

fromText :: T.Text -> Ident
fromText s =
  I (s, 0)
