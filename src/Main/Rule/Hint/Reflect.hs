module Main.Rule.Hint.Reflect (fromSourcePos) where

import Main.Rule.Hint
import Text.Megaparsec

fromSourcePos :: SourcePos -> Hint
fromSourcePos pos = do
  let line = unPos $ sourceLine pos
  let column = unPos $ sourceColumn pos
  let file = sourceName pos
  Main.Rule.Hint.newHint line column file
