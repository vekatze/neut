module Rule.Hint.Reflect (fromSourcePos) where

import Rule.Hint
import Text.Megaparsec

fromSourcePos :: SourcePos -> Hint
fromSourcePos pos = do
  let line = unPos $ sourceLine pos
  let column = unPos $ sourceColumn pos
  let file = sourceName pos
  Rule.Hint.newHint line column file
