module Entity.Hint.Reflect (fromSourcePos) where

import Entity.Hint
import Text.Megaparsec

fromSourcePos :: SourcePos -> Hint
fromSourcePos pos = do
  let line = unPos $ sourceLine pos
  let column = unPos $ sourceColumn pos
  let file = sourceName pos
  Entity.Hint.new line column file
