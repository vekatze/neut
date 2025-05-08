module Aux.Logger.Rule.Hint.Reflect (fromSourcePos) where

import Aux.Logger.Rule.Hint
import Text.Megaparsec

fromSourcePos :: SourcePos -> Hint
fromSourcePos pos = do
  let line = unPos $ sourceLine pos
  let column = unPos $ sourceColumn pos
  let file = sourceName pos
  newHint line column file
