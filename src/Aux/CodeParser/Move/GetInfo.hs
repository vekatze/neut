module Aux.CodeParser.Move.GetInfo
  ( getCurrentHint,
    getCurrentLoc,
  )
where

import Aux.CodeParser.Rule.Parser
import Aux.Logger.Rule.Hint
import Aux.Logger.Rule.Hint.Reflect (fromSourcePos)
import Text.Megaparsec

getCurrentHint :: Parser Hint
getCurrentHint =
  fromSourcePos <$> getSourcePos

getCurrentLoc :: Parser Loc
getCurrentLoc = do
  pos <- getSourcePos
  let line = unPos $ sourceLine pos
  let column = unPos $ sourceColumn pos
  return (line, column)
