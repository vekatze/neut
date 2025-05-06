module BasicParser.Move.GetInfo
  ( getCurrentHint,
    getCurrentLoc,
  )
where

import BasicParser.Rule.Parser
import Logger.Rule.Hint
import Logger.Rule.Hint.Reflect (fromSourcePos)
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
