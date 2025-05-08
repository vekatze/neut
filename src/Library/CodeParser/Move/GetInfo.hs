module Library.CodeParser.Move.GetInfo
  ( getCurrentHint,
    getCurrentLoc,
  )
where

import Library.CodeParser.Rule.Parser
import Library.Logger.Rule.Hint
import Library.Logger.Rule.Hint.Reflect (fromSourcePos)
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
