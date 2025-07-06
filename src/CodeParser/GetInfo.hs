module CodeParser.GetInfo
  ( getCurrentHint,
    getCurrentLoc,
  )
where

import CodeParser.Parser
import Logger.Hint
import Logger.Hint.Reflect (fromSourcePos)
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
