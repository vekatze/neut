module Language.RawTerm.Name.Error
  ( missingDefinition,
    unexpectedEnd,
    unexpectedSeparator,
  )
where

import App.Error (Error, newError)
import Data.Text qualified as T
import Logger.Hint qualified as H

missingDefinition :: H.Hint -> T.Text -> Error
missingDefinition m nameText = do
  newError m $ "Name reference `" <> nameText <> "` does not contain a definition"

unexpectedEnd :: H.Hint -> T.Text -> T.Text -> Error
unexpectedEnd m nameText expected = do
  if T.null nameText
    then newError m $ "Unexpected end of a name; expected " <> expected
    else newError m $ "Unexpected end of `" <> nameText <> "`; expected " <> expected

unexpectedSeparator :: H.Hint -> T.Text -> T.Text -> T.Text -> Error
unexpectedSeparator m nameText separator expected = do
  newError m $ "Unexpected `" <> separator <> "` in `" <> nameText <> "`; expected " <> expected
