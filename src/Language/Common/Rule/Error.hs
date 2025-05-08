module Language.Common.Rule.Error
  ( newError,
    newError',
    newCritical,
    newCritical',
    join,
  )
where

import Data.Text qualified as T
import Library.Error.Rule.Error
import Library.Logger.Rule.Hint
import Library.Logger.Rule.Log
import Library.Logger.Rule.LogLevel

newError :: Hint -> T.Text -> Error
newError m text = do
  MakeError [newLog m Error text]

newError' :: T.Text -> Error
newError' text = do
  MakeError [Log Nothing True Error text]

newCritical :: Hint -> T.Text -> Error
newCritical m text = do
  MakeError [newLog m Critical text]

newCritical' :: T.Text -> Error
newCritical' text = do
  MakeError [Log Nothing True Critical text]
