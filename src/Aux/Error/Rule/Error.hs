module Aux.Error.Rule.Error
  ( Error (..),
    join,
    newError,
    newError',
    newCritical,
    newCritical',
  )
where

import Aux.Logger.Rule.Hint
import Aux.Logger.Rule.Log
import Aux.Logger.Rule.LogLevel
import Data.Text qualified as T

newtype Error
  = MakeError [Log]
  deriving (Show, Semigroup)

join :: [Error] -> Error
join es = do
  MakeError $ concatMap (\(MakeError rs) -> rs) es

newError :: Hint -> T.Text -> Error
newError m text = do
  MakeError [newLog m Error text]

newError' :: T.Text -> Error
newError' text = do
  MakeError [Log Nothing Error text]

newCritical :: Hint -> T.Text -> Error
newCritical m text = do
  MakeError [newLog m Critical text]

newCritical' :: T.Text -> Error
newCritical' text = do
  MakeError [Log Nothing Critical text]
