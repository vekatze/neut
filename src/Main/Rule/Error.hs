module Main.Rule.Error
  ( Error (..),
    newError,
    newError',
    newCritical,
    newCritical',
    join,
  )
where

import Control.Exception
import Data.Text qualified as T
import Logger.Rule.Log
import Logger.Rule.LogLevel
import Main.Rule.Hint

newtype Error
  = MakeError [Log]
  deriving (Show, Semigroup)

instance Exception Error

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

join :: [Error] -> Error
join es = do
  MakeError $ concatMap (\(MakeError rs) -> rs) es
