module Rule.Error
  ( Error (..),
    newError,
    newError',
    newCritical,
    newCritical',
  )
where

import Control.Exception
import Data.Text qualified as T
import Rule.Hint
import Rule.Remark

newtype Error
  = MakeError [Remark]
  deriving (Show, Semigroup)

instance Exception Error

newError :: Hint -> T.Text -> Error
newError m text = do
  MakeError [newRemark m Error text]

newError' :: T.Text -> Error
newError' text = do
  MakeError [newRemark' Error text]

newCritical :: Hint -> T.Text -> Error
newCritical m text = do
  MakeError [newRemark m Critical text]

newCritical' :: T.Text -> Error
newCritical' text = do
  MakeError [newRemark' Critical text]
