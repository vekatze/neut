module Error.Rule.Error
  ( Error (..),
    join,
  )
where

import Logger.Rule.Log

newtype Error
  = MakeError [Log]
  deriving (Show, Semigroup)

join :: [Error] -> Error
join es = do
  MakeError $ concatMap (\(MakeError rs) -> rs) es
