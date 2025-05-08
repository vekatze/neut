module Library.Logger.Rule.Hint.Reify (toString) where

import Library.Logger.Rule.Hint

toString :: Hint -> String
toString m = do
  let name = metaFileName m
  let (l, c) = metaLocation m
  name ++ ":" ++ show l ++ ":" ++ show c
