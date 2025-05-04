module Main.Rule.Hint.Reify (toString) where

import Language.Common.Rule.Hint

toString :: Hint -> String
toString m = do
  let name = metaFileName m
  let (l, c) = metaLocation m
  name ++ ":" ++ show l ++ ":" ++ show c
