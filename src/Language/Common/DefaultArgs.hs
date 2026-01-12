module Language.Common.DefaultArgs
  ( DefaultArgs (..),
    extract,
    traverseDefaultArgs,
  )
where

import Data.Text qualified as T

newtype DefaultArgs a
  = ByKey [(T.Text, a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

extract :: DefaultArgs a -> [a]
extract args =
  case args of
    ByKey xs ->
      map snd xs

traverseDefaultArgs :: (Applicative f) => (a -> f b) -> DefaultArgs a -> f (DefaultArgs b)
traverseDefaultArgs f args =
  case args of
    ByKey xs ->
      ByKey <$> traverse (\(k, v) -> (,) k <$> f v) xs
