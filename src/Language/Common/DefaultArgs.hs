module Language.Common.DefaultArgs
  ( DefaultArgs (..),
    extract,
    toMaybeList,
    traverseDefaultArgs,
  )
where

import Data.Maybe (catMaybes)
import Data.Text qualified as T

data DefaultArgs a
  = ByKey [(T.Text, a)]
  | Aligned [Maybe a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

extract :: DefaultArgs a -> [a]
extract args =
  case args of
    ByKey xs ->
      map snd xs
    Aligned xs ->
      catMaybes xs

toMaybeList :: DefaultArgs a -> [Maybe a]
toMaybeList args =
  case args of
    ByKey xs ->
      map (Just . snd) xs
    Aligned xs ->
      xs

traverseDefaultArgs :: (Applicative f) => (a -> f b) -> DefaultArgs a -> f (DefaultArgs b)
traverseDefaultArgs f args =
  case args of
    ByKey xs ->
      ByKey <$> traverse (\(k, v) -> (,) k <$> f v) xs
    Aligned xs ->
      Aligned <$> traverse (traverse f) xs
