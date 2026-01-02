module Language.Common.DefaultArgs
  ( DefaultArgs (..),
    extract,
    traverseDefaultArgs,
  )
where

import Data.Maybe (catMaybes)

data DefaultArgs a
  = Unspecified
  | FullySpecified [a]
  | PartiallySpecified [Maybe a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

extract :: DefaultArgs a -> [a]
extract args =
  case args of
    Unspecified ->
      []
    FullySpecified xs ->
      xs
    PartiallySpecified xs ->
      catMaybes xs

traverseDefaultArgs :: (Applicative f) => (a -> f b) -> DefaultArgs a -> f (DefaultArgs b)
traverseDefaultArgs f args =
  case args of
    Unspecified ->
      pure Unspecified
    FullySpecified xs ->
      FullySpecified <$> traverse f xs
    PartiallySpecified xs ->
      PartiallySpecified <$> traverse (traverse f) xs
