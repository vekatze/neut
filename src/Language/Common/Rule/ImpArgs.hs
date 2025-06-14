module Language.Common.Rule.ImpArgs
  ( ImpArgs (..),
    extract,
    traverseImpArgs,
  )
where

import Data.Maybe (catMaybes)

data ImpArgs a
  = Unspecified
  | FullySpecified [a]
  | PartiallySpecified [Maybe a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

extract :: ImpArgs a -> [a]
extract args =
  case args of
    Unspecified ->
      []
    FullySpecified xs ->
      xs
    PartiallySpecified xs ->
      catMaybes xs

traverseImpArgs :: (Applicative f) => (a -> f b) -> ImpArgs a -> f (ImpArgs b)
traverseImpArgs f args =
  case args of
    Unspecified ->
      pure Unspecified
    FullySpecified xs ->
      FullySpecified <$> traverse f xs
    PartiallySpecified xs ->
      PartiallySpecified <$> traverse (traverse f) xs
