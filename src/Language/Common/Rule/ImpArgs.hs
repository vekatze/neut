module Language.Common.Rule.ImpArgs
  ( ImpArgs (..),
    extract,
    traverseImpArgs,
  )
where

data ImpArgs a
  = Unspecified
  | FullySpecified [a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

extract :: ImpArgs a -> [a]
extract args =
  case args of
    Unspecified ->
      []
    FullySpecified xs ->
      xs

traverseImpArgs :: (Applicative f) => (a -> f b) -> ImpArgs a -> f (ImpArgs b)
traverseImpArgs f args =
  case args of
    Unspecified ->
      pure Unspecified
    FullySpecified xs ->
      FullySpecified <$> traverse f xs
