module Language.Common.ImpArgs
  ( ImpArgs (..),
  )
where

data ImpArgs a
  = Unspecified
  | FullySpecified [a]
  deriving (Eq, Show, Functor, Foldable, Traversable)
