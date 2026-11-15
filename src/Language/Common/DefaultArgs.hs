module Language.Common.DefaultArgs
  ( DefaultArgs (..),
    toMaybeList,
  )
where

import Data.Text qualified as T

data DefaultArgs a
  = ByKey [(T.Text, a)]
  | Aligned [Maybe a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

toMaybeList :: DefaultArgs a -> [Maybe a]
toMaybeList args =
  case args of
    ByKey xs ->
      map (Just . snd) xs
    Aligned xs ->
      xs
