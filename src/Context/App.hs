module Context.App
  ( Axis (..),
    (&),
  )
where

import qualified Context.Log as Log
import qualified Context.Throw as Throw

data Axis = Axis
  { log :: Log.Context,
    throw :: Throw.Context
  }

infixl 1 &

(&) :: a -> (a -> b) -> b
x & f = f x
