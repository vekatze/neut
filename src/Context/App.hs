module Context.App
  ( Axis (..),
  )
where

import qualified Context.Gensym as Gensym
import qualified Context.Log as Log
import qualified Context.Throw as Throw

data Axis = Axis
  { log :: Log.Context,
    throw :: Throw.Context,
    gensym :: Gensym.Axis
  }
