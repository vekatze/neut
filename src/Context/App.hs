module Context.App
  ( Axis (..),
  )
where

import qualified Context.Enum as Enum
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.LLVM as LLVM
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import Entity.Source

data Axis = Axis
  { log :: Log.Context,
    throw :: Throw.Context,
    gensym :: Gensym.Axis,
    llvm :: LLVM.Axis,
    enum :: Enum.Axis,
    global :: Global.Axis,
    locator :: Locator.Axis,
    shouldCancelAlloc :: Bool,
    initialSource :: Source
  }
