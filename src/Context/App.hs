module Context.App
  ( Axis (..),
  )
where

import qualified Context.Alias as Alias
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.LLVM as LLVM
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import Entity.Source
import Entity.Target

data Axis = Axis
  { log :: Log.Context,
    throw :: Throw.Context,
    gensym :: Gensym.Axis,
    llvm :: LLVM.Axis,
    global :: Global.Axis,
    locator :: Locator.Axis,
    alias :: Alias.Context,
    shouldCancelAlloc :: Bool,
    initialSource :: Source,
    target :: Target
  }
