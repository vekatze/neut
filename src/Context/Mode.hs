module Context.Mode where

import qualified Context.Enum as Enum
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.LLVM as LLVM
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Throw as Throw

data Mode = Mode
  { logCtx :: Log.Config -> IO Log.Context,
    throwCtx :: Throw.Config -> IO Throw.Context,
    gensymCtx :: Gensym.Config -> IO Gensym.Axis,
    llvmCtx :: LLVM.Config -> IO LLVM.Axis,
    enumCtx :: Enum.Config -> IO Enum.Axis,
    globalCtx :: Global.Config -> IO Global.Axis,
    locatorCtx :: Locator.Config -> IO Locator.Axis
  }
