module Context.App
  ( Context (..),
  )
where

import qualified Context.Alias as Alias
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.LLVM as LLVM
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import Entity.AliasInfo
import Entity.Source
import Entity.Target

data Context = Context
  { log :: Log.Context,
    throw :: Throw.Context,
    gensym :: Gensym.Context,
    llvm :: LLVM.Context,
    global :: Global.Context,
    locator :: Locator.Context,
    alias :: Alias.Context,
    shouldCancelAlloc :: Bool,
    initialSource :: Source,
    sourceAliasMap :: SourceAliasMap,
    target :: Target
  }
