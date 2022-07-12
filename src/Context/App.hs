module Context.App
  ( Context (..),
    Config (..),
    new,
  )
where

import qualified Context.Alias as Alias
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.LLVM as LLVM
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Throw as Throw
import qualified Data.Set as S
import Entity.AliasInfo
import Entity.Module
import Entity.Source
import qualified Entity.TargetPlatform as TP
import Path
import qualified System.Info as System
import Prelude hiding (log)

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
    hasCacheSet :: S.Set (Path Abs File),
    targetPlatform :: TP.TargetPlatform
  }

data Config = Config
  { mode :: Mode.Mode,
    throwCtx :: Throw.Context,
    logCtx :: Log.Context,
    cancelAllocFlagConf :: Bool,
    mainModuleConf :: Module,
    initialSourceConf :: Source,
    sourceAliasMapConf :: SourceAliasMap,
    hasCacheSetConf :: S.Set (Path Abs File)
  }

new :: Config -> Source -> IO Context
new cfg source = do
  gensymCtx <- Mode.gensymCtx (mode cfg) $ Gensym.Config {}
  llvmCtx <-
    Mode.llvmCtx (mode cfg) $
      LLVM.Config
        { LLVM.throwCtx = throwCtx cfg,
          LLVM.clangOptString = "" -- fixme
        }
  globalCtx <-
    Mode.globalCtx (mode cfg) $
      Global.Config
        { Global.throwCtx = throwCtx cfg
        }
  locatorCtx <-
    Mode.locatorCtx (mode cfg) $
      Locator.Config
        { Locator.currentSource = source,
          Locator.mainModule = mainModuleConf cfg,
          Locator.throwCtx = throwCtx cfg
        }
  aliasCtx <-
    Mode.aliasCtx (mode cfg) $
      Alias.Config
        { Alias.currentModule = sourceModule source,
          Alias.mainModule = mainModuleConf cfg,
          Alias.throwCtx = throwCtx cfg,
          Alias.locatorCtx = locatorCtx
        }
  return $
    Context
      { log = logCtx cfg,
        throw = throwCtx cfg,
        gensym = gensymCtx,
        llvm = llvmCtx,
        global = globalCtx,
        locator = locatorCtx,
        alias = aliasCtx,
        shouldCancelAlloc = cancelAllocFlagConf cfg,
        initialSource = initialSourceConf cfg,
        targetPlatform =
          TP.TargetPlatform
            { TP.os = System.os,
              TP.arch = System.arch
            },
        sourceAliasMap = sourceAliasMapConf cfg,
        hasCacheSet = hasCacheSetConf cfg
      }
