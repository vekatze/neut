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
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import qualified Context.Type as Type
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
    moduleCtx :: Module.Context,
    asTypeCtx :: Type.Context,
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
    pathCtx :: Path.Context,
    globalCtx :: Global.Context,
    gensymCtx :: Gensym.Context,
    typeCtx :: Type.Context,
    cancelAllocFlagConf :: Bool,
    mainModuleConf :: Module,
    initialSourceConf :: Source,
    sourceAliasMapConf :: SourceAliasMap,
    hasCacheSetConf :: S.Set (Path Abs File)
  }

new :: Config -> Source -> IO Context
new cfg source = do
  _moduleCtx <-
    Mode.moduleCtx (mode cfg) $
      Module.Config
        { Module.mainModule = mainModuleConf cfg,
          Module.throwCtx = throwCtx cfg,
          Module.pathCtx = pathCtx cfg
        }
  -- gensymCtx <- Mode.gensymCtx (mode cfg) $ Gensym.Config {}
  llvmCtx <-
    Mode.llvmCtx (mode cfg) $
      LLVM.Config
        { LLVM.throwCtx = throwCtx cfg,
          LLVM.clangOptString = "" -- fixme
        }
  locatorCtx <-
    Mode.locatorCtx (mode cfg) $
      Locator.Config
        { Locator.currentSource = source,
          Locator.mainModule = mainModuleConf cfg,
          Locator.throwCtx = throwCtx cfg,
          Locator.pathCtx = pathCtx cfg,
          Locator.moduleCtx = _moduleCtx
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
        gensym = gensymCtx cfg,
        llvm = llvmCtx,
        global = globalCtx cfg,
        locator = locatorCtx,
        alias = aliasCtx,
        moduleCtx = _moduleCtx,
        asTypeCtx = typeCtx cfg,
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
