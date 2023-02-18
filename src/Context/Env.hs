module Context.Env where

import Context.App
import Context.App.Internal
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Entity.AliasInfo
import Entity.Const
import Entity.Source qualified as Source
import Entity.TargetPlatform
import Path
import System.Environment
import System.Info qualified as SI

getNopFreeSet :: App (S.Set Int)
getNopFreeSet =
  readRef' nopFreeSet

setNopFreeSet :: S.Set Int -> App ()
setNopFreeSet =
  writeRef' nopFreeSet

getTargetPlatform :: App TargetPlatform
getTargetPlatform =
  readRef "targetPlatform" targetPlatform

setTargetPlatform :: App ()
setTargetPlatform = do
  mTargetArch <- liftIO $ lookupEnv envVarTargetArch
  mTargetOS <- liftIO $ lookupEnv envVarTargetOS
  let targetOS = fromMaybe SI.os mTargetOS
  let targetArch = fromMaybe SI.arch mTargetArch
  writeRef targetPlatform $ TargetPlatform {os = targetOS, arch = targetArch}

-- writeRef targetPlatform

insertToNopFreeSet :: Int -> App ()
insertToNopFreeSet i =
  modifyRef' nopFreeSet $ S.insert i

getShouldCancelAlloc :: App Bool
getShouldCancelAlloc =
  readRef' shouldCancelAlloc

setShouldCancelAlloc :: Bool -> App ()
setShouldCancelAlloc =
  writeRef' shouldCancelAlloc

setCurrentSource :: Source.Source -> App ()
setCurrentSource =
  writeRef currentSource

getCurrentSource :: App Source.Source
getCurrentSource =
  readRef "currentSource" currentSource

type PathSet = S.Set (Path Abs File)

getHasCacheSet :: App PathSet
getHasCacheSet =
  readRef' hasCacheSet

getHasObjectSet :: App PathSet
getHasObjectSet =
  readRef' hasObjectSet

getHasLLVMSet :: App PathSet
getHasLLVMSet =
  readRef' hasLLVMSet

insertToHasObjectSet :: Path Abs File -> App ()
insertToHasObjectSet v =
  modifyRef' hasObjectSet $ S.insert v

insertToHasCacheSet :: Path Abs File -> App ()
insertToHasCacheSet v =
  modifyRef' hasCacheSet $ S.insert v

insertToHasLLVMSet :: Path Abs File -> App ()
insertToHasLLVMSet v =
  modifyRef' hasLLVMSet $ S.insert v

getSourceAliasMap :: App SourceAliasMap
getSourceAliasMap =
  readRef' sourceAliasMap

insertToSourceAliasMap :: Path Abs File -> [AliasInfo] -> App ()
insertToSourceAliasMap k v =
  modifyRef' sourceAliasMap $ Map.insert k v

-- import Data.HashMap.Strict qualified as Map
-- import Data.Set qualified as S
-- import Data.Text qualified as T
-- import Entity.AliasInfo
-- import Entity.DefiniteDescription qualified as DD
-- import Entity.GlobalLocatorAlias qualified as GLA
-- import Entity.GlobalName qualified as GN
-- import Entity.HoleID qualified as HID
-- import Entity.HoleSubst qualified as HS
-- import Entity.Ident
-- import Entity.Module
-- import Entity.ModuleAlias
-- import Entity.ModuleChecksum
-- import Entity.Section qualified as Section
-- import Entity.Source qualified as Source
-- import Entity.Stmt
-- import Entity.StrictGlobalLocator qualified as SGL
-- import Entity.TargetPlatform
-- import Entity.VisitInfo
-- import Entity.WeakTerm
-- import Path

-- class MonadFail m => Context m where
--   setMainModule :: Module -> m ()
--   getMainModule :: m Module
--   setCurrentSource :: Source.Source -> m ()
--   getCurrentSource :: m Source.Source
--   setCurrentSectionStack :: [Section.Section] -> m ()
--   getCurrentSectionStack :: m [Section.Section]
--   setShouldCancelAlloc :: Bool -> m ()
--   getShouldCancelAlloc :: m Bool
--   setShouldColorize :: Bool -> m ()
--   getShouldColorize :: m Bool
--   setEndOfEntry :: T.Text -> m ()
--   getEndOfEntry :: m T.Text
--   setClangOptString :: String -> m ()
--   getClangOptString :: m String
--   getModuleCacheMap :: m (Map.HashMap (Path Abs File) Module)
--   insertToModuleCacheMap :: Path Abs File -> Module -> m ()
--   insertToLocatorAliasMap :: GLA.GlobalLocatorAlias -> SGL.StrictGlobalLocator -> m ()
--   getLocatorAliasMap :: m (Map.HashMap GLA.GlobalLocatorAlias SGL.StrictGlobalLocator)
--   setModuleAliasMap :: Map.HashMap ModuleAlias ModuleChecksum -> m ()
--   insertToModuleAliasMap :: ModuleAlias -> ModuleChecksum -> m ()
--   getModuleAliasMap :: m (Map.HashMap ModuleAlias ModuleChecksum)
--   setNameMap :: Map.HashMap DD.DefiniteDescription GN.GlobalName -> m ()
--   insertToNameMap :: DD.DefiniteDescription -> GN.GlobalName -> m ()
--   getNameMap :: m (Map.HashMap DD.DefiniteDescription GN.GlobalName)
--   setNopFreeSet :: S.Set Int -> m ()
--   getNopFreeSet :: m (S.Set Int)
--   insertToNopFreeSet :: Int -> m ()
--   getSourceAliasMap :: m SourceAliasMap
--   insertToSourceAliasMap :: Path Abs File -> [AliasInfo] -> m ()
--   setConstraintEnv :: [(WeakTerm, WeakTerm)] -> m ()
--   insConstraintEnv :: WeakTerm -> WeakTerm -> m ()
--   getConstraintEnv :: m [(WeakTerm, WeakTerm)]
--   setHoleSubst :: HS.HoleSubst -> m ()
--   insertSubst :: HID.HoleID -> [Ident] -> WeakTerm -> m ()
--   getHoleSubst :: m HS.HoleSubst
--   setTargetPlatform :: m ()
--   getTargetPlatform :: m TargetPlatform
--   insertToSourceChildrenMap :: Path Abs File -> [Source.Source] -> m ()
--   getSourceChildrenMap :: m (Map.HashMap (Path Abs File) [Source.Source])
--   setTraceSourceList :: [Source.Source] -> m ()
--   pushToTraceSourceList :: Source.Source -> m ()
--   popFromTraceSourceList :: m ()
--   getTraceSourceList :: m [Source.Source]
--   getHasObjectSet :: m PathSet
--   getHasLLVMSet :: m PathSet
--   insertToHasObjectSet :: Path Abs File -> m ()
--   insertToHasCacheSet :: Path Abs File -> m ()
--   insertToHasLLVMSet :: Path Abs File -> m ()
--   getHasCacheSet :: m PathSet
--   insertToVisitEnv :: Path Abs File -> VisitInfo -> m ()
--   getVisitEnv :: m (Map.HashMap (Path Abs File) VisitInfo)
