module Context.Env where

import qualified Context.Throw as Throw
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as S
import Entity.AliasInfo
import qualified Entity.HoleID as HID
import qualified Entity.HoleSubst as HS
import Entity.Ident
import Entity.Module
import qualified Entity.Source as Source
import Entity.Stmt
import Entity.TargetPlatform
import Entity.VisitInfo
import Entity.WeakTerm
import Path

class Throw.Context m => Context m where
  setMainModule :: Module -> m ()
  getMainModule :: m Module
  setShouldCancelAlloc :: Bool -> m ()
  getShouldCancelAlloc :: m Bool
  setNopFreeSet :: S.Set Int -> m ()
  getNopFreeSet :: m (S.Set Int)
  insertToNopFreeSet :: Int -> m ()
  getSourceAliasMap :: m SourceAliasMap
  insertToSourceAliasMap :: Path Abs File -> [AliasInfo] -> m ()
  insConstraintEnv :: WeakTerm -> WeakTerm -> m ()
  getConstraintEnv :: m [(WeakTerm, WeakTerm)]
  setHoleSubst :: HS.HoleSubst -> m ()
  insertSubst :: HID.HoleID -> [Ident] -> WeakTerm -> m ()
  getHoleSubst :: m HS.HoleSubst
  setTargetPlatform :: TargetPlatform -> m ()
  getTargetPlatform :: m TargetPlatform
  getSourceChildrenMap :: m (Map.HashMap (Path Abs File) [Source.Source])
  insertToSourceChildrenMap :: Path Abs File -> [Source.Source] -> m ()
  getTraceSourceList :: m [Source.Source]
  pushToTraceSourceList :: Source.Source -> m ()
  popFromTraceSourceList :: m Source.Source
  getHasObjectSet :: m PathSet
  insertToHasObjectSet :: Path Abs File -> m ()
  getHasCacheSet :: m PathSet
  insertToHasCacheSet :: Path Abs File -> m ()
  getVisitEnv :: m (Map.HashMap (Path Abs File) VisitInfo)
  insertToVisitEnv :: Path Abs File -> VisitInfo -> m ()
