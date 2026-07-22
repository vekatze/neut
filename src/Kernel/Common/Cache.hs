module Kernel.Common.Cache
  ( Cache (..),
    LowCache (..),
    CompletionCache (..),
    LocationCache (..),
    compress,
    extend,
  )
where

import Control.Comonad.Cofree
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Binary
import Data.IntSet qualified as IntSet
import GHC.Generics
import Kernel.Common.LocalVarTree qualified as LVT
import Kernel.Common.LocationTree qualified as LT
import Kernel.Common.RawImportSummary
import Kernel.Common.TopCandidate (TopCandidate)
import Language.Common.Binder (BinderF)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Opacity qualified as O
import Language.Common.StmtKind qualified as SK
import Language.Term.Compress qualified as TM
import Language.Term.Extend qualified as TM
import Language.Term.Stmt qualified as Stmt
import Language.Term.Term qualified as Term
import Language.Term.Trace qualified as Trace
import Logger.Log

data Cache = Cache
  { stmtList :: [Stmt.Stmt],
    knownTraceSiteIDs :: Maybe [IntSet.IntSet],
    remarkList :: [Log],
    globalReferenceList :: [DD.DefiniteDescription],
    countSnapshot :: Int
  }
  deriving (Generic)

data LowCache = LowCache
  { stmtList' :: [Stmt.StrippedStmt],
    remarkList' :: [Log],
    globalReferenceList' :: [DD.DefiniteDescription],
    countSnapshot' :: Int,
    termTraceSnapshot' :: Trace.Snapshot
  }
  deriving (Generic)

instance Binary LowCache

data CompletionCache = CompletionCache
  { localVarTree :: LVT.LocalVarTree,
    topCandidate :: [TopCandidate],
    rawImportSummary :: Maybe RawImportSummary
  }
  deriving (Generic)

instance Binary CompletionCache

newtype LocationCache = LocationCache
  { locationTree :: LT.LocationTree
  }
  deriving (Generic)

instance Binary LocationCache

compress :: Trace.Handle -> Cache -> IO LowCache
compress traceHandle cache = do
  let (results, traceIDs) = runState (mapM compressStmt $ stmtList cache) IntSet.empty
  let (stmtList', siteGroups) = unzip results
  termTraceSnapshot' <- Trace.snapshot traceHandle traceIDs siteGroups
  return LowCache
    { stmtList',
      remarkList' = remarkList cache,
      globalReferenceList' = globalReferenceList cache,
      countSnapshot' = countSnapshot cache,
      termTraceSnapshot'
    }

extend :: Trace.Handle -> LowCache -> IO Cache
extend traceHandle cache = do
  (remapping, siteGroups) <- Trace.restore traceHandle $ termTraceSnapshot' cache
  let stmtList = map (extendStmt remapping) (stmtList' cache)
  return Cache
    { stmtList,
      knownTraceSiteIDs = Just siteGroups,
      remarkList = remarkList' cache,
      globalReferenceList = globalReferenceList' cache,
      countSnapshot = countSnapshot' cache
    }

compressStmt :: Stmt.Stmt -> State IntSet.IntSet (Stmt.StrippedStmt, IntSet.IntSet)
compressStmt stmt = do
  case stmt of
    Stmt.StmtDefine isConstLike stmtKind m functionName impArgs expArgs defaultArgs codType e -> do
      let stmtKind' = TM.compressStmtKindTerm stmtKind
      let impArgs' = map TM.compressBinder impArgs
      defaultArgs' <- mapM compressDefaultArg defaultArgs
      let expArgs' = map TM.compressBinder expArgs
      let codType' = TM.compressType codType
      let bodyCompression =
            if O.isOpaque $ SK.toOpacityTerm stmtKind
              then TM.compressDiscardingTrace e
              else TM.compressCollect e
      let (e', bodySiteIDs) = runState bodyCompression IntSet.empty
      modify' $ IntSet.union bodySiteIDs
      return (Stmt.StmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' defaultArgs' codType' e', bodySiteIDs)
    Stmt.StmtDefineType isConstLike stmtKind m functionName impArgs expArgs defaultArgs codType body -> do
      let stmtKind' = TM.compressStmtKindType stmtKind
      let impArgs' = map TM.compressBinder impArgs
      defaultArgs' <- mapM compressDefaultArg defaultArgs
      let expArgs' = map TM.compressBinder expArgs
      let codType' = TM.compressType codType
      let body' = TM.compressType body
      return (Stmt.StmtDefineType isConstLike stmtKind' m functionName impArgs' expArgs' defaultArgs' codType' body', IntSet.empty)
    Stmt.StmtDefineResource m name resourceID unitType discarder copier resourceSize -> do
      let unitType' = TM.compressType unitType
      discarder' <- TM.compressCollect discarder
      copier' <- TM.compressCollect copier
      resourceSize' <- TM.compressCollect resourceSize
      return (Stmt.StmtDefineResource m name resourceID unitType' discarder' copier' resourceSize', IntSet.empty)
    Stmt.StmtTrope m name defineMetaList -> do
      defineMetaList' <- mapM compressDefineMeta defineMetaList
      return (Stmt.StmtTrope m name defineMetaList', IntSet.empty)
    Stmt.StmtVariadic kind m name -> do
      return (Stmt.StmtVariadic kind m name, IntSet.empty)
    Stmt.StmtForeign foreignList ->
      return (Stmt.StmtForeign foreignList, IntSet.empty)
    Stmt.StmtNamespace m name ->
      return (Stmt.StmtNamespace m name, IntSet.empty)

compressDefaultArg :: (BinderF Term.Type, Term.Term) -> State IntSet.IntSet (BinderF (Cofree Term.TypeF ()), Cofree Term.TermF ())
compressDefaultArg (binder, value) = do
  value' <- TM.compressCollect value
  return (TM.compressBinder binder, value')

extendStmt :: Trace.Remapping -> Stmt.StrippedStmt -> Stmt.Stmt
extendStmt remapping stmt =
  case stmt of
    Stmt.StmtDefine isConstLike stmtKind m functionName impArgs expArgs defaultArgs codType e -> do
      let stmtKind' = TM.extendStmtKindTerm stmtKind
      let impArgs' = map TM.extendBinder impArgs
      let defaultArgs' = map (bimap TM.extendBinder (TM.extend remapping)) defaultArgs
      let expArgs' = map TM.extendBinder expArgs
      let codType' = TM.extendType codType
      let e' = TM.extend remapping e
      Stmt.StmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' defaultArgs' codType' e'
    Stmt.StmtDefineType isConstLike stmtKind m functionName impArgs expArgs defaultArgs codType body -> do
      let stmtKind' = TM.extendStmtKindType stmtKind
      let impArgs' = map TM.extendBinder impArgs
      let defaultArgs' = map (bimap TM.extendBinder (TM.extend remapping)) defaultArgs
      let expArgs' = map TM.extendBinder expArgs
      let codType' = TM.extendType codType
      let body' = TM.extendType body
      Stmt.StmtDefineType isConstLike stmtKind' m functionName impArgs' expArgs' defaultArgs' codType' body'
    Stmt.StmtDefineResource m name resourceID unitType discarder copier resourceSize -> do
      let unitType' = TM.extendType unitType
      let discarder' = TM.extend remapping discarder
      let copier' = TM.extend remapping copier
      let resourceSize' = TM.extend remapping resourceSize
      Stmt.StmtDefineResource m name resourceID unitType' discarder' copier' resourceSize'
    Stmt.StmtTrope m name defineMetaList -> do
      Stmt.StmtTrope m name $ map (extendDefineMeta remapping) defineMetaList
    Stmt.StmtVariadic kind m name -> do
      Stmt.StmtVariadic kind m name
    Stmt.StmtForeign foreignList ->
      Stmt.StmtForeign foreignList
    Stmt.StmtNamespace m name ->
      Stmt.StmtNamespace m name

compressDefineMeta :: Stmt.DefineMetaF Term.Type Term.Term -> State IntSet.IntSet (Stmt.DefineMetaF (Cofree Term.TypeF ()) (Cofree Term.TermF ()))
compressDefineMeta defineMeta = do
  body' <- TM.compressCollect $ Stmt.defineMetaBody defineMeta
  return $
    defineMeta
      { Stmt.defineMetaTargetArgs = map TM.compressType $ Stmt.defineMetaTargetArgs defineMeta,
        Stmt.defineMetaExpArgs = map TM.compressBinder $ Stmt.defineMetaExpArgs defineMeta,
        Stmt.defineMetaCodType = TM.compressType $ Stmt.defineMetaCodType defineMeta,
        Stmt.defineMetaBody = body'
      }

extendDefineMeta :: Trace.Remapping -> Stmt.DefineMetaF (Cofree Term.TypeF ()) (Cofree Term.TermF ()) -> Stmt.DefineMetaF Term.Type Term.Term
extendDefineMeta remapping defineMeta =
  defineMeta
    { Stmt.defineMetaTargetArgs = map TM.extendType $ Stmt.defineMetaTargetArgs defineMeta,
      Stmt.defineMetaExpArgs = map TM.extendBinder $ Stmt.defineMetaExpArgs defineMeta,
      Stmt.defineMetaCodType = TM.extendType $ Stmt.defineMetaCodType defineMeta,
      Stmt.defineMetaBody = TM.extend remapping $ Stmt.defineMetaBody defineMeta
    }
