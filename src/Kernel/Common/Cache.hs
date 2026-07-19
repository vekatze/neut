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
import Data.Bifunctor
import Data.Binary
import GHC.Generics
import Kernel.Common.LocalVarTree qualified as LVT
import Kernel.Common.LocationTree qualified as LT
import Kernel.Common.RawImportSummary
import Kernel.Common.TopCandidate (TopCandidate)
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.Compress qualified as TM
import Language.Term.Extend qualified as TM
import Language.Term.Stmt qualified as Stmt
import Language.Term.Term qualified as Term
import Logger.Log

data Cache = Cache
  { stmtList :: [Stmt.Stmt],
    remarkList :: [Log],
    globalReferenceList :: [DD.DefiniteDescription],
    countSnapshot :: Int
  }
  deriving (Generic)

data LowCache = LowCache
  { stmtList' :: [Stmt.StrippedStmt],
    remarkList' :: [Log],
    globalReferenceList' :: [DD.DefiniteDescription],
    countSnapshot' :: Int
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

compress :: Cache -> LowCache
compress cache =
  LowCache
    { stmtList' = map compressStmt (stmtList cache),
      remarkList' = remarkList cache,
      globalReferenceList' = globalReferenceList cache,
      countSnapshot' = countSnapshot cache
    }

extend :: LowCache -> Cache
extend cache =
  Cache
    { stmtList = map extendStmt (stmtList' cache),
      remarkList = remarkList' cache,
      globalReferenceList = globalReferenceList' cache,
      countSnapshot = countSnapshot' cache
    }

compressStmt :: Stmt.Stmt -> Stmt.StrippedStmt
compressStmt stmt =
  case stmt of
    Stmt.StmtDefine isConstLike stmtKind m functionName impArgs expArgs defaultArgs codType e -> do
      let stmtKind' = TM.compressStmtKindTerm stmtKind
      let impArgs' = map TM.compressBinder impArgs
      let defaultArgs' = map (bimap TM.compressBinder TM.compress) defaultArgs
      let expArgs' = map TM.compressBinder expArgs
      let codType' = TM.compressType codType
      let e' = TM.compress e
      Stmt.StmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' defaultArgs' codType' e'
    Stmt.StmtDefineType isConstLike stmtKind m functionName impArgs expArgs defaultArgs codType body -> do
      let stmtKind' = TM.compressStmtKindType stmtKind
      let impArgs' = map TM.compressBinder impArgs
      let defaultArgs' = map (bimap TM.compressBinder TM.compress) defaultArgs
      let expArgs' = map TM.compressBinder expArgs
      let codType' = TM.compressType codType
      let body' = TM.compressType body
      Stmt.StmtDefineType isConstLike stmtKind' m functionName impArgs' expArgs' defaultArgs' codType' body'
    Stmt.StmtDefineResource m name resourceID unitType discarder copier resourceSize -> do
      let unitType' = TM.compressType unitType
      let discarder' = TM.compress discarder
      let copier' = TM.compress copier
      let resourceSize' = TM.compress resourceSize
      Stmt.StmtDefineResource m name resourceID unitType' discarder' copier' resourceSize'
    Stmt.StmtTrope m name defineMetaList -> do
      Stmt.StmtTrope m name $ map compressDefineMeta defineMetaList
    Stmt.StmtVariadic kind m name -> do
      Stmt.StmtVariadic kind m name
    Stmt.StmtForeign foreignList ->
      Stmt.StmtForeign foreignList
    Stmt.StmtNamespace m name ->
      Stmt.StmtNamespace m name

extendStmt :: Stmt.StrippedStmt -> Stmt.Stmt
extendStmt stmt =
  case stmt of
    Stmt.StmtDefine isConstLike stmtKind m functionName impArgs expArgs defaultArgs codType e -> do
      let stmtKind' = TM.extendStmtKindTerm stmtKind
      let impArgs' = map TM.extendBinder impArgs
      let defaultArgs' = map (bimap TM.extendBinder TM.extend) defaultArgs
      let expArgs' = map TM.extendBinder expArgs
      let codType' = TM.extendType codType
      let e' = TM.extend e
      Stmt.StmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' defaultArgs' codType' e'
    Stmt.StmtDefineType isConstLike stmtKind m functionName impArgs expArgs defaultArgs codType body -> do
      let stmtKind' = TM.extendStmtKindType stmtKind
      let impArgs' = map TM.extendBinder impArgs
      let defaultArgs' = map (bimap TM.extendBinder TM.extend) defaultArgs
      let expArgs' = map TM.extendBinder expArgs
      let codType' = TM.extendType codType
      let body' = TM.extendType body
      Stmt.StmtDefineType isConstLike stmtKind' m functionName impArgs' expArgs' defaultArgs' codType' body'
    Stmt.StmtDefineResource m name resourceID unitType discarder copier resourceSize -> do
      let unitType' = TM.extendType unitType
      let discarder' = TM.extend discarder
      let copier' = TM.extend copier
      let resourceSize' = TM.extend resourceSize
      Stmt.StmtDefineResource m name resourceID unitType' discarder' copier' resourceSize'
    Stmt.StmtTrope m name defineMetaList -> do
      Stmt.StmtTrope m name $ map extendDefineMeta defineMetaList
    Stmt.StmtVariadic kind m name -> do
      Stmt.StmtVariadic kind m name
    Stmt.StmtForeign foreignList ->
      Stmt.StmtForeign foreignList
    Stmt.StmtNamespace m name ->
      Stmt.StmtNamespace m name

compressDefineMeta :: Stmt.DefineMetaF Term.Type Term.Term -> Stmt.DefineMetaF (Cofree Term.TypeF ()) (Cofree Term.TermF ())
compressDefineMeta defineMeta =
  defineMeta
    { Stmt.defineMetaTargetArgs = map TM.compressType $ Stmt.defineMetaTargetArgs defineMeta,
      Stmt.defineMetaExpArgs = map TM.compressBinder $ Stmt.defineMetaExpArgs defineMeta,
      Stmt.defineMetaCodType = TM.compressType $ Stmt.defineMetaCodType defineMeta,
      Stmt.defineMetaBody = TM.compress $ Stmt.defineMetaBody defineMeta
    }

extendDefineMeta :: Stmt.DefineMetaF (Cofree Term.TypeF ()) (Cofree Term.TermF ()) -> Stmt.DefineMetaF Term.Type Term.Term
extendDefineMeta defineMeta =
  defineMeta
    { Stmt.defineMetaTargetArgs = map TM.extendType $ Stmt.defineMetaTargetArgs defineMeta,
      Stmt.defineMetaExpArgs = map TM.extendBinder $ Stmt.defineMetaExpArgs defineMeta,
      Stmt.defineMetaCodType = TM.extendType $ Stmt.defineMetaCodType defineMeta,
      Stmt.defineMetaBody = TM.extend $ Stmt.defineMetaBody defineMeta
    }
