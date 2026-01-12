module Kernel.Common.Cache
  ( Cache (..),
    LowCache (..),
    CompletionCache (..),
    LocationCache (..),
    compress,
    extend,
  )
where

import Data.Bifunctor
import Data.Binary
import GHC.Generics
import Kernel.Common.LocalVarTree qualified as LVT
import Kernel.Common.LocationTree qualified as LT
import Kernel.Common.RawImportSummary
import Kernel.Common.TopCandidate (TopCandidate)
import Language.Term.Compress qualified as TM
import Language.Term.Extend qualified as TM
import Language.Term.Stmt qualified as Stmt
import Logger.Log

data Cache = Cache
  { stmtList :: [Stmt.Stmt],
    remarkList :: [Log],
    countSnapshot :: Int
  }
  deriving (Generic)

data LowCache = LowCache
  { stmtList' :: [Stmt.StrippedStmt],
    remarkList' :: [Log],
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
      countSnapshot' = countSnapshot cache
    }

extend :: LowCache -> Cache
extend cache =
  Cache
    { stmtList = map extendStmt (stmtList' cache),
      remarkList = remarkList' cache,
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
    Stmt.StmtVariadic kind m name -> do
      Stmt.StmtVariadic kind m name
    Stmt.StmtForeign foreignList ->
      Stmt.StmtForeign foreignList

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
    Stmt.StmtVariadic kind m name -> do
      Stmt.StmtVariadic kind m name
    Stmt.StmtForeign foreignList ->
      Stmt.StmtForeign foreignList
