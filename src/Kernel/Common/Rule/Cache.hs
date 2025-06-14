module Kernel.Common.Rule.Cache
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
import Kernel.Common.Rule.LocalVarTree qualified as LVT
import Kernel.Common.Rule.LocationTree qualified as LT
import Kernel.Common.Rule.RawImportSummary
import Kernel.Common.Rule.TopCandidate (TopCandidate)
import Language.Term.Rule.Stmt qualified as Stmt
import Language.Term.Rule.Term.Compress qualified as TM
import Language.Term.Rule.Term.Extend qualified as TM
import Logger.Rule.Log

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
    Stmt.StmtDefine isConstLike stmtKind m functionName impArgs expArgs codType e -> do
      let stmtKind' = TM.compressStmtKind stmtKind
      let impArgs' = map (bimap TM.compressBinder (fmap TM.compress)) impArgs
      let expArgs' = map TM.compressBinder expArgs
      let codType' = TM.compress codType
      let e' = TM.compress e
      Stmt.StmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' codType' e'
    Stmt.StmtForeign foreignList ->
      Stmt.StmtForeign foreignList

extendStmt :: Stmt.StrippedStmt -> Stmt.Stmt
extendStmt stmt =
  case stmt of
    Stmt.StmtDefine isConstLike stmtKind m functionName impArgs expArgs codType e -> do
      let stmtKind' = TM.extendStmtKind stmtKind
      let impArgs' = map (bimap TM.extendBinder (fmap TM.extend)) impArgs
      let expArgs' = map TM.extendBinder expArgs
      let codType' = TM.extend codType
      let e' = TM.extend e
      Stmt.StmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' codType' e'
    Stmt.StmtForeign foreignList ->
      Stmt.StmtForeign foreignList
