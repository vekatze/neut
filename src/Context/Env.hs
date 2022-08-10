module Context.Env where

import qualified Context.Throw as Throw
import qualified Data.Set as S
import Entity.AliasInfo
import qualified Entity.HoleID as HID
import qualified Entity.HoleSubst as HS
import Entity.Ident
import Entity.Stmt
import Entity.WeakTerm

class Throw.Context m => Context m where
  setShouldCancelAlloc :: Bool -> m ()
  getShouldCancelAlloc :: m Bool
  getNopFreeSet :: m (S.Set Int)
  insertToNopFreeSet :: Int -> m ()
  getSourceAliasMap :: m SourceAliasMap
  getHasCacheSet :: m PathSet
  insConstraintEnv :: WeakTerm -> WeakTerm -> m ()
  getConstraintEnv :: m [(WeakTerm, WeakTerm)]
  setHoleSubst :: HS.HoleSubst -> m ()
  insertSubst :: HID.HoleID -> [Ident] -> WeakTerm -> m ()
  getHoleSubst :: m HS.HoleSubst
