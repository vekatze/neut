module Context.Cache
  ( Context (..),
  )
where

import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.Stmt

class Monad m => Context m where
  saveCache :: Program -> m ()
  loadCache :: Source.Source -> PathSet -> m (Maybe Cache)
  whenCompilationNecessary :: [OK.OutputKind] -> Source.Source -> m () -> m ()
