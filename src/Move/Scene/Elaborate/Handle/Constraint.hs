module Move.Scene.Elaborate.Handle.Constraint
  ( Handle,
    new,
    get,
    set,
    insertEqualityConstraint,
    insertActualityConstraint,
    insertIntegerConstraint,
  )
where

import Control.Monad.Reader (asks)
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Constraint qualified as C
import Rule.WeakTerm qualified as WT

newtype Handle = Handle
  { constraintEnvRef :: IORef [C.Constraint]
  }

new :: App Handle
new = do
  constraintEnvRef <- asks App.constraintEnv
  return $ Handle {..}

insertEqualityConstraint :: Handle -> WT.WeakTerm -> WT.WeakTerm -> IO ()
insertEqualityConstraint h expected actual = do
  modifyIORef' (constraintEnvRef h) $ (:) (C.Eq expected actual)

insertActualityConstraint :: Handle -> WT.WeakTerm -> IO ()
insertActualityConstraint h t = do
  modifyIORef' (constraintEnvRef h) $ (:) (C.Actual t)

insertIntegerConstraint :: Handle -> WT.WeakTerm -> IO ()
insertIntegerConstraint h t = do
  modifyIORef' (constraintEnvRef h) $ (:) (C.Integer t)

get :: Handle -> IO [C.Constraint]
get h = do
  readIORef (constraintEnvRef h)

set :: Handle -> [C.Constraint] -> IO ()
set h cs = do
  writeIORef (constraintEnvRef h) cs
