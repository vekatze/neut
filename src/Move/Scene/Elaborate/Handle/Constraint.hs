module Move.Scene.Elaborate.Handle.Constraint
  ( Handle,
    new,
    get,
    set,
    insert,
    insertActualityConstraint,
    insertIntegerConstraint,
  )
where

import Data.IORef
import Rule.Constraint qualified as C
import Rule.WeakTerm qualified as WT

newtype Handle = Handle
  { constraintEnvRef :: IORef [C.Constraint]
  }

new :: IO Handle
new = do
  constraintEnvRef <- newIORef []
  return $ Handle {..}

insert :: Handle -> WT.WeakTerm -> WT.WeakTerm -> IO ()
insert h expected actual = do
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
