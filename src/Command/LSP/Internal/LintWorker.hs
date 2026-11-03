module Command.LSP.Internal.LintWorker
  ( Handle,
    new,
    restart,
    markFresh,
    isAnswerable,
  )
where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Data.IORef
import Data.Set qualified as S
import Language.LSP.Protocol.Types (NormalizedUri)

data Freshness
  = Settled
  | Rechecking (S.Set NormalizedUri)

data Handle = Handle
  { runningRef :: MVar (Maybe (Async ())),
    freshnessRef :: IORef Freshness
  }

new :: IO Handle
new = do
  runningRef <- newMVar Nothing
  freshnessRef <- newIORef Settled
  return $ Handle {..}

restart :: Handle -> IO () -> IO ()
restart h action =
  modifyMVar_ (runningRef h) $ \running -> do
    mapM_ cancel running
    writeIORef (freshnessRef h) (Rechecking S.empty)
    Just <$> async (action `finally` writeIORef (freshnessRef h) Settled)

markFresh :: Handle -> NormalizedUri -> IO ()
markFresh h uri =
  atomicModifyIORef' (freshnessRef h) $ \freshness ->
    case freshness of
      Settled ->
        (Settled, ())
      Rechecking uris ->
        (Rechecking (S.insert uri uris), ())

isAnswerable :: Handle -> NormalizedUri -> IO Bool
isAnswerable h uri = do
  freshness <- readIORef (freshnessRef h)
  case freshness of
    Settled ->
      return True
    Rechecking uris ->
      return $ S.member uri uris
