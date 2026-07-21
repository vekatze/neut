module Command.Common.Dependency (run) where

import App.App (App)
import App.Error qualified as E
import App.Run (runApp)
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception (bracket_)
import Control.Monad (forM)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isRight, partitionEithers)
import Data.HashMap.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Kernel.Common.SourceDependencyMap (SourceDependencyMap)
import Path
import UnliftIO.Async (forConcurrently)

run :: Int -> SourceDependencyMap -> (a -> Path Abs File) -> [a] -> (a -> App b) -> App [b]
run concurrency dependencyMap getKey itemList f = do
  semaphore <- liftIO $ newQSem concurrency
  completionList <- liftIO $ forM itemList $ \item -> do
    completion <- newEmptyMVar
    return (getKey item, completion)
  let completionMap = Map.fromList completionList
  resultList <- liftIO $ forConcurrently itemList $ \item -> do
    let key = getKey item
    let dependencyKeyList = Map.lookupDefault [] key dependencyMap
    let dependencyCompletionList = mapMaybe (`Map.lookup` completionMap) dependencyKeyList
    dependencyResultList <- mapM readMVar dependencyCompletionList
    if and dependencyResultList
      then do
        result <- bracket_ (waitQSem semaphore) (signalQSem semaphore) $ runApp $ f item
        putMVar (completionMap Map.! key) (isRight result)
        return $ fmap Just result
      else do
        putMVar (completionMap Map.! key) False
        return $ Right Nothing
  let (errorList, valueList) = partitionEithers resultList
  if null errorList
    then return $ catMaybes valueList
    else throwError $ E.join errorList
