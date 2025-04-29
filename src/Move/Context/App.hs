module Move.Context.App
  ( App,
    runApp,
    runAppInEnv,
  )
where

import Control.Monad.Reader
import Move.Context.App.Internal

type App = ReaderT Env IO

runApp :: App a -> IO a
runApp app = do
  newEnv >>= runReaderT app

runAppInEnv :: Env -> App a -> IO a
runAppInEnv env app = do
  runReaderT app env
