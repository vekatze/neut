module Move.Context.App
  ( App,
    runApp,
  )
where

import Control.Monad.Reader
import Move.Context.App.Internal

type App = ReaderT Env IO

runApp :: App a -> IO a
runApp app = do
  newEnv >>= runReaderT app
