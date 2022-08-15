module Context.External where

class Monad m => Context m where
  run :: String -> [String] -> m ()
