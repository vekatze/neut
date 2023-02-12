module Context.OptParse where

import Entity.Command

class Monad m => Context m where
  parseCommand :: m Command
