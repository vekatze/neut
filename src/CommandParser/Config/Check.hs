module CommandParser.Config.Check (Config (..)) where

data Config = Config
  { shouldCheckAllDependencies :: Bool
  }
