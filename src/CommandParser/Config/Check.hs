module CommandParser.Config.Check (Config (..)) where

newtype Config = Config
  { shouldCheckAllDependencies :: Bool
  }
