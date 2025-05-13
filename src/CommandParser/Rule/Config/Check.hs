module CommandParser.Rule.Config.Check (Config (..)) where

newtype Config = Config
  { shouldCheckAllDependencies :: Bool
  }
