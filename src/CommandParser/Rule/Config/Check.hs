module CommandParser.Rule.Config.Check (Config (..)) where

data Config = Config
  { shouldInsertPadding :: Bool,
    shouldCheckAllDependencies :: Bool
  }
