module Aux.CommandParser.Rule.Config.Check (Config (..)) where

newtype Config = Config
  { shouldCheckAllDependencies :: Bool
  }
