module CommandParser.Config.Remark
  ( Config (..),
    lspConfig,
  )
where

data Config = Config
  { shouldColorize :: Bool,
    enableDebugMode :: Bool,
    enableSilentMode :: Bool
  }

lspConfig :: Config
lspConfig =
  Config
    { shouldColorize = False,
      enableDebugMode = False,
      enableSilentMode = False
    }
