module Library.CommandParser.Rule.Config.Remark
  ( Config (..),
    lspConfig,
  )
where

import Data.Text qualified as T

data Config = Config
  { shouldColorize :: Bool,
    enableDebugMode :: Bool,
    enableSilentMode :: Bool,
    endOfEntry :: T.Text
  }

lspConfig :: Config
lspConfig =
  Config
    { shouldColorize = False,
      enableDebugMode = False,
      enableSilentMode = False,
      endOfEntry = ""
    }
