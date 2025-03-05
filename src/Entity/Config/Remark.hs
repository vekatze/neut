module Entity.Config.Remark
  ( Config (..),
    lspConfig,
  )
where

import Data.Text qualified as T

data Config = Config
  { shouldColorize :: Bool,
    enableDebugMode :: Bool,
    endOfEntry :: T.Text
  }

lspConfig :: Config
lspConfig =
  Config
    { shouldColorize = False,
      enableDebugMode = False,
      endOfEntry = ""
    }
