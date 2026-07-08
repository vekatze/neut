module CommandParser.Config.Remark
  ( Config (..),
    lspConfig,
  )
where

import Console.ReportMode

data Config = Config
  { shouldColorize :: Bool,
    reportMode :: ReportMode
  }

lspConfig :: Config
lspConfig =
  Config
    { shouldColorize = False,
      reportMode = NoReport
    }
