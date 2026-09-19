module CommandParser.Config.Remark
  ( Config (..),
  )
where

import Console.ReportMode

data Config = Config
  { shouldColorize :: Bool,
    reportMode :: Maybe ReportMode
  }
