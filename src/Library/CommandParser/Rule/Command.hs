module Library.CommandParser.Rule.Command
  ( Command (..),
    InternalCommand (..),
    ExternalCommand (..),
  )
where

import Library.CommandParser.Rule.Config.Archive qualified as Archive
import Library.CommandParser.Rule.Config.Build qualified as Build
import Library.CommandParser.Rule.Config.Check qualified as Check
import Library.CommandParser.Rule.Config.Clean qualified as Clean
import Library.CommandParser.Rule.Config.Create qualified as Create
import Library.CommandParser.Rule.Config.FormatEns qualified as FormatEns
import Library.CommandParser.Rule.Config.FormatSource qualified as FormatSource
import Library.CommandParser.Rule.Config.Get qualified as Get
import Library.CommandParser.Rule.Config.Remark qualified as Remark
import Library.CommandParser.Rule.Config.Version qualified as Version
import Library.CommandParser.Rule.Config.Zen qualified as Zen

data InternalCommand
  = Archive Archive.Config
  | Build Build.Config
  | Check Check.Config
  | Clean Clean.Config
  | FormatSource FormatSource.Config
  | FormatEns FormatEns.Config
  | Get Get.Config
  | LSP
  | Zen Zen.Config

data ExternalCommand
  = Create Create.Config
  | ShowVersion Version.Config

data Command
  = Internal Remark.Config InternalCommand
  | External Remark.Config ExternalCommand
