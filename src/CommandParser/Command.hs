module CommandParser.Command
  ( Command (..),
    InternalCommand (..),
    ExternalCommand (..),
  )
where

import CommandParser.Config.Archive qualified as Archive
import CommandParser.Config.Build qualified as Build
import CommandParser.Config.Check qualified as Check
import CommandParser.Config.Clean qualified as Clean
import CommandParser.Config.Create qualified as Create
import CommandParser.Config.FormatEns qualified as FormatEns
import CommandParser.Config.FormatSource qualified as FormatSource
import CommandParser.Config.Get qualified as Get
import CommandParser.Config.Remark qualified as Remark
import CommandParser.Config.Version qualified as Version
import CommandParser.Config.Zen qualified as Zen

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
